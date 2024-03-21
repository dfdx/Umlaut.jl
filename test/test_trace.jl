import Umlaut: Tape, V, Call, mkcall, play!, compile, Loop, __new__
import Umlaut: trace, isprimitive, record_primitive!, BaseCtx


non_primitive(x) = 2x + 1
non_primitive_caller(x) = sin(non_primitive(x))

struct MyCtx end

isprimitive(ctx::MyCtx, f, args...) = isprimitive(BaseCtx(), f, args...) || f == non_primitive


@testset "trace: primitives" begin
    # primitives
    x = 3.0
    val1, tape1 = trace(non_primitive_caller, x)
    val2, tape2 = trace(non_primitive_caller, x; ctx=BaseCtx([non_primitive, sin]))
    val3, tape3 = trace(non_primitive_caller, x; ctx=MyCtx())

    @test val1 == val2
    @test val1 == val3
    @test any(op isa Call && op.fn == (*) for op in tape1)
    @test tape2[V(3)].fn == non_primitive
    @test tape2[V(4)].fn == sin
    @test tape3[V(3)].fn == non_primitive
    @test tape3[V(4)].fn == sin
end


###############################################################################

inc_mul(a::Real, b::Real) = a * (b + 1.0)
inc_mul(A::AbstractArray, B::AbstractArray) = inc_mul.(A, B)
inc_mul2(A::AbstractArray, B::AbstractArray) = A .* (B .+ 1)


@testset "trace: calls" begin
    # calls
    val, tape = trace(inc_mul, 2.0, 3.0)
    @test val == inc_mul(2.0, 3.0)
    @test length(tape) == 5
    @test tape[V(5)].args[1].id == 2
end

###############################################################################

@eval _alt_getindex(x::Vector, i) = Base.arrayref($(Expr(:boundscheck)), x, i)
@eval function _boundscheck_foo()
    v = $(Expr(:boundscheck))
    return v ? 1 : 0
end

@testset "trace: :boundscheck" begin
    @testset "boundscheck as argument" begin
        x = randn(5)
        val, tape = trace(_alt_getindex, x, 1)
        @test val == getindex(x, 1)
        @test play!(tape, getindex, x, 1) == getindex(x, 1)
    end

    @testset "boundscheck as rhs statement" begin
        val, tape = trace(_boundscheck_foo)
        @test val == _boundscheck_foo()
        @test play!(tape, _boundscheck_foo) == val
    end
end



###############################################################################

# Test case courtesy of maleadt. See discourse for details:
# https://discourse.julialang.org/t/testing-gc-preserve-when-doing-compiler-passes/102241

struct GCCtx end

function isprimitive(::GCCtx, f, args...)
    typeof(f) === typeof(GC.gc) && return true
    return isprimitive(BaseCtx(), f, args...)
end

mutable struct Object
    finalized::Bool
    @noinline function Object()
        finalizer(new(false)) do obj
            obj.finalized = true
            return obj
        end
    end
end

function with_preservation()
    obj = Object()
    return GC.@preserve obj begin
        ptr = convert(Ptr{Bool}, Base.pointer_from_objref(obj))
        GC.gc(true)
        unsafe_load(ptr)
    end
end

function without_preservation()
    obj = Object()
    ptr = convert(Ptr{Bool}, Base.pointer_from_objref(obj))
    GC.gc(true)
    return unsafe_load(ptr)
end

@testset "trace: gc_preserve" begin
    @testset "with preservation" begin
        @test with_preservation() == false
        val, tape = trace(with_preservation; ctx=GCCtx())
        @test val == false
        @test play!(tape, with_preservation) == false
        @test compile(tape)(with_preservation) == false
    end

    # The tape never drops references to `obj`, so it never gets freed, regardless preserve.
    @testset "without preservation" begin
        @test without_preservation() == true
        val, tape = trace(without_preservation; ctx=GCCtx())
        @test val == false
        @test play!(tape, without_preservation) == false
        compiled_tape = Umlaut.compile(tape)
        @test compiled_tape(without_preservation) == false
    end
end


###############################################################################

# No static parameters.
function foreigncall_0(x::Array{Float64})
    return unsafe_load(ccall(:jl_array_ptr, Ptr{Float64}, (Any, ), x))
end

# Static parameters in nested types directly in ccall expression. See
# https://discourse.julialang.org/t/types-in-foreigncall-nodes/101830 for context.
function foreigncall_1(::Type{Ptr{T}}, x::Array{T}) where {T}
    return unsafe_load(ccall(:jl_array_ptr, Ptr{T}, (Any, ), x))
end

# Tuple-style foreigncall name specification.
function foreigncall_blas_0(n::Int, DA::Float64, DX::Array{Float64}, incx::Int)
    ccall(
        (BLAS.@blasfunc(dscal_), BLAS.libblastrampoline),
        Cvoid,
        (Ref{BLAS.BlasInt}, Ref{Float64}, Ptr{Float64}, Ref{BLAS.BlasInt}),
        n, DA, DX, incx,
    )
    return DX
end

# Must be called with T == Float64. Addresses the same limitation as foreigncall_1.
function foreigncall_blas_1(n::Int, DA::T, DX::Array{T}, incx::Int) where {T}
    ccall(
        (BLAS.@blasfunc(dscal_), BLAS.libblastrampoline),
        Cvoid,
        (Ref{BLAS.BlasInt}, Ref{T}, Ptr{T}, Ref{BLAS.BlasInt}),
        n, DA, DX, incx,
    )
    return DX
end

@static if VERSION >= v"1.9"
    @testset "trace: foreigncall" begin
        @testset "$f" for (f, args) in [
            (foreigncall_0, (randn(5), )),
            (foreigncall_1, (Ptr{Float64}, randn(5))),
            (foreigncall_blas_0, (5, 3.0, randn(11), 2)),
            (foreigncall_blas_1, (5, 3.0, randn(11), 2)),
        ]
            original_args = deepcopy(args)
            val, tape = trace(f, args...)
            @test val == f(deepcopy(original_args)...)
        end
    end
end

###############################################################################

@eval function foo(x)
    $(Expr(:loopinfo, Symbol("julia.simd"), nothing))
    return 5x
end

@testset "trace: :loopinfo" begin
    val, tape = trace(foo, 5.0)
    @test val == foo(5.0)
    @test play!(tape, foo, 5.0) == val
end

###############################################################################

@testset "trace: bcast" begin
    # bcast
    A = rand(3)
    B = rand(3)
    val, tape = trace(inc_mul, A, B)
    @test val == inc_mul(A, B)
    # broadcasting may be lowered to different forms,
    # so making no assumptions regarding the tape

    val, tape = trace(inc_mul2, A, B)
    @test val == inc_mul2(A, B)
end

###############################################################################


make_tuple(a, b) = (a, b)

function tuple_unpack(x)
    a, b = make_tuple(1, 2)
    x + a
end


function make_adder(x)
    return y -> x + y
end

function add(x, y)
    adder = make_adder(x)
    adder(y)
end


@testset "trace: closures, iterables" begin
    # tuple unpacking
    val, tape = trace(tuple_unpack, 4.0)
    @test val == tuple_unpack(4.0)

    # closures
    val, tape = trace(add, 2.0, 4.0)
    @test val == add(2.0, 4.0)
    @test play!(tape, add, 2.0, 5.0) == add(2.0, 5.0)
    @test play!(tape, add, 3.0, 5.0) == add(3.0, 5.0)

    # iterators
    xs = rand(3)
    f = xs -> [x + 1 for x in xs]
    val, tape = trace(f, xs)
    @test val == f(xs)
    xs2 = rand(5)
    @test play!(tape, nothing, xs2) == f(xs2)
end


###############################################################################

function constant_return_value(x)
    2x
    nothing
end

function no_input()
    print()
end


@testset "trace: no inputs/outputs" begin
    # constant return value
    _, tape = trace(constant_return_value, 1.0)
    @test play!(tape, nothing, 2.0) === nothing

    # no input
    _, tape = trace(no_input)
    @test tape[V(2)].fn == print
end


###############################################################################

function vararg_fn(x, xs...)
    return x + sum(xs)
end

multiarg_fn(x) = only(x)
multiarg_fn(x, y) = only(x) + only(y)
multiarg_fn(x, y, z) = only(x) + only(y) + only(z)

function vararg_fn_sparam(x, xs::Vararg{T,N}) where {T,N}
    T(N)
end

@testset "trace: varargs, splatting" begin

    @testset "__to_tuple__" begin
        @test Umlaut.__to_tuple__(5.0) == (5.0, )
        @test Umlaut.__to_tuple__((5.0, 4.0)) == (5.0, 4.0)
        @test Umlaut.__to_tuple__((a=5.0, b="hi")) == (5.0, "hi")
        @test Umlaut.__to_tuple__([5.0, 4.0, 3.0]) == (5.0, 4.0, 3.0)
        @test Umlaut.__to_tuple__(zip([1.0, 2.0, 3.0])) == ((1.0, ), (2.0, ), (3.0, ))
        @test Umlaut.__to_tuple__(Core.svec(1.0, 2.0)) == (1.0, 2.0)
    end

    # varargs
    _, tape = trace(vararg_fn, 1, 2, 3)
    @test play!(tape, vararg_fn, 4, 5, 6) == vararg_fn(4, 5, 6)
    @test compile(tape)(vararg_fn, 4, 5, 6) == vararg_fn(4, 5, 6)
    # in the latest version of Umlaut the number of arguments to the tape
    # must stay the same, so not sure what to do with these test
    # marking them as broken for now
    @test_broken play!(tape, vararg_fn, 4, 5, 6, 7) == vararg_fn(4, 5, 6, 7)
    @test_broken play!(tape, vararg_fn, 4, 5) == vararg_fn(4, 5)
    @test_broken compile(tape)(vararg_fn, 4, 5, 6, 7) == vararg_fn(4, 5, 6, 7)
    @test_broken compile(tape)(vararg_fn, 4, 5) == vararg_fn(4, 5)

    vararg_wrapper = xs -> vararg_fn(xs...)
    _, tape = trace(vararg_wrapper, (1, 2, 3))
    @test play!(tape, vararg_wrapper, (4, 5, 6)) == vararg_wrapper((4, 5, 6))

    res, tape = trace(vararg_fn_sparam, 1, 1.0, 1.0)
    @test res === 2.0
    @test play!(tape, vararg_fn_sparam, 1, 1.0, 1.0) == vararg_fn_sparam(1, 1.0, 1.0)
    @test compile(tape)(vararg_fn_sparam, 1, 1.0, 1.0) == vararg_fn_sparam(1, 1.0, 1.0)

    # tuple/vararg unpacking
    f = t -> multiarg_fn(t...)
    _, tape = trace(f, (1, 2))
    @test play!(tape, f, (3, 4)) == f((3, 4))
    @test tape[V(5)].fn == Base.getfield
    @test tape[V(6)].fn == Base.getfield

    @test_logs (:warn, "Variable %2 had length 2 during tracing, but now has length 3") play!(tape, f, (5, 6, 7))

    # tuple/vararg optimization
    f = x -> multiarg_fn((x, x, 1)...)
    _, tape = trace(f, 1)
    @test play!(tape, f, 2) == f(2)
    v2 = V(tape, 2)
    v6 = V(tape, 6)
    if VERSION >= v"1.9"
        @test (tape[V(end)].fn == +) && (tape[V(end)].args == [v2, v2, v6])
    end

    test_f = x -> multiarg_fn(x...)
    @testset "$name" for (name, x) in [
        ("splat single Int", 1),
        ("splat single Float64", 1.0),
        ("splat Vector{Float64}", [1.0, 2.0]),
        ("splat Tuple{Float64, Int}", (5.0, 4)),
        ("splat NamedTuple(Float64, Int)", (a=5.0, b=2)),
        ("splat zip", zip([1.0, 2.0, 3.0])),
        ("splat Core.SimpleVector", Core.svec(1.0, 2.)),
    ]
        @test test_f(x) === test_f(x)
        v, tape = trace(test_f, x)
        @test v == test_f(x)
    end
end


###############################################################################

mutable struct Point x; y end
constructor_loss(a) = (p = Point(a, a); p.x + p.y)

struct SplatNewTester
    x
    y
    SplatNewTester(x...) = new(x...)
end

@testset "trace: constructors" begin
    # isprimitive for constructors
    # MyType & MyTypeWithParams are defined in test_utils.jl
    @test isprimitive(BaseCtx(), Int, 1.0)
    @test isprimitive(BaseCtx(), MyType, 1.0) == false
    @test isprimitive(BaseCtx(), MyTypeWithParams, 1.0) == false

    # constructors
    _, tape = trace(constructor_loss, 4.0)
    @test tape[V(3)].val isa Point
    @test_broken tape[V(4)].fn == __new__  # test broken in v1.10

    # Exact code generated is version dependent -- either is fine.
    @test(
        (tape[V(3)].val == Point && tape[V(4)].fn == __new__) ||
        (tape[V(3)].fn == __new__ && tape[V(3)].args[1] == Point)
    )

    # constructor with splatnew
    # This test seems to be quite brittle, and to depend on the precise version of Julia
    # used. Might be good to refactor this in the future.
    # If this test fails for a new version of Julia, it might well not be an actual bug.
    _, tape = trace((x, y) -> SplatNewTester(x, y), 5.0, 4)
    if VERSION < v"1.9"
        tape[V(10)].fn == __new__
    else
        tape[V(7)].val == __new__
    end
end


###############################################################################

inc_val(::Val{N}) where N = N + 1

@testset "trace: static_params" begin
    # Expr(:static_parameter, n)
    val, tape = trace(inc_val, Val(2))
    @test val == inc_val(Val(2))
    # note: static parameter is recorded as constant
    # so changing value on tape has no effect
    @test play!(tape, inc_val, Val(42)) == inc_val(Val(2))
end

static_val(::Val{N}) where {N} = N

@testset "trace: returning :static_parameter" begin
    val, tape = trace(static_val, Val(2))
    @test val == static_val(Val(2))
    # note: static parameter is recorded as constant
    # so changing value on tape has no effect
    @test play!(tape, static_val, Val(42)) == 2
end


###############################################################################

function return_in_branch(x)
    if x > 0
        return 2x
    end
    return 3x
end

@testset "trace: branches" begin
    # See https://github.com/dfdx/Ghost.jl/issues/8
    _, tape = trace(return_in_branch, 1.0)
    @test tape[V(length(tape))].args[1] == 2

    _, tape = trace(return_in_branch, -1.0)
    @test tape[V(length(tape))].args[1] == 3
end


###############################################################################

function loop1(a, n)
    a = 2a
    for i in 1:n
        a = a * n
    end
    return a
end

function loop2(a, b)
    while b > 0
        a = a * b
        b = b - 1
    end
    return a
end


function loop3(a, b)
    while b > 1
        b = b - 1
        a = b
        while a < 100
            a = a * b + 1
        end
    end
    return a
end


function loop4(x, n, m)
    for i in 1:n
        for j in 1:m
            x = 2x
        end
    end
    return x
end


function loop5(a, n)
    for i=1:3
        a = loop1(a, n)
    end
    return a
end

function loop6(n)
    a = 0
    i = 1
    while true
        a += i
        if a > n
            break
        end
    end
    return a
end

function myabs(x)
    if x >= 0
        return x
    else
        return -x
    end
end

function myabs2(x)
    if x <= 0
        return -x
    else
        return 1*x
    end
end

@testset "trace: loops" begin
    # should_trace_loops!(false)

    _, tape = trace(loop1, 1.0, 3)
    @test findfirst(op -> op isa Loop, tape.ops) === nothing
    # same number of iteration
    @test play!(tape, loop1, 2.0, 3) == loop1(2.0, 3)
    @test compile(tape)(loop1, 2.0, 3) == loop1(2.0, 3)
    # different number of iteration - with loop tracing off, should be incorrect
    @test play!(tape, loop1, 2.0, 4) != loop1(2.0, 4)
    @test compile(tape)(loop1, 2.0, 4) != loop1(2.0, 4)

    # should_trace_loops!(true)

    # _, tape = trace(loop1, 1.0, 3)
    # @test play!(tape, loop1, 2.0, 4) == loop1(2.0, 4)
    # @test compile(tape)(loop1, 2.0, 4) == loop1(2.0, 4)
    # @test findfirst(op -> op isa Loop, tape.ops) !== nothing

    # _, tape = trace(loop2, 1.0, 3)
    # @test play!(tape, loop2, 2.0, 4) == loop2(2.0, 4)
    # @test compile(tape)(loop2, 2.0, 4) == loop2(2.0, 4)
    # @test findfirst(op -> op isa Loop, tape.ops) !== nothing

    # _, tape = trace(loop3, 1.0, 3)
    # @test play!(tape, loop3, 2.0, 4) == loop3(2.0, 4)
    # @test compile(tape)(loop3, 2.0, 4) == loop3(2.0, 4)
    # @test findfirst(op -> op isa Loop, tape.ops) !== nothing

    # _, tape = trace(loop4, 1.0, 2, 3)
    # @test play!(tape, loop4, 2.0, 3, 4) == loop4(2.0, 3, 4)
    # @test compile(tape)(loop4, 2.0, 3, 4) == loop4(2.0, 3, 4)
    # loop_idx = findfirst(op -> op isa Loop, tape.ops)
    # @test loop_idx !== nothing
    # subtape = tape[V(loop_idx)].subtape
    # @test findfirst(op -> op isa Loop, subtape.ops) !== nothing

    # _, tape = trace(loop5, 1.0, 3)
    # @test play!(tape, loop5, 2.0, 4) == loop5(2.0, 4)
    # @test compile(tape)(loop5, 2.0, 4) == loop5(2.0, 4)
    # loop_idx = findfirst(op -> op isa Loop, tape.ops)
    # @test loop_idx !== nothing
    # subtape = tape[V(loop_idx)].subtape
    # @test findfirst(op -> op isa Loop, subtape.ops) !== nothing

    # should_trace_loops!()

    # Test with fixed boolean condition
    # Currently broken
    #=
    _, tape = trace(loop6, 3)
    @test play!(tape, loop6, 3) == loop6(3)
    @test compile(tape, loop6, 3) == loop6(3)
    =#
end

# @testset "trace: branches" begin
#     should_assert_branches!(true)

#     _, tape = trace(myabs, 3)
#     @test play!(tape, myabs, 3) == 3
#     @test_throws AssertionError play!(tape, myabs, -3) # Throws assert exception due to bad branch

#     _, tape = trace(myabs2, 3)
#     @test play!(tape, myabs2, 3) == 3
#     @test_throws AssertionError play!(tape, myabs2, -3) # Test with branching flipped

#     should_assert_branches!(false)
# end


###############################################################################

function pow(x, n)
    r = 1
    for i=1:n
        r = x * r
    end
    return r
end


function const1(x)
    y = 1
    return x + y
end


function cond1(x)
    y = 2x
    if x > 0
        y = 3x
    end
    return y
end


function while1(x)
    y = 10
    while x > 5
        y -= 1
        x /= 2
    end
    return y
end


function while_continue(x)
    y = 1
    while y < x
        if y % 3 == 0
            y += 1
            continue
        end
        y *= 2
    end
    return y
end


function while_break(x)
    y = 1
    while true
        if y > x
            break
        end
        y *= 2y
    end
    return y
end


@testset "trace: jumps" begin
    # a few smoke tests to ensure static tracer handles jumps properly
    _, tape = trace(pow, 2.0, 3)
    @test play!(tape, pow, 3.0, 3) == pow(3.0, 3)

    _, tape = trace(const1, 2.0)
    @test play!(tape, const1, 3.0) == const1(3.0)

    _, tape = trace(cond1, 2.0)
    @test play!(tape, cond1, 3.0) == cond1(3.0)

    _, tape = trace(while1, 2.0)
    @test play!(tape, while1, 2.0) == while1(2.0)
    _, tape = trace(while1, 7.0)
    @test play!(tape, while1, 7.0) == while1(7.0)

    _, tape = trace(while_continue, 3.0)
    @test play!(tape, while_continue, 3.0) == while_continue(3.0)

    _, tape = trace(while_break, 3.0)
    @test play!(tape, while_break, 3.0) == while_break(3.0)
end


###############################################################################

mutable struct CountingReplacingCtx
    replace::Pair
    count::Int
end

function record_primitive!(tape::Tape{CountingReplacingCtx}, v_fargs...)
    if v_fargs[1] == tape.c.replace[1]
        tape.c.count += 1
        return push!(tape, mkcall(tape.c.replace[2], v_fargs[2:end]...))
    else
        return push!(tape, mkcall(v_fargs...))
    end
end

@testset "record_primitive!" begin
    _, tape = trace(loop1, 2.0, 3; ctx=CountingReplacingCtx((*) => (+), 0))
    @test tape.c.count == 4
    @test count(op -> op isa Call && op.fn == (+), tape) == 4
end

###############################################################################

@testset "passing missing as argument" begin
    res, _ = trace(ismissing, missing)
    @test !ismissing(res)
    @test res === true
end

###############################################################################

function enter_leave_tester(x)
    try
        x > 0 && throw(error("an error"))
    catch
        x += 1
    end
    return x
end

@testset "enter" begin
    y, tape = trace(enter_leave_tester, -0.5)
    @test y == enter_leave_tester(-0.5)
    @test enter_leave_tester(0.5) == 1.5
    @test_throws ErrorException trace(enter_leave_tester, 0.5)
end

###############################################################################

# Cannot be traced if you don't check if the `values` field of a `PhiNode` is
# defined or not before accessing.
function conditionally_defined_tester(x)
    isneg = x < 0
    if isneg
        y = 1.0
    end
    if isneg
        x += y
    end
    return x
end

@testset "undef in PhiNode" begin
    res, tape = trace(conditionally_defined_tester, 5.0)
    @test res == conditionally_defined_tester(5.0)
    @test play!(tape, conditionally_defined_tester, 5.0) == res
end

###############################################################################

function undefcheck_tester(x)
    if x > 0
        y = 5.0
    end
    return y # the compiler inserts an :undefcheck expression near here.
end

@testset "undefcheck" begin
    res, tape = trace(undefcheck_tester, 5.0)
    @test res == undefcheck_tester(5.0)
    @test play!(tape, undefcheck_tester, 5.0) == res
end
