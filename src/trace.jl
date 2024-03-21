# Naming in this file:
#
# ir :: IRCode
# v_xxx :: Variable xxx
# sv_xxx :: SSAValue xxx or Argument xxx
# pc :: SSA ID (abbreviation from "program counter")
# bi :: block ID
# fargs :: tuple of function and its arguments, e.g. (+, 1, 2)
# v_fargs :: same as fargs, but may contain vars instead if values, e.g. (+, %3, 1)
# fargtypes :: tuple of function and argument types, e.g. (+, (Int, Int))


const VecOrTuple = Union{Tuple, Vector}


###############################################################################
#                                  Frame                                      #
###############################################################################

# see https://github.com/dfdx/Umlaut.jl/issues/58
function new_exprs(ir::IRCode)
    stmts = ir.new_nodes.stmts
    if hasfield(typeof(stmts), :inst)
        return stmts.inst
    else
        return stmts.stmt
    end
end


"""
    block_expressions(ir::IRCode)

For each block, compute a vector of its expressions along with their SSA IDs.
Returns Vector{block_info}, where block_info is Vector{ssa_id => expr}
"""
function block_expressions(ir::IRCode)
    # new statements
    new_exs = new_exprs(ir)
    # where to insert them
    new_positions = [(info.attach_after ? info.pos + 1 : info.pos)
                    for info in ir.new_nodes.info]
    # their indices (program counters)
    new_pcs = [idx + length(ir.stmts) for idx=1:length(new_exs)]
    new_node_map = Dict{Int, Vector{Any}}()
    for (pos, pc, ex) in zip(new_positions, new_pcs, new_exs)
        if !haskey(new_node_map, pos)
            new_node_map[pos] = []
        end
        push!(new_node_map[pos], (pc, ex))
    end
    block_exprs = Vector{Vector{Tuple}}(undef, 0)
    for bi in eachindex(ir.cfg.blocks)
        pc_exprs = Tuple[]
        for pos in ir.cfg.blocks[bi].stmts
            if haskey(new_node_map, pos)
                for pc_ex in new_node_map[pos]
                    push!(pc_exprs, pc_ex)
                end
            end
            push!(pc_exprs, (pos, ir.stmts[pos][:inst]))
        end
        push!(block_exprs, pc_exprs)
    end
    return block_exprs
end


mutable struct Frame
    # typically values are Variables, but can also be constant values
    ir2tape::Dict{Union{SSAValue, Argument}, Any}
    # block_exprs[bi] = [(pc => expr), ...]
    block_exprs::Vector{Vector{Tuple}}
    # map from pc to corresponding block ID
    pc_blocks::Dict{Int, Int}
    # debug info
    ir::IRCode
    v_fargs
end


function Frame(tape::Tape, ir::IRCode, v_fargs...)
    ir2tape = Dict{Union{SSAValue, Argument}, Any}()
    for (i, v) in enumerate(v_fargs)
        if v isa V
            ir2tape[Argument(i)] = v
        else
            # c = push!(tape, Constant(promote_const_value(v)))
            # ir2tape[Argument(i)] = c
            ir2tape[Argument(i)] = v  # experimental
            # TODO: unify code in 2 branches if it works this way
        end
    end
    block_exprs = block_expressions(ir)
    pc_blocks = Dict([pc => bi for bi in eachindex(block_exprs) for (pc, _) in block_exprs[bi]])
    return Frame(ir2tape, block_exprs, pc_blocks, ir, v_fargs)
end


getid(x::SSAValue) = x.id
getid(x::Argument) = x.n


function Base.show(io::IO, frame::Frame)
    s = "Frame(\n"
    for (sv, v) in sort(collect(frame.ir2tape), by=p->getid(p[1]))
        s *= "  $sv => $v\n"
    end
    s *= ")"
    print(io, s)
end


function resolve_tape_vars(frame::Frame, sv_fargs...)
    v_fargs = []
    for sv in sv_fargs
        if sv isa Argument || sv isa SSAValue
            push!(v_fargs, frame.ir2tape[sv])
        else
            push!(v_fargs, promote_const_value(sv))
        end
    end
    return v_fargs
end


###############################################################################
#                           Context & Primitives                              #
###############################################################################

"""
Dict-like tracing context that treats as primitives all functions from the
standard Julia modules (e.g. Base, Core, Statistics, etc.)
"""
struct BaseCtx
    primitives::Set
    data::Dict
end

BaseCtx() = BaseCtx(Set(), Dict())
BaseCtx(primitives) = BaseCtx(Set(primitives), Dict())

function Base.show(io::IO, ctx::BaseCtx)
    n_primitives = length(ctx.primitives)
    n_entries = length(ctx.data)
    print(io, "BaseCtx($n_primitives primitives, $n_entries entries)")
end

Base.getindex(ctx::BaseCtx, key) = getindex(ctx.data, key)
Base.setindex!(ctx::BaseCtx, val, key) = setindex!(ctx.data, val, key)


"""
    isprimitive(ctx::BaseCtx, f, args...)

The default implementation of `isprimitive` used in [`trace()`](@ref).
Returns `true` if the method with the provided signature is defined
in one of the Julia's built-in modules, e.g. `Base`, `Core`, `Broadcast`, etc.
"""
function isprimitive(ctx::BaseCtx, f, args...)
    if isempty(ctx.primitives)
        f in (__new__, Colon(), Base.Generator, __foreigncall__) && return true
        f isa NamedTuple && return true
        (f isa ComposedFunction || f === (∘)) && return false
        modl = module_of(f, args...)
        modl in (Base, Base.Math, Core, Core.Intrinsics, Broadcast, Statistics, LinearAlgebra) && return true
        return false
    else
        return f in ctx.primitives
    end
end


"""
    isprimitive(ctx::Any, f, args...)

Fallback implementation of `isprimitive()`, behaves the same way
as `isprimitive(BaseCtx(), f, args...)`.
"""
isprimitive(ctx::Any, f, args...) = isprimitive(BaseCtx(), f, args...)


"""
    is_ho_tracable(ctx::Any, f, args...)

Is higher-order tracable. Returns true if `f` is a known higher-order function
that Umlaut knows how to trace and its functional argument is a non-primitive.

`is_ho_tracable()` helps to trace through higher-order functions like `Core._apply_iterate()`
(used internally when splatting arguments with ...) as if they themselves
were non-primitives.
"""
function is_ho_tracable(ctx::Any, fargs...)
    f = fargs[1]
    if f === Core._apply_iterate && !isprimitive(ctx, fargs[3:end]...)
        return true
    end

    return false
end


"""
    record_primitive!(tape::Tape{C}, v_fargs...) where C

Record a primitive function call to the tape.

By default, this function simply pushes the function call to the tape,
but it can also be overwritten to do more complex logic. For example,
instead of recording the function call, a user can push one or more
other calls, essentially implementing `replace!()` right during the
tracing and without calling the function twice.

Examples:
=========

The following code shows how to replace f(args...) with ChainRules.rrule(f, args...)
duing the tracing:

    function record_primitive!(tape::Tape{RRuleContext}, v_fargs)
        v_rr = push!(tape, mkcall(rrule, v_fargs...))
        v_val = push!(tape, mkcall(getfield, v_rr, 1))
        v_pb = push!(tape, mkcall(getfield, v_rr, 1))
        tape.c.pullbacks[v_val] = v_pb
        return v_val   # the function should return Variable with the result
    end


See also: [`isprimitive()`](@ref)
"""
function record_primitive!(tape::Tape, v_fargs...)
    line = get(tape.meta, :line, nothing)
    push!(tape, mkcall(v_fargs...; line=line))
end


###############################################################################
#                                 Tracing                                     #
###############################################################################


mutable struct Tracer{C}
    tape::Tape{C}
    stack::Vector{Frame}
end

Tracer(tape::Tape{C}) where C = Tracer{C}(tape, [])


function getcode(f, argtypes)
    irs = Base.code_ircode(f, argtypes; optimize_until="slot2reg")
    @assert !isempty(irs) "No IR found for $f($argtypes...)"
    @assert length(irs) == 1 "More than one IR found for $f($argtypes...)"
    @assert irs[1] isa Pair{IRCode, <:Any} "Expected Pair{IRCode,...}, " *
            "but got $(typeof(irs[1])) instead for f=$f with argtypes=$argtypes"
    return irs[1][1]
end


macro getcode(ex)
    f, args... = ex.args
    return quote
        _f = $(esc(f))
        _args = $(esc(args))
        argtypes = map(Core.Typeof, _args)
        getcode(_f, argtypes)
    end
end


function rewrite_special_cases(st::Expr)
    ex = Meta.isexpr(st, :(=)) ? st.args[2] : st
    if Meta.isexpr(ex, :new)
        ex = Expr(:call, __new__, ex.args...)
    end
    if Meta.isexpr(ex, :splatnew)
        ex = Expr(:call, __splatnew__, ex.args...)
    end
    # replace :($(Expr(:boundscheck))) with just `true`
    if Meta.isexpr(ex, :boundscheck)
        ex = true
    end
    # same for arguments
    if ex isa Expr
        ex.args = [Meta.isexpr(arg, :boundscheck) ? true : arg for arg in ex.args]
    end
    return Meta.isexpr(st, :(=)) ? Expr(:(=), st.args[1], ex) : ex
end
rewrite_special_cases(st) = st

function get_static_params(t::Tracer, v_fargs::VecOrTuple)
    mi = Base.method_instances(code_signature(t.tape.c, v_fargs)..., Base.get_world_counter())[1]
    mi_dict = Dict(zip(sparam_names(mi), mi.sparam_vals))
    return mi.sparam_vals, mi_dict
end

function sparam_names(m::Core.Method)
    whereparams = ExprTools.where_parameters(m.sig)
    whereparams === nothing && return []
    return map(whereparams) do name
        name isa Symbol && return name
        Meta.isexpr(name, :(<:)) && return name.args[1]
        Meta.isexpr(name, :(>:)) && return name.args[1]
        error("unrecognised type param $name")
    end
end
sparam_names(mi::Core.MethodInstance) = sparam_names(mi.def)


"""
    trace_call!(t::Tracer{C}, v_f, v_args...) where C

Customizable handler that controls what to do with a function call.
The default implementation checks if the call is a primitive and either
records it to the tape or recurses into it.
"""
function trace_call!(t::Tracer{C}, vs...) where C
    fargs = var_values(vs)
    if isprimitive(t.tape.c, fargs...) && !is_ho_tracable(t.tape.c, fargs...)
        return record_primitive!(t.tape, vs...)
    else
        return trace!(t, vs)
    end
end


function record_or_recurse!(t, vs...)
    @warn "record_or_recurse!(t, vs...) is deprecated, use trace_call!(t, vs...) instead"
    trace_call!(t, vs...)
end


function is_control_flow(ex)
    return ex isa GotoNode || ex isa GotoIfNot || ex isa ReturnNode ||
        Meta.isexpr(ex, :enter)
end



function getlineinfo(ir::IRCode, pc::Integer)
    if (1 <= pc <= length(ir.stmts.line)) &&
        (1 <= ir.stmts.line[pc] <= length(ir.linetable))
        return ir.linetable[ir.stmts.line[pc]]
    else
        approx_loc = first(ir.linetable)
        return "near $(approx_loc.file):$(approx_loc.line)"
    end
end

function trace_block!(t::Tracer, ir::IRCode, bi::Integer, prev_bi::Integer, sparams, sparams_dict)
    frame = t.stack[end]
    for (pc, ex) in frame.block_exprs[bi]
        ex = rewrite_special_cases(ex)
        line = getlineinfo(ir, pc)
        if is_control_flow(ex)
            return ex   # exit on control flow statement
        elseif Meta.isexpr(ex, :call)
            vs = resolve_tape_vars(frame, ex.args...)
            vs = [Meta.isexpr(x, :static_parameter) ? sparams[x.args[1]] : x for x in vs]
            vs = unsplat!(t, vs)
            t.tape.meta[:line] = line
            v = trace_call!(t, vs...)
            t.tape.meta[:line] = nothing
            frame.ir2tape[SSAValue(pc)] = v
        elseif ex isa Core.PhiNode
            # map current pc to the currently active value of Phi node
            ir2tape = t.stack[end].ir2tape
            k = indexin(prev_bi, ex.edges)[]
            if isassigned(ex.values, k)
                v = ex.values[k]
                # It is possible that the value associated to a PhiNode is a constant,
                # raather than a Variable.
                if v isa Union{Core.SSAValue, Core.Argument}
                    ir2tape[SSAValue(pc)] = ir2tape[v]
                else
                    ir2tape[SSAValue(pc)] = v
                end
            end
        elseif ex isa Core.PiNode
            # val = t.tape[frame.ir2tape[ex.val]].val
            # frame.ir2tape[SSAValue(pc)] = push!(t.tape, Constant(val; line))
            frame.ir2tape[SSAValue(pc)] = frame.ir2tape[ex.val]
        elseif ex isa SSAValue || ex isa Argument
            # assignment
            sv = SSAValue(pc)
            frame.ir2tape[sv] = frame.ir2tape[ex]
        elseif Meta.isexpr(ex, :static_parameter)
            sv = SSAValue(pc)
            frame.ir2tape[sv] = sparams[ex.args[1]]
        elseif Meta.isexpr(ex, :foreigncall)
            @static if VERSION < v"1.9"
                throw(error("foreigncall not supported on versions less than 1.9"))
            end

            vs = resolve_tape_vars(frame, ex.args...)

            # Extract arguments.
            name = extract_foriegncall_name(vs[1])
            RT = Val(interpolate_sparams(vs[2], sparams_dict))
            AT = (map(x -> Val(interpolate_sparams(x,sparams_dict)), vs[3])..., )
            nreq = Val(vs[4])
            calling_convention = Val(vs[5])
            x = vs[6:end]
            frame.ir2tape[SSAValue(pc)] = push!(
                t.tape,
                mkcall(__foreigncall__, name, RT, AT, nreq, calling_convention, x...),
            )
        elseif Meta.isexpr(ex, :undefcheck)
            @assert haskey(frame.ir2tape, ex.args[2])
        elseif Meta.isexpr(ex, :throw_undef_if_not)
            @assert haskey(frame.ir2tape, ex.args[2])
        elseif ex isa Expr && ex.head in [
            :code_coverage_effect, :gc_preserve_begin, :gc_preserve_end, :loopinfo,
            :leave, :pop_exception,
        ]
            # ignored expressions, just skip it
        elseif ex isa Expr
            error("Unexpected expression: $ex\nFull IRCode:\n\n $ir")
        else
            # treat as constant
            v = push!(t.tape, Constant(promote_const_value(ex); line))
            frame.ir2tape[SSAValue(pc)] = v
        end
    end
    return  # exit on implicit fallthrough
end

extract_foriegncall_name(x::Symbol) = Val(x)
function extract_foriegncall_name(x::Expr)
    # Make sure that we're getting the expression that we're expecting.
    !Meta.isexpr(x, :call) && error("unexpected expr $x")
    !isa(x.args[1], GlobalRef) && error("unexpected expr $x")
    x.args[1].name != :tuple && error("unexpected expr $x")
    length(x.args) != 3 && error("unexpected expr $x")

    # Parse it into a name that can be passed as a type.
    v = eval(x)
    return Val((Symbol(v[1]), Symbol(v[2])))
end
function extract_foriegncall_name(v::Tuple)
    return Val((Symbol(v[1]), Symbol(v[2])))
end

# adapted from https://github.com/JuliaDebug/JuliaInterpreter.jl/blob/aefaa300746b95b75f99d944a61a07a8cb145ef3/src/optimize.jl#L239
function interpolate_sparams(@nospecialize(t::Type), sparams::Dict)
    t isa Core.TypeofBottom && return t
    while t isa UnionAll
        t = t.body
    end
    t = t::DataType
    if Base.isvarargtype(t)
        return Expr(:(...), t.parameters[1])
    end
    if Base.has_free_typevars(t)
        params = map(t.parameters) do @nospecialize(p)
            if isa(p, TypeVar)
                return sparams[p.name]
            elseif isa(p, DataType) && Base.has_free_typevars(p)
                return interpolate_sparams(p, sparams)
            else
                return p
            end
        end
        T = t.name.Typeofwrapper.parameters[1]
        return T{params...}
    end
    return t
end

function check_variable_length(val, len::Integer, id::Integer)
    if length(val) != len
        @warn("Variable %$(id) had length $len during tracing, " *
              "but now has length $(length(val))")
    end
end

"""
    code_signature(ctx, v_fargs)

Returns method signature as a tuple (f, (arg1_typ, arg2_typ, ...)).
This signature is suitable for getcode() and which().
"""
function code_signature(ctx, v_fargs)
    fargs = var_values(v_fargs)
    f, args... = fargs
    fargtypes = (f, map(Core.Typeof, args))
    return fargtypes
end

__to_tuple__(x::Tuple) = x
__to_tuple__(x::NamedTuple) = Tuple(x)
__to_tuple__(x::Array) = Tuple(x)
__to_tuple__(x) = __to_tuple__(collect(x))

"""
    unsplat!(t::Tracer, v_fargs)

In the lowered form, splatting syntax f(xs...) is represented as
Core._apply_iterate(f, xs). unsplat!() reverses this change and transformes
v_fargs to a normal form, possibly destructuring xs into separate variables
on the tape.
"""
function unsplat!(t::Tracer, v_fargs)
    f = v_fargs[1] isa V ? t.tape[v_fargs[1]] : v_fargs[1]
    # handle splatted arguments
    if f === Core._apply_iterate
        # destructure splatted arguments as separate vars onto the tape
        # or use existing vars if they can be inferred
        actual_v_args = []
        for v in v_fargs[4:end]
            check_call = mkcall(check_variable_length, v, length(v.op.val), v.id; line="Umlaut.unsplat!")
            push!(t.tape, check_call)
            iter = t.tape[v]
            is_tuple = iter isa Call && iter.fn === Base.tuple
            if is_tuple
                push!(actual_v_args, iter.args...)
            else
                tuple_v = push!(t.tape, mkcall(__to_tuple__, v; line="Umlaut.unsplat!"))
                iter = t.tape[tuple_v].val
                for i in eachindex(iter)
                    x = push!(t.tape, mkcall(getfield, tuple_v, i; line="Umlaut.unsplat!"))
                    push!(actual_v_args, x)
                end
            end
        end
        v_fargs = (v_fargs[3], actual_v_args...)
    end
    return v_fargs
end


function group_varargs!(t::Tracer, v_fargs)
    v_f, v_args... = v_fargs
    fargtypes = code_signature(t.tape.c, v_fargs)
    # ir = getcode(fargtypes...)
    meth = which(fargtypes...)
    # if the method has varargs, group extra vars into a tuple
    if meth.isva
        extra_v_args = v_args[meth.nargs - 1:end]
        # don't group the input varargs tuple - we handle it separately
        # if !(length(extra_v_args) == 1 && t.tape[extra_v_args[1]] isa Input)
        va = record_primitive!(t.tape, tuple, extra_v_args...)
        v_args = (v_args[1:meth.nargs - 2]..., va)
        # end
    end
    return (v_f, v_args...)
end

"""
    handle_gotoifnot_node!(t::Tracer, cf::Core.GotoIfNot, frame::Frame)

Must return the value associated to a `Core.GotoIfNot` node.
May also have side-effects, such as modifying the tape.
"""
function handle_gotoifnot_node!(t::Tracer, cf::Core.GotoIfNot, frame::Frame)
    return if cf.cond isa Argument || cf.cond isa SSAValue
        # resolve tape var
        t.tape[frame.ir2tape[cf.cond]].val
    elseif cf.cond isa Bool
        # literal condition (e.g. while true)
        cf.cond
    elseif cf.cond == Expr(:boundscheck)
        # boundscheck expression must be evaluated on some later stage
        # of interpretation that we don't have access to on this level
        # so just skipping the check instead
        true
    else
        exc = AssertionError(
            "Expected goto condition to be of type Argument, " *
            "SSAValue or Bool, but got $(cf.cond). \n\nFull IR: \n\n$(ir)\n"
        )
        throw(exc)
    end
end

"""
    trace!(t::Tracer, v_fargs)

Trace call defined by variables in v_fargs.
"""
function trace!(t::Tracer, v_fargs)
    v_fargs = unsplat!(t, v_fargs)
    # note: we need to extract IR before vararg grouping, which may change
    # v_fargs, thus invalidating method search
    ir = getcode(code_signature(t.tape.c, v_fargs)...)
    sparams, sparams_dict = get_static_params(t, v_fargs)
    v_fargs = group_varargs!(t, v_fargs)
    frame = Frame(t.tape, ir, v_fargs...)
    push!(t.stack, frame)
    bi = 1
    prev_bi = 0
    cf = nothing
    while bi <= length(ir.cfg.blocks)
        cf = trace_block!(t, ir, bi, prev_bi, sparams, sparams_dict)
        if isnothing(cf)
            # fallthrough to the next block
            prev_bi = bi
            bi += 1
        elseif cf isa Core.GotoIfNot

            # conditional jump
            cond_val = handle_gotoifnot_node!(t, cf, frame)

            # if not cond, set i to destination, otherwise step forward
            prev_bi = bi
            bi = !cond_val ? cf.dest : bi + 1
        elseif cf isa Core.GotoNode
            # unconditional jump
            prev_bi = bi
            bi = cf.label
        elseif cf isa ReturnNode
            # global STATE = t, cf, ir
            isdefined(cf, :val) || error("Reached unreachable")
            res = cf.val
            f, args... = var_values(v_fargs)
            line = "return value from $f$(map(Core.Typeof, args))"
            v = if res isa SSAValue || res isa Argument
                val = frame.ir2tape[res]
                val isa V ? val : push!(t.tape, Constant(promote_const_value(val); line))
            elseif Meta.isexpr(res, :static_parameter, 1)
                val = sparams[res.args[1]]
                push!(t.tape, Constant(promote_const_value(val); line))
            else
                push!(t.tape, Constant(promote_const_value(res); line))
            end
            pop!(t.stack)
            return v
        elseif Meta.isexpr(cf, :enter)
            prev_bi = bi
            bi += 1
        else
            error("Panic! Don't know how to handle control flow expression $cf")
        end
    end
    pop!(t.stack)
    # if no ReturnNode was encountered, use last op on the tape
    return V(t.tape[V(end)])
end


const LATEST_TRACER = Ref{Tracer}()



"""
    trace(f, args...; ctx=BaseCtx())

Trace function call, return the result and the corresponding Tape.
`trace` records to the tape primitive methods and recursively dives into
non-primitives.

Tracing can be customized using a context and the following methods:

* isprimitive(ctx, f, args...) - decides whethere `f(args...)` should be
  treated as a primitive.
* record_primitive!(tape::Tape{C}, v_f, v_args...) - records the primitive
  call defined by variables `f_v(v_args...)` to the tape.

The default context is `BaseCtx()`, which treats all functions from standard
Julia modules as primitives and simply pushes the call to the tape. See the
docstrings of these functions for further examples of customization.

Examples:
=========

    foo(x) = 2x
    bar(x) = foo(x) + 1

    val, tape = trace(bar, 2.0)
    # (5.0, Tape{Dict{Any, Any}}
    #   inp %1::typeof(bar)
    #   inp %2::Float64
    #   %3 = *(2, %2)::Float64
    #   %4 = +(%3, 1)::Float64
    # )

    val, tape = trace(bar, 2.0; ctx=BaseCtx([*, +, foo]))
    # (5.0, Tape{Dict{Any, Any}}
    #   inp %1::typeof(bar)
    #   inp %2::Float64
    #   %3 = foo(%2)::Float64
    #   %4 = +(%3, 1)::Float64
    # )

    struct MyCtx end

    isprimitive(ctx::MyCtx, f, args...) = isprimitive(BaseCtx(), f, args...) || f in [foo]
    val, tape = trace(bar, 2.0; ctx=MyCtx())
    # (5.0, Tape{Dict{Any, Any}}
    #   inp %1::typeof(bar)
    #   inp %2::Float64
    #   %3 = foo(%2)::Float64
    #   %4 = +(%3, 1)::Float64
    # )

"""
function trace(f, args...; ctx=BaseCtx(), deprecated_kws...)
    warn_deprecated_keywords(deprecated_kws)
    t = Tracer(Tape(ctx))
    v_fargs = inputs!(t.tape, f, args...)
    try
        rv = trace!(t, v_fargs)
        t.tape.result = rv
        return t.tape[t.tape.result].val, t.tape
    catch
        LATEST_TRACER[] = t
        rethrow()
    end
end


###############################################################################
#                           Post-tracing utils                                #
###############################################################################


get_latest_tracer() = LATEST_TRACER[]

function get_latest_tracer_state()
    t = get_latest_tracer()
    frame = t.stack[end]
    return t, frame.ir, frame.v_fargs
end

function print_stack_trace()
    t = get_latest_tracer()
    for (i, frame) in enumerate(reverse(t.stack))
        fn, args... = [v isa V ? t.tape[v].val : v for v in frame.v_fargs]
        meth = which(fn, map(Core.Typeof, args))
        println("[$i] $meth")
        # println("  @ $(meth.module) $(meth.file)")
    end
end
