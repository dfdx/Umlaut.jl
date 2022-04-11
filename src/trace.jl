# Naming:
# v_xxx - Variable xxx
# sv_xxx - SSAValue xxx or SlotNumber xxx

###############################################################################
#                                  Frame                                      #
###############################################################################

mutable struct Frame
    # typically values are Variables, but can also be constant values
    ir2tape::Dict{Union{SSAValue, SlotNumber}, Any}
    # debug info
    ci::CodeInfo
    v_fargs
end


function Frame(tape::Tape, ci::CodeInfo, v_fargs...)
    ir2tape = Dict{Union{SSAValue, SlotNumber}, Any}()
    for (i, v) in enumerate(v_fargs)
        if v isa V
            ir2tape[SlotNumber(i)] = v
        else
            # c = push!(tape, Constant(promote_const_value(v)))
            # ir2tape[SlotNumber(i)] = c
            ir2tape[SlotNumber(i)] = v  # experimental
        end

    end
    return Frame(ir2tape, ci, v_fargs)
end

function Base.show(io::IO, frame::Frame)
    s = "Frame(\n"
    for (sv, v) in sort(frame.ir2tape, by=sv -> sv.id)
        s *= "  $sv => $v\n"
    end
    s *= ")"
    print(io, s)
end


function resolve_tape_vars(frame::Frame, sv_fargs...)
    v_fargs = []
    for sv in sv_fargs
        if sv isa SlotNumber || sv isa SSAValue
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
    print(io, "DefaultCtx($n_primitives primitives, $n_entries entries)")
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
        f in (__new__, Colon(), Base.Generator) && return true
        f isa NamedTuple && return true
        f isa DataType && return false   # usually we want to recurse to constructors
        modl = parentmodule(typeof(f))
        modl in (Base, Core, Core.Intrinsics, Broadcast, Statistics, LinearAlgebra) && return true
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
record_primitive!(tape::Tape, v_fargs...) = push!(tape, mkcall(v_fargs...))


###############################################################################
#                                 Tracing                                     #
###############################################################################


mutable struct Tracer{C}
    tape::Tape{C}
    stack::Vector{Frame}
end

Tracer(tape::Tape{C}) where C = Tracer{C}(tape, [])


function getcode(f, types)
    cis = code_lowered(f, types)
    if isempty(cis)
        arg_type_str = join(types, ", ")
        error("Cannot get CodeInfo for $f($arg_type_str)")
    end
    return cis[1]
end


function rewrite_special_cases(st::Expr)
    ex = Meta.isexpr(st, :(=)) ? st.args[2] : st
    if Meta.isexpr(ex, :new)
        ex = Expr(:call, __new__, ex.args...)
    end
    return Meta.isexpr(st, :(=)) ? Expr(:(=), st.args[1], ex) : ex
end
rewrite_special_cases(st) = st


"""
    record_or_recurse!(t::Tracer{C}, v_f, v_args...) where C

Customizable handler that controls what to do with a function call.
The default implementation checks if the call is a primitive and either
records it to the tape or recurses into it.
"""
function record_or_recurse!(t::Tracer{C}, vs...) where C
    fvals = [v isa V ? t.tape[v].val : v for v in vs]
    return if isprimitive(t.tape.c, fvals...)
        record_primitive!(t.tape, vs...)
    else
        trace!(t, getcode(fvals[1], map(typeof, fvals[2:end])), vs...)
    end
end


function trace!(t::Tracer, ci::CodeInfo, v_fargs...)
    frame = Frame(t.tape, ci, v_fargs...)
    push!(t.stack, frame)
    i = 1
    while i <= length(ci.code)
        st = rewrite_special_cases(ci.code[i])
        # global STATE = (t, ci, v_fargs, frame, i, st)
        if Meta.isexpr(st, :call) || (Meta.isexpr(st, :(=)) && Meta.isexpr(st.args[2], :call))
            # function call
            sv = SSAValue(i)
            ex = Meta.isexpr(st, :(=)) ? st.args[2] : st
            vs = resolve_tape_vars(frame, ex.args...)
            v = record_or_recurse!(t, vs...)
            frame.ir2tape[sv] = v
            if Meta.isexpr(st, :(=))
                # update mapping for slot
                slot = st.args[1]
                frame.ir2tape[slot] = v
            end
            i += 1
        elseif Meta.isexpr(st, :(=))
            # constant or assignment
            sv = st.args[1]
            rhs = resolve_tape_vars(frame, st.args[2])[1]
            # RHS may be a variable or a constant value; in both cases we simply
            # update the mapping from LHS (SlotNumber & SSAValue) to the RHS
            frame.ir2tape[sv] = rhs
            frame.ir2tape[SSAValue(i)] = rhs
            i += 1
        elseif st isa SlotNumber
            # assignment
            sv = SSAValue(i)
            frame.ir2tape[sv] = frame.ir2tape[st]
            i += 1
        elseif st isa Core.GotoIfNot
            # conditional jump
            cond_val = (st.cond isa SlotNumber || st.cond isa SSAValue ?
                            t.tape[frame.ir2tape[st.cond]].val :   # resolve tape var
                            st.cond)                               # literal condition (e.g. while true)
            # if not cond, set i to destination, otherwise step forward
            i = !cond_val ? st.dest : i + 1
        elseif st isa Core.GotoNode
            # unconditional jump
            i = st.label
        elseif st isa Core.ReturnNode
            # return statement
            sv = st.val
            if sv isa SSAValue || sv isa SlotNumber
                val = frame.ir2tape[sv]
                v = val isa V ? val : push!(t.tape, Constant(promote_const_value(val)))
                pop!(t.stack)
                return v
            else
                v = push!(t.tape, Constant(promote_const_value(sv)))
                pop!(t.stack)
                return v
            end
        else
            # treat as constant
            v = push!(t.tape, Constant(promote_const_value(st)))
            frame.ir2tape[SSAValue(i)] = v
            i += 1
            # error("Unexpected statement type in CodeInfo: $st")
        end
    end
    pop!(t.stack)
    # if no ReturnNode was encountered, use last op on the tape
    return V(t.tape[V(end)])
end


const LATEST_TRACER = Ref{Tracer}()
get_latest_tracer() = LATEST_TRACER[]


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
function trace(f, args...; ctx=BaseCtx(), fargtypes=nothing, deprecated_kws...)
    warn_deprecated_keywords(deprecated_kws)
    if isnothing(fargtypes)
        fargtypes = (f, map(typeof, args))
    end
    t = Tracer(Tape(ctx))
    meth = which(fargtypes...)
    xargs = meth.isva ? (args[1:meth.nargs - 2]..., args[meth.nargs - 1:end]) : args
    t.tape.meta[:isva] = meth.isva
    v_fargs = inputs!(t.tape, f, xargs...)
    ci = getcode(fargtypes...)
    try
        rv = trace!(t, ci, v_fargs...)
        t.tape.result = rv
        return t.tape[t.tape.result].val, t.tape
    catch
        LATEST_TRACER[] = t
        rethrow()
    end
end
