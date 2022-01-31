import Ghost: promote_const_value, __new__

# v_xxx - Variable xxx
# sv_xxx - SSAValue xxx


mutable struct Frame
    # typically values are Variables, but can also be constant values
    ir2tape::Dict{Union{SSAValue, SlotNumber}, Any}
end


function Frame(tape::Tape, v_fargs...)
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
    return Frame(ir2tape)
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


mutable struct Tracer
    tape::Tape
    is_primitive::Function
end


function get_code_info(f, args...)
    types = map(typeof, args)
    cis = code_lowered(f, types)
    if isempty(cis)
        arg_type_str = join(types, ", ")
        error("Cannot get CodeInfo for $f($arg_type_str)")
    end
    return cis[1]
end


function get_code_info2(f, args...)
    types = map(typeof, args)
    mis = Base.method_instances(f, types)
    if isempty(mis)
        arg_type_str = join(types, ", ")
        error("Cannot get CodeInfo for $f($arg_type_str)")
    end
    m = first(mis).def
    ci = Base.uncompressed_ir(m)
    return ci
end


"""
    push_call!(tape::Tape, fn, args...; kwargs)

Shortcut for `push!(tape, mkcall(fn, args..))` also handling
keyword arguments and respecting `ONNXCtx.exec` setting.
"""
function push_call!(tape::Tape, fn, args...; kwargs...)
    kwargs = NamedTuple(kwargs)
    if !isempty(kwargs)
        args = (kwargs, fn, args...)
        fn = Core.kwfunc(fn)
    end
    # op = tape.c.exec ? mkcall(fn, args...) : mkcall(fn, args...; val=nothing)
    op = mkcall(fn, args...)
    return push!(tape, op)
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
    is_primitive(sig)

The default implementation of `is_primitive` argument in [`trace()`](@ref).
Returns `true` if the method with the provided signature is defined
in one of the Julia's built-in modules, e.g. `Base`, `Core`, `Broadcast`, etc.
"""
function is_primitive(sig)
    FT = get_type_parameters(sig)[1]
    FT in (typeof(__new__), Colon) && return true
    FT <: NamedTuple && return true
    FT <: DataType && return false  # usually we want to recurse to constructors
    modl = parentmodule(FT)
    modl in (Base, Core, Core.Intrinsics, Broadcast, Statistics, LinearAlgebra) && return true
    return false
end


function is_special_primitive(f)
    return (
        f === Base.Generator
    )
end


"""
    record_primitive!(tape::Tape, v_fargs...)

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
"""
record_primitive!(tape::Tape, v_fargs...) = push_call!(tape, v_fargs...)


function trace!(t::Tracer, ci::CodeInfo, v_fargs...)
    frame = Frame(t.tape, v_fargs...)
    i = 1
    while i <= length(ci.code)
        st = rewrite_special_cases(ci.code[i])
        global STATE = (t, ci, v_fargs, frame, i, st)
        if Meta.isexpr(st, :call) || (Meta.isexpr(st, :(=)) && Meta.isexpr(st.args[2], :call))
            # function call
            sv = SSAValue(i)
            ex = Meta.isexpr(st, :(=)) ? st.args[2] : st
            vs = resolve_tape_vars(frame, ex.args...)
            fvals = [v isa V ? t.tape[v].val : v for v in vs]
            # fvals[1] == NamedTuple{(:dims,)} && error("STOP")
            sig = Tuple{map(typeof, fvals)...}
            v = if t.is_primitive(sig) || is_special_primitive(fvals[1])
                # push_call!(t.tape, vs...)
                record_primitive!(t.tape, vs...)
            else
                # println("diving into $(fvals)")
                trace!(t, get_code_info(fvals[1], fvals[2:end]...), vs...)
            end
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
                return val isa V ? val : push!(t.tape, Constant(promote_const_value(val)))
            else
                v = push!(t.tape, Constant(promote_const_value(sv)))
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
    # if no ReturnNode was encountered, use last op on the tape
    return V(t.tape[V(end)])
end


"""
    trace(f, args...; is_primitive, primitives)

Trace function call, produce call value and a Tape.
`trace` records to the tape primitive methods and recursively dives into
non-primitives. There are 2 ways to tell `trace` that a particular method
is a primitive:

* provide `is_primitive(sig) -> Bool` function, where `sig` is
    is a method signature, e.g. `Tuple{typeof(f), map(typeof, args)...}`
* provide an iterable `primitives`; in this case `trace` matches
    all methods of this function
"""
function trace(f, args...; ctx=Dict(), is_primitive=is_primitive, primitives=nothing)
    # primitives = ensure_function_resolver(primitives)
    if primitives !== nothing
        sigs = FunctionResolver{Bool}([Tuple{typeof(f), Vararg} => true for f in primitives])
        is_primitive = sig -> sig in sigs
    end
    ci = get_code_info(f, args...)
    meth = which(f, map(typeof, args))
    # xargs are here to support vararg inputs
    xargs = meth.isva ? (args[1:meth.nargs - 2]..., args[meth.nargs - 1:end]) : args
    t = Tracer(Tape(ctx), is_primitive)
    t.tape.meta[:isva] = meth.isva
    v_fn = push!(t.tape, Input(f))
    v_args = [push!(t.tape, Input(a)) for a in xargs]
    v_fargs = [v_fn, v_args...]
    rv = trace!(t, ci, v_fargs...)
    t.tape.result = rv
    return t.tape[t.tape.result].val, t.tape
end
