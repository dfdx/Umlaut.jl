import Ghost: promote_const_value, __new__

# v_xxx - Variable xxx
# sv_xxx - SSAValue xxx


mutable struct Frame
    ir2tape::Dict{Any, Variable}
end


function Frame(tape::Tape, v_fargs...)
    ir2tape = Dict{Union{SSAValue, SlotNumber}, V}()
    for (i, v) in enumerate(v_fargs)
        if v isa V
            ir2tape[SlotNumber(i)] = v
        else
            c = push!(tape, Constant(promote_const_value(v)))
            ir2tape[SlotNumber(i)] = c
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
        # elseif sv isa GlobalRef
        #     push!(v_fargs, getfield(sv.mod, sv.name))
        # else
        #     # treat as constant value
        #     push!(v_fargs, sv)
        end
    end
    return v_fargs
end


mutable struct Tracer
    tape::Tape
    primitives::FunctionResolver
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
            # v = if fvals[1] in t.primitives
            sig = Tuple{map(typeof, fvals)...}
            v = if sig in t.primitives || fvals[1] === Base.Generator
                push_call!(t.tape, vs...)
            else
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
            v_rhs = resolve_tape_vars(frame, st.args[2])[1]
            v = if v_rhs isa V
                val = t.tape[v_rhs].val
                push!(t.tape, Constant(promote_const_value(val)))
            else
                # there's no assignment operator, so we simply update the mapping
                # for the LHS
                frame.ir2tape[sv] = v_rhs
                # push_call!(t.tape, identity, v_rhs)
            end
            frame.ir2tape[sv] = v
            frame.ir2tape[SSAValue(i)] = v
            i += 1
        elseif st isa SlotNumber
            # assignment
            sv = SSAValue(i)
            frame.ir2tape[sv] = frame.ir2tape[st]
            i += 1
        elseif st isa Core.GotoIfNot
            # conditional jump
            v_cond = frame.ir2tape[st.cond]
            # if not cond, set i to destination, otherwise step forward
            i = !t.tape[v_cond].val ? st.dest : i + 1
        elseif st isa Core.GotoNode
            # unconditional jump
            i = st.label
        elseif st isa Core.ReturnNode
            # return statement
            sv = st.val
            if sv isa SSAValue || sv isa SlotNumber
                return frame.ir2tape[sv]
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
    # for now assume the returned value is always the last recorded
    # we will update this when control flow instructions are implemented
    return V(t.tape[V(end)])
end


function trace(f, args...; primitives=PRIMITIVES)
    primitives = ensure_function_resolver(primitives)
    ci = get_code_info(f, args...)
    t = Tracer(Tape(), primitives)
    v_fn = push!(t.tape, Input(f))
    v_args = [push!(t.tape, Input(a)) for a in args]
    v_fargs = [v_fn, v_args...]
    rv = trace!(t, ci, v_fargs...)
    t.tape.result = rv
    return t.tape[t.tape.result].val, t.tape
end
