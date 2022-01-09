using CodeInfoTools
import Ghost: Tape, Variable, V, Input, Constant, Call, mkcall
import Core: CodeInfo, SSAValue, SlotNumber


mutable struct Frame
    ir2tape::Dict{Any, Variable}
end


function Frame(ci::CodeInfo, v_fargs...)
    ir2tape = Dict(SlotNumber(i) => v_fargs[i] for i=1:length(v_fargs) if v_fargs[i] isa V)
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
        elseif sv isa GlobalRef
            push!(v_fargs, getfield(sv.mod, sv.name))
        else
            # treat as constant value
            push!(v_fargs, sv)
        end
    end
    return v_fargs
end


struct Fragment
    ci::CodeInfo
    from::Int
    to::Int
end


struct IfSpec
    cond::SSAValue
    true_block::Fragment
    false_block::Fragment
end


mutable struct Tracer
    tape::Tape
    primitives   # TODO: make it more Ghost-like instead
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



# TODO: imagine we have complete IfSpecs and LoopSpecs, so what? how are we going to use them?

function detect_ifs(ci::CodeInfo)
    ifs = []
    for (i, st) in enumerate(ci.code)
        if st isa Core.GotoIfNot
            j = i + 1
            while j < length(ci.code) && (ci.code[j] isa Core.GotoIfNot || ci.code[j] isa Core.ReturnNode)
                j += 1
            end
            if_spec = IfSpec(st.cond, Fragment(ci, st.cond.id, j), nothing)
        end
    end

end


# macro debug_state(vars, expr)
#     try  # todo: turn into a macro
#         push_call!(t.tape, vs...)
#     catch
#         global STATE = (t, ci, v_fargs, frame, i, st)
#         rethrow()
#     end
# end


function trace!(t::Tracer, ci::CodeInfo, v_fargs...)
    frame = Frame(ci, v_fargs...)
    i = 1
    while i <= length(ci.code)
        st = ci.code[i]
        global STATE = (t, ci, v_fargs, frame, i, st)
        # @show i, st
        if Meta.isexpr(st, :call) || (Meta.isexpr(st, :(=)) && Meta.isexpr(st.args[2], :call))
            # function call
            sv = SSAValue(i)
            ex = Meta.isexpr(st, :(=)) ? st.args[2] : st
            vs = resolve_tape_vars(frame, ex.args...)
            fvals = [v isa V ? t.tape[v].val : v for v in vs]
            v = if fvals[1] in t.primitives
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
                push!(t.tape, Constant(val))
            else
                # there's no assignment operator, so we record a dummy call to identity
                # would it be safe to simply update mapping ir2tape[sv] = v_rhs?
                push_call!(t.tape, identity, v_rhs)
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
                v = push!(t.tape, Constant(sv))
                return v
            end
        else
            error("Unexpected statement type in CodeInfo: $st")
        end
    end
    # for now assume the returned value is always the last recorded
    # we will update this when control flow instructions are implemented
    return V(t.tape[V(end)])
end


"""
Same as trace!(), but tries to parse control flow constructs as well
"""
function trace_cf!(t::Tracer, ci::CodeInfo, v_fargs...)
    # TODO
end


function trace(f, args...)
    ci = get_code_info(f, args...)
    t = Tracer(Tape(),
        Set([+, -, *, /, <, <=, >, >=, ==, ===, !=, !==, Colon(),
            Base.iterate, Base.not_int, Core.getfield,
            Base.broadcasted, Base.materialize,
            sin, cos, tanh]))
    v_fn = push!(t.tape, Input(f))
    v_args = [push!(t.tape, Input(a)) for a in args]
    v_fargs = [v_fn, v_args...]
    rv = trace!(t, ci, v_fargs...)
    t.tape.result = rv
    return t.tape[t.tape.result].val, t.tape
end


# v_xxx - Variable xxx
# sv_xxx - SSAValue xxx


############################################################################


function main()
    f = linear
    args = [1.0, 2.0, 3.0]
    trace(f, args...)


    f = while1
    args = (2.0,)
    tp = trace(f, args...)


    ci = @code_lowered cond1(2.0)
end



mul(x, y) = x * y
linear(x, a, b) = mul(a, x) + b

function pow(x, n)
    # TODO: broken
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
    y = 3x
    while y > 0
        if y < x
            continue
        end
        y -= 1
    end
    return y
end


function while_break(x)
    y = 3x
    while y > 0
        if y < x
            break
        end
        y -= 1
    end
    return y
end


