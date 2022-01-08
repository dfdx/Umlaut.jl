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
            push!(v_fargs, v)
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
    frames::Vector{Frame}
    primitives   # TODO: make it more Ghost-like instead
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



function trace!(t::Tracer, ci::CodeInfo, v_fargs...)
    frame = Frame(ci, v_fargs...)
    for (i, st) in enumerate(ci.code)
        if Meta.isexpr(st, :call) || Meta.isexpr(st, :(=))
            sv = SSAValue(i)
            ex = Meta.isexpr(st, :(=)) ? st.args[2] : st
            vs = resolve_tape_vars(frame, ex.args...)
            fvals = [v isa V ? t.tape[v].val : v for v in vs]
            if fvals[1] in t.primitives
                v = push_call!(t.tape, vs...)
                frame.ir2tape[sv] = v
            else
                v = trace!(t, code_lowered(fvals[1], fvals[2:end])[1], vs...)
                frame.ir2tape[sv] = v
            end
        else
            @info "Ignoring statement $st for the moment"
        end
    end
    # for now assume the returned value is always the last recorded
    # we will update this when control flow instructions are implemented
    return V(t.tape[V(end)])
end


function trace(f, args...)
    ci = code_lowered(f, args)[1]
    t = Tracer(Tape(), [], Set([+, *]))
    v_fn = push!(t.tape, Input(f))
    v_args = [push!(t.tape, Input(a)) for a in args]
    v_fargs = [v_fn, v_args...]
    trace!(t, ci, v_fargs...)
    return t.tape
end


# v_xxx - Variable xxx
# sv_xxx - SSAValue xxx


############################################################################

mul(x, y) = x * y
linear(x, a, b) = mul(a, x) + b

pow(x, n) = for i=1:n x = x * x end


function cond1(x)
    y = 2x
    if x > 0
        y = 3x
    end
    return y
end


function while1(x)
    y = 2x
    while y > 0
        y -= 1
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


function main()
    f = linear
    args = [1.0, 2.0, 3.0]
    trace(f, args...)


    ci = @code_lowered cond1(2.0)
end