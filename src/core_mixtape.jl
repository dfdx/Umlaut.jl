import Ghost: Tape, Input, Call, Constant, mkcall
using Mixtape
import Mixtape: CompilationContext, transform, allow
using CodeInfoTools
using MacroTools


# Goal: create Tape with control flow
# Approaches:
# 1. CodeInfoTools + Mixtape
#   - fully static via CodeInfoTools.code_inferred; code may be transformed quite a lot
#   - translate to tape and execute command by command, keep ref to unevaluated branches in CodeInfo
# 2. Overload AbstractInterpeter



struct TracerMix <: CompilationContext
    tape::Tape
end

TracerMix() = TracerMix(Tape())

(mix::TracerMix)() = mix



swap(ctx, e) = e
function swap(ctx, e::Expr)
    new = MacroTools.postwalk(e) do s
        isexpr(s, :call) || return s
        push!(ctx.tape, mkcall(s.args...; val=nothing))
        s.args[1] == (*) || return s
        return Expr(:call, /, s.args[2:end]...)
    end
    return new
end


function transform(ctx::TracerMix, src)
    b = CodeInfoTools.Builder(src)
    for (v, st) in b
        b[v] = swap(ctx, st)
    end
    return CodeInfoTools.finish(b)
end


allow(ctx::TracerMix, m::Module) = m != Base && m != Core


Mixtape.@load_abi()


function simple(x)
    y = 4*x
    return y + 1
end

