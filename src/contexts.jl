# A couple of useful context types

struct BcastCtx
    inner
end


function trace_call!(t::Tracer{BcastCtx}, vs...)
    fvals = [v isa V ? t.tape[v].val : v for v in vs]
    return if isprimitive(t.tape.c.inner, fvals...)
        push!(t.tape, mkcall(broadcast, vs...))
    else
        types = map(eltype, fvals[2:end])
        # TODO: handle varargs
        trace!(t, getcode(fvals[1], types), vs...)
    end
end