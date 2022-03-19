# A couple of useful context types

struct BcastCtx
    inner
end


function record_or_recurse!(t::Tracer{BcastCtx}, vs...)
    fvals = [v isa V ? t.tape[v].val : v for v in vs]
    return if isprimitive(t.tape.c.inner, fvals...)
        push!(t.tape, mkcall(broadcast, vs...))
    else
        types = map(eltype, fvals[2:end])
        trace!(t, getcode_t(fvals[1], types...), vs...)
    end
end