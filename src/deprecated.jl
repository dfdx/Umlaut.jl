function warn_deprecated_keywords(deprecated_kws)
    for k in keys(deprecated_kws)
        if k == :primitives
            @warn "`primitives` keyword argument is deprecated, use `ctx=BaseCtx(primitives)` instead"
        elseif k == :is_primitive
            @warn ("`is_primitive` keyword argument is deprecated, define a custom context and implement " *
                   "`isprimitive(ctx, f, args...)` instead")
        else
            @warn "Unknown keyword argument: $k"
        end
    end
end