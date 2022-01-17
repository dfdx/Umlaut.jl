ensure_function_resolver(res::FunctionResolver) = res

function ensure_function_resolver(primitives)
    sigs = [p isa Function ? Tuple{typeof(p), Vararg} : p for p in primitives]
    return FunctionResolver{Bool}([sig => true for sig in sigs])
end