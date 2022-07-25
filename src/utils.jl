"""
    __new__(T, args...)
User-level version of the `new()` pseudofunction.
Can be used to construct most Julia types, including structs
without default constructors, closures, etc.
"""
@generated function __new__(T, args...)
    return Expr(:splatnew, :T, :args)
end


"""
Unwrap constant value from its expression container such as
GlobalRef, QuoteNode, etc. No-op if there's no known container.
"""
promote_const_value(x::QuoteNode) = x.value
promote_const_value(x::GlobalRef) = getproperty(x.mod, x.name)
promote_const_value(x) = x


function module_of(f, args...)
    if f isa Core.IntrinsicFunction || f isa Core.Builtin
        # may be actually another built-in module, but it's ok for our use case
        return Core
    else
        types = map(Core.Typeof, args)
        return which(f, types).module
    end
end


function flatten(xs)
    res = []
    for x in xs
        push!(res, x...)
    end
    return res
end