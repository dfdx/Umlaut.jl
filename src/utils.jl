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


module_of(f) = parentmodule(typeof(f))
module_of(f::Function) = parentmodule(f)
module_of(f::Type{T}) where T = parentmodule(T)