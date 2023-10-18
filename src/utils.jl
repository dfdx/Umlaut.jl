macro __splatnew__(T, args)
    esc(Expr(:splatnew, T, args))
end

"""
    __new__(T, args...)
User-level version of the `new()` pseudofunction.
Can be used to construct most Julia types, including structs
without default constructors, closures, etc.
"""
@generated __new__(T, x...) = Expr(:new, :T, map(n -> :(x[$n]), 1:length(x))...)

# Special case. NamedTuple attempts to do some conversion if you don't provide the correct
# types, which means that it must be special-cased.
__new__(::Type{<:NamedTuple{names, T}}, x...) where {names, T} = NamedTuple{names, T}(x)

__splatnew__(T, t) = __new__(T, t...)

"""
    function __foreigncall__(
        ::Val{name}, ::Val{RT}, AT::Tuple, ::Val{nreq}, ::Val{calling_convention}, x...
    ) where {name, RT, nreq, calling_convention}

:foreigncall nodes get translated into calls to this function.
For example,
```julia
Expr(:foreigncall, :foo, Tout, (A, B), nreq, :ccall, args...)
```
becomes
```julia
__foreigncall__(Val(:foo), Val(Tout), (Val(A), Val(B)), Val(nreq), Val(:ccall), args...)
```
Please consult the Julia documentation for more information on how foreigncall nodes work,
and consult this package's tests for examples.
"""
@generated function __foreigncall__(
    ::Val{name}, ::Val{RT}, AT::Tuple, ::Val{nreq}, ::Val{calling_convention}, x...
) where {name, RT, nreq, calling_convention}
    return Expr(
        :foreigncall,
        QuoteNode(name),
        :($(RT)),
        Expr(:call, :(Core.svec), map(__get_arg_type, AT.parameters)...),
        :($nreq),
        QuoteNode(calling_convention),
        map(n -> :(x[$n]), 1:length(x))...,
    )
end

__get_arg_type(::Type{Val{T}}) where {T} = T

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
        append!(res, x)
    end
    return res
end
