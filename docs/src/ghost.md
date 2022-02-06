```@meta
CurrentModule = Umlaut
```
# Migration from Ghost

The default context is now [`BaseCtx`](@ref) instead of just `Dict{Any,Any}()`. `BaseCtx`
can still be used as key-value storage, but allows more fine-grained control over tracing.

Keyword arguments `primitives` and `is_primitive` to [`trace`](@ref) have been replaced
with [`isprimitive(ctx, f, args...)`](@ref isprimitive) function. As a convenient shortcut,
one can set a list of functions as primitives using `trace(f, args; ctx=BaseCtx(MY_PRIMITIVES))`.

Additionally, [`record_primitive!()`](@ref) has been introduced.

`using Umlaut` imports much more stuff than `using Ghost`. In particular, you don't need to explicitely import
things like `Tape`, `Call`, `Variable`, etc. You still need to import `V` (or alias `const V = Variable`) though.