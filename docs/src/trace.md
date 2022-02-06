```@meta
CurrentModule = Umlaut
```

# Linearized traces

Usually, programs are executed as a sequence of nested function calls, e.g.:

```@example
foo(x) = 2x
bar(x, y) = foo(x) + 3y
baz(x, y) = bar(x, y) - 1

baz(1.0, 2.0)
```
Sometimes, however, it's more convenient to work with a linearized representation of the computation. Example use cases include collecting computational graphs for automatic differentiation, exporting to ONNX, serialization of functions to library-independent format, etc. [`trace()`](@ref) lets you obtain such a linearized representation:

```@example
foo(x) = 2x                # hide
bar(x, y) = foo(x) + 3y    # hide
baz(x, y) = bar(x, y) - 1  # hide

using Umlaut

val, tape = trace(baz, 1.0, 2.0)
```

[`trace()`](@ref) returns two values - the result of the original function call and the generated tape. The structure of the tape is described in [Tape anatomy](@ref) section, here just note that [`trace()`](@ref) recursed into `baz()`, `bar()` and `foo()`, but recorded `+`, `-` and `*` onto the tape as is. This is because `+`, `-` and `*` are considered "primitives", i.e. the most basic operations which all other functions consist of. This behavior can be customized using a tracing context.

## Context

Context is a way to **customize tracing** and **attach arbitrary data** to the generated tape. For example, here's how we can add a new function to the list of primitives:

```@example
foo(x) = 2x                # hide
bar(x, y) = foo(x) + 3y    # hide
baz(x, y) = bar(x, y) - 1  # hide

using Umlaut                # hide
import Umlaut: isprimitive, BaseCtx

struct MyCtx end

isprimitive(::MyCtx, f, args...) = isprimitive(BaseCtx(), f, args...) || f == foo

val, tape = trace(baz, 1.0, 2.0; ctx=MyCtx())
```

In this code:

* `MyCtx` is a new context type; there are no restrictions on the type of context
* [`isprimitive`](@ref) is a function that decides whether a particular function call
  `f(args...)` should be treated as a primitive in this context
* [`BaseCtx`](@ref) is the default context that treats all built-in functions from
  modules `Base`, `Core`, etc. as primitives

So we define a new method for [`isprimitive()`](@ref) that returns `true` for all built-in functions and for function `foo`.

[`isprimitive()`](@ref) can be artibtrarily complex. For example, if we want to include
all functions from a particular module, we can write:

```julia
isprimitive(::MyCtx, f, args...) = Base.parentmodule(f) == Main
```

On the other hand, if we only need to set a few functions as primitives,
[`BaseCtx()`](@ref) provides a convenient constructor for it:

```@example
foo(x) = 2x                # hide
bar(x, y) = foo(x) + 3y    # hide
baz(x, y) = bar(x, y) - 1  # hide

using Umlaut                # hide
import Umlaut: isprimitive, BaseCtx  # hide

val, tape = trace(baz, 1.0, 2.0; ctx=BaseCtx([+, -, *, foo]))
```

Another useful function is [`record_primitive!()`](@ref), which lets you overload the way
a primitive call is recorded to the tape. As a toy example, imagine that we want to replace
all invokations of `*` with `+` and calculate the number of times it has been called.
Even though we haven't learned tape anatomy and utils yet, try to parse this code:


```@example
using Umlaut     # hide
import Umlaut: record_primitive!

function loop1(a, n)
    a = 2a
    for i in 1:n
        a = a * n
    end
    return a
end

mutable struct CountingReplacingCtx
    replace::Pair
    count::Int
end

# v_fargs is a tuple of Variables or constant values, representing a function call
# that we are about to invoke (but haven't yet)
function record_primitive!(tape::Tape{CountingReplacingCtx}, v_fargs...)
    # tape.c refers to the provided context
    if v_fargs[1] == tape.c.replace[1]
        tape.c.count += 1
        return push!(tape, mkcall(tape.c.replace[2], v_fargs[2:end]...))
    else
        return push!(tape, mkcall(v_fargs...))
    end
end


_, tape = trace(loop1, 2.0, 3; ctx=CountingReplacingCtx((*) => (+), 0))
@assert tape.c.count == 4
@assert count(op -> op isa Call && op.fn == (+), tape) == 4
```

Although we could have done it as a postprocessing using [`replace!()`](@ref),
[`record_primitive!()`](@ref) has advantage of running _before_ the original function
is invoked and thus avoiding double calculation.