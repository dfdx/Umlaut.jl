# Umlaut.jl

> :warning: These docs are taken from Ghost.jl and will be updated to Umlaut.jl soon!


Umlaut.jl is a code tracer for the Julia programming language. It lets you trace the function execution, recording all primitive operations onto a linearized tape. Here's a quick example:


```@example
using Umlaut     # hide
inc(x) = x + 1
mul(x, y) = x * y
inc_double(x) = mul(inc(x), inc(x))

val, tape = trace(inc_double, 2.0)
```
The tape can then be analyzed, modified and even compiled back to a normal function. See the following sections for details.