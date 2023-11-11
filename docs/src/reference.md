```@meta
CurrentModule = Umlaut
```

## Public API

### Tracing

```@docs
trace
isprimitive
record_primitive!
BaseCtx
__new__
```

### Variables

```@docs
Variable
bound
rebind!
rebind_context!
```

### Tape structure

```@docs
Tape
AbstractOp
Input
Constant
Call
Loop
inputs
inputs!
mkcall
```

### Tape transformations

```@docs
push!
insert!
replace!
deleteat!
primitivize!
```

## Tape execution

```@docs
play!
compile
to_expr
```

## Internal functions

```@docs
code_signature
call_signature
trace!
trace_call!
unsplat!
map_vars
block_expressions
promote_const_value
loop_exit_vars_at_point
handle_gotoifnot_node!
is_ho_tracable
__foreigncall__
UncalculatedValue
```

## Index

```@index
```