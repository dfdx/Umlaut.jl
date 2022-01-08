`if` condition:

```julia
function cond1(x)
    y = 2x
    if x > 0
        y = 3x
    end
    return y
end
```

```
CodeInfo(
1 ─      y = 2 * x
│   %2 = x > 0
└──      goto #3 if not %2
2 ─      y = 3 * x
3 ┄      return y
)
```

`while` loop:

```julia
function while1(x)
    y = 2x
    while y > 0
        y -= 1
    end
    return y
end
```

```
CodeInfo(
1 ─      y = 2 * x
2 ┄ %2 = y > 0
└──      goto #4 if not %2
3 ─      y = y - 1
└──      goto #2
4 ─      return y
)
```

loop with `continue`:

```julia
function while_continue(x)
    y = 3x
    while y > 0
        if y < x
            continue
        end
        y -= 1
    end
    return y
end
```

```
CodeInfo(
1 ─      y = 3 * x
2 ┄ %2 = y > 0
└──      goto #7 if not %2
3 ─ %4 = y < x
└──      goto #5 if not %4
4 ─      goto #6
5 ─      y = y - 1
6 ┄      goto #2
7 ─      return y
)
```

loop with `break`:

```julia
function while_break(x)
    y = 3x
    while y > 0
        if y < x
            break
        end
        y -= 1
    end
    return y
end
```

```
CodeInfo(
1 ─      y = 3 * x
2 ┄ %2 = y > 0
└──      goto #6 if not %2
3 ─ %4 = y < x
└──      goto #5 if not %4
4 ─      goto #6
5 ─      y = y - 1
└──      goto #2
6 ┄      return y
)
```
