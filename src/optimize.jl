function is_const_call(tape::Tape, op::Call)
    for v in [op.fn, op.args...]
        if v isa V && !isa(tape[v], Constant)
            return false
        end
    end
    return true
end
is_const_call(tape::Tape, op) = false


function fold_constants!(tape::Tape)
    for (i, op) in enumerate(tape)
        if is_const_call(tape, op)
            # replace with constant
            line = "folded from $(op.fn)($(join(op.args, ", ")))"
            tape[V(i)] = Constant(tape[V(i)].val; line)
        end
    end
    return tape
end
