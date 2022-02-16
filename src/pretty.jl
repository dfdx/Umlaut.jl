function show_compact(io::IO, tape::Tape{C}) where C
    println(io, "Tape{$C}")
    dont_show = Set([])
    for op in tape
        if in(op.id, dont_show)
            continue
        elseif op isa Call && op.val isa Tuple
            out_vars = []
            destructured = false
            for i=1:length(op.val)
                var = "_"
                for id=(op.id + 1:length(tape))
                    opc = tape[V(id)]
                    if opc isa Call &&
                            (opc.fn == getfield || Symbol(opc.fn) == :_getfield) &&
                            opc.args[1].id == op.id && opc.args[2] == i
                        var = "%$(opc.id)"
                        push!(dont_show, opc.id)
                        destructured = true
                        break
                    end
                end
                push!(out_vars, var)
            end
            if destructured
                out_vars_str = join(out_vars, ", ")
                arg_str = join(op.args, ", ")
                println(io, "  $out_vars_str = [%$(op.id)] = $(op.fn)($arg_str)")
            else
                println(io, "  $op")
            end
        else
            println(io, "  $op")
        end
    end
end
