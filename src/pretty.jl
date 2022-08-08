mutable struct ShowConfig
    compact::Bool
    line::Bool
end

const SHOW_CONFIG = ShowConfig(false, false)




format_lineinfo(line::Core.LineInfoNode) = "$(line.module).$(line.method) at $(line.file):$(line.line)"
format_lineinfo(line) = line

# line_str = !isnothing(op.line) ? "\t\t# $(format_lineinfo(op.line))" : ""


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
                line_str = SHOW_CONFIG.line && !isnothing(op.line) ? "\t\t# $(format_lineinfo(op.line))" : ""
                println(io, "  $out_vars_str = [%$(op.id)] = $(op.fn)($arg_str) $line_str")
            else
                println(io, "  $op")
            end
        else
            println(io, "  $op")
        end
    end
end


function Base.show(io::IO, tape::Tape{C}, config) where C
    if config.compact
        show_compact(io, tape)
    else
        println(io, "Tape{$C}")
        for op in tape.ops
            println(io, "  ", op)
        end
    end
end