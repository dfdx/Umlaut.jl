struct Fragment
    ci::CodeInfo
    from::Int
    to::Int
end


struct IfSpec
    cond::SSAValue
    true_block::Fragment
    false_block::Fragment
end


# TODO: imagine we have complete IfSpecs and LoopSpecs, so what? how are we going to use them?

function detect_ifs(ci::CodeInfo)
    ifs = []
    for (i, st) in enumerate(ci.code)
        if st isa Core.GotoIfNot
            j = i + 1
            while j < length(ci.code) && (ci.code[j] isa Core.GotoIfNot || ci.code[j] isa Core.ReturnNode)
                j += 1
            end
            if_spec = IfSpec(st.cond, Fragment(ci, st.cond.id, j), nothing)
        end
    end

end


"""
Same as trace!(), but tries to parse control flow constructs as well
"""
function trace_cf!(t::Tracer, ci::CodeInfo, v_fargs...)
    # TODO
end
