import Ghost: Tape, Variable, V, Input, Constant, Call, mkcall, Loop, play!, compile
import Ghost: PRIMITIVES, is_primitive, FunctionResolver
import Core: CodeInfo, SSAValue, SlotNumber


include("utils.jl")
include("trace.jl")
include("trace_cf.jl")