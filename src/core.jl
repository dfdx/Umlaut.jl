import Ghost: Tape, Variable, V, Input, Constant, Call, mkcall, PRIMITIVES, FunctionResolver
import Core: CodeInfo, SSAValue, SlotNumber


include("utils.jl")
include("trace.jl")
include("trace_cf.jl")