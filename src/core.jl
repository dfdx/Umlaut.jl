import Core: CodeInfo, SSAValue, SlotNumber
import Ghost: Tape, Variable, V, Input, Constant, Call, mkcall, Loop, play!, compile
import Ghost: PRIMITIVES, is_primitive, FunctionResolver, get_type_parameters
import Statistics, LinearAlgebra   # include primitives from these stanard modules



include("utils.jl")
include("trace.jl")
include("trace_cf.jl")