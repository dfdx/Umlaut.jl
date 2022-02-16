import Core: CodeInfo, SSAValue, SlotNumber
import Statistics, LinearAlgebra   # include primitives from these stanard modules


include("utils.jl")
include("deprecated.jl")
include("tape.jl")
include("trace.jl")
include("trace_cf.jl")
include("compile.jl")
include("pretty.jl")