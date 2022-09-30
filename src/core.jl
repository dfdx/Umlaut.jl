import Core: CodeInfo, SSAValue, SlotNumber
import Statistics, LinearAlgebra   # include primitives from these stanard modules
using CompilerPluginTools


include("utils.jl")
include("deprecated.jl")
include("tape.jl")
include("trace.jl")
include("optimize.jl")
include("compile.jl")
include("pretty.jl")