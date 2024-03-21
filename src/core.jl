Base.Experimental.@compiler_options optimize=0 compile=min infer=no

import Core: CodeInfo, SSAValue, SlotNumber, Argument
import Core: GotoNode, GotoIfNot, ReturnNode
import Core.Compiler: IRCode
import Statistics, LinearAlgebra   # include primitives from these standard modules
using ExprTools


include("utils.jl")
include("deprecated.jl")
include("tape.jl")
include("trace.jl")
include("optimize.jl")
include("compile.jl")
include("pretty.jl")