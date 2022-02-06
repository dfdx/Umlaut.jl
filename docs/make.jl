using Documenter
using Umlaut

makedocs(
    sitename = "Umlaut",
    format = Documenter.HTML(),
    modules = [Umlaut],
    pages = [
        "Main" => "index.md",
        "Linearized traces" => "trace.md",
        "Tape anatomy" => "tape.md",
        "Loops" => "loops.md",
        "Differences from Ghost" => "ghost.md",
        "Reference" => "reference.md",
    ],
)

deploydocs(
    repo = "github.com/dfdx/Umlaut.jl.git",
    devbranch = "main",
)