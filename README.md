# Ümlaut

<p align="center">
  <img height="300" src="https://github.com/dfdx/Umlaut.jl/blob/main/docs/src/logo.png">
</p>

[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://dfdx.github.io/Umlaut.jl/dev)
[![Test](https://github.com/dfdx/Umlaut.jl/actions/workflows/test.yml/badge.svg)](https://github.com/dfdx/Umlaut.jl/actions/workflows/test.yml)

Umlaut is a new experimental code tracer for [Ghost.jl](https://github.com/dfdx/Ghost.jl). The main goals of Umlaut are:

* avoid dependency on unmaintained packages like IRTools
* complete support for control flow (e.g. `if`s, loops)


At the moment Umlaut completely replicates static tracer interface in Ghost and can be used as drop-in replacement for scanirios without or with unrolled control flow.

Note that right now Umlaut is highly experimental, and in general you should use Ghost instead.

!!! note

    Umlaut.jl was started as a fork of Ghost.jl trying to overcome some of its
    limitations, but eventually the codebase has diverged so much that the new package was born. Although the two have pretty similar API, there are several notable differences.
    See [Migration from Ghost](@ref) for details.