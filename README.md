# Ümlaut

<p align="center">
  <img height="300" src="https://github.com/dfdx/Umlaut.jl/blob/main/docs/src/logo.png">
</p>

[![](https://img.shields.io/badge/docs-dev-blue.svg)](https://dfdx.github.io/Umlaut.jl/dev)
[![Test](https://github.com/dfdx/Umlaut.jl/actions/workflows/test.yml/badge.svg)](https://github.com/dfdx/Umlaut.jl/actions/workflows/test.yml)

Umlaut.jl is a code tracer for the Julia programming language. It lets you trace the function execution, recording all primitive operations onto a linearized tape.


> :point_right: Umlaut.jl was started as a fork of Ghost.jl trying to overcome some of its
> limitations, but eventually the codebase has diverged so much that the new package was
> born. Although the two have pretty similar API, there are several notable differences.
> See [Migration from Ghost](https://dfdx.github.io/Umlaut.jl/dev/ghost/) for details.