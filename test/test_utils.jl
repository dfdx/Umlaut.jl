import Umlaut.module_of


Base.read(::Int, ::Int) = 42
struct MyType x end
struct MyTypeWithParams{T} x::T end

@testset "module_of" begin
    @test module_of(Base.not_int, 1) == Core
    @test module_of(broadcast, sin, [1.0, 2.0]) == Base.Broadcast
    @test module_of(read, stdout) == Base
    @test module_of(read, 1, 1) == @__MODULE__
    @test module_of(Int, 1) == Core
    @test module_of(Complex{Float64}, 1, 1) == Base
    @test module_of(MyType, 1) == @__MODULE__
    @test module_of(MyTypeWithParams{Int}, 1) == @__MODULE__
end

struct StructPartialInit
    x::Float64
    y::Float64
    StructPartialInit(x::Float64) = new(x)
end

@testset "__new__" begin
    @test first(trace(StructPartialInit, 5.0)).x == StructPartialInit(5.0).x
end
