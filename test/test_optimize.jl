@testset "optimize" begin
    tape = Tape()
    x, = inputs!(tape, 3.0)
    c = push!(tape, Constant(2.0))
    yx = push!(tape, mkcall(*, x, 2))
    yc = push!(tape, mkcall(*, c, 2))
    z = push!(tape, mkcall(+, yx, yc))
    tape.result = z
    res1 = play!(tape, 8.0)
    Umlaut.fold_constants!(tape)
    res2 = play!(tape, 8.0)
    @test res1 == res2
    @test tape[V(3)] isa Call       # no changes
    @test tape[V(4)] isa Constant   # converted to Constant
end