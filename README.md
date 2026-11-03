# fitminimizers

The downhill simplex optimisation algorithm for Delphi and Lazarus, in two
variants — the classical one and a version with simulated annealing — packaged so
that the algorithm knows nothing about the problem it is solving. The host
supplies the parameters and evaluates the goal function.

**[dvmorozov.github.io/fitminimizers](https://dvmorozov.github.io/fitminimizers/)**
— what each component does, what state it is in, and the class diagram.

## Using it

The package is `package/FitMinimizers.lpk` for Lazarus and
`package/FitMinimizers.dpk` for Delphi. Open it in the IDE and compile.

`examples/` solves the minimum bounding box problem three ways — through an
interface, through a form component on a background thread, and from the console.

Written for [Fit](https://dvmorozov.github.io/fit/), and used by
[MotifMASTER](https://dvmorozov.github.io/motifmaster/) as well.

## Cost of a step

A step moves one vertex, and the simplex's centre - the sum of every parameter
over every vertex - is updated by that vertex's difference, so a step costs about
N numbers for N parameters, beside the host's goal function. It used to add up
every vertex again, N x N, which dominated models with over a thousand
parameters. The running sum differs from a fresh one by rounding (about 1e-16 of
a term per step); it is computed whole whenever the simplex is built or shrunk.

## License

MPL-2.0 - see [LICENSE](LICENSE). Every source file has said so since 2019.
The Mozilla Public License is file-level: a program under any licence, free or
commercial, may use this package, and changes to its own files stay under MPL-2.0
with their source available. [Fit](https://github.com/dvmorozov/fit), the
application it was written for, is licensed separately, under GPL-3.0-or-later.
