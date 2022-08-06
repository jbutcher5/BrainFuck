# BrainFuck

An implementation of the popular [BrainFuck](https://esolangs.org/wiki/Brainfuck) esoteric programming language in [Haskell](https://www.haskell.org/). The compiler outputs x86_64 Intel assembly that is designed to be assembled with the [yasm](https://yasm.tortall.net/) assembler. The compiler produces assembly that isn't fully optimised but we perform optimisations such as [constant folding](https://en.wikipedia.org/wiki/Constant_folding) by taking the summation of consecutive '+'/'-' or '<'/'>' as to cut down on lines of code and improve performance ever so slightly.
