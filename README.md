# [hexy-foundation][]

Fast, flexible hexadecimal pretty-printing.  This repo is for the bonus stage in the final entry of the [Writing Performant Haskell][] blog post series.  It uses the `foundation` package instead of `text` to enable readers to compare implementations between [hexy-foundation][] and [hexy][].

``` sh
# Build the project.
stack build

# Run the test suite.
stack test

# Run the benchmarks.
stack bench

# Generate documentation.
stack haddock
```

[hexy]: https://github.com/jship/hexy
[hexy-foundation]: https://github.com/jship/hexy-foundation
[Writing Performant Haskell]: https://jship.github.io/tags/Hexy%20Tutorial.html
