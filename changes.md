# Changes to `dga` version 1.2

## Summary of major changes

- Helper functions are no longer exported in the package namespace.
  - **WARNING:** This is an incompatible API change. Following semantic versioning, the update is therefore version 2.0.0
  - This change will ease maintenance of the package.
- The package now depends on Rcpp.

## Detailed changes summary

### Package metadata

- Reformat Autors field.
- Add Olivier Binette as a contributor
- Change version to 2.0.0
- Change date to 2020-10-05
- Add `NeedsCompilation: yes`
- Add `ByteCompile: yes`
- Add Rcpp to Imports 
- Add `LinkingTo: Rcpp, RcppArmadillo`


### Documentation

- Port documentation to Roxygen using `Rd2roxygen`.
  - Package documentation is in `R/dga-package.R`.
  - Data documentation is in `R/data.R`.

### Code

- Remove function `makeAdjMatrix`: unused and doesn't work as intended. Use instead the implementation below if needed:
```r
adjMat = function(graph, p) {
  mat = matrix(rep(0,p^2), nrow=p, ncol=p)
  for (clique in graph$C) {
    clique = c(clique)
    mat[clique, clique] = 1
  }
  return(mat - diag(1, p))
}
```

- Add `src` folder with Rcpp source and the implementation of the following functions:
  - `computeLogPostProbs` implements the loop over graphs of bma.cr in Rcpp.
  - `colAdd`, `rowAdd` and `expNormalize` are utility functions used for marginal memory and speed gains. Note that they perform **in-place** matrix modification. This can lead to unexpected behaviours if used incorrectly and is considered bad practice by some. Here it does not cause any issues.

- Modified the `CompLogML` function to be more efficient. It now takes the additional parameter `Nmssing`.

- Modified the `MakeCompMatrix` function to be more efficient in combination with `CompLogML`. This was one of the main bottlenecks together with the main graphs loop.
  
## Performance tips

- `rowSums` and `colSums` is much faster than `apply(., 1, sum)` and `apply(., 2, sum)`.
- R doesn't work efficiently with matrices. Things such as `t(t(mat) + vect)` to add a vector to rows can be a bit costly. Creating new matrices as part of data manipulation should be avoided.



