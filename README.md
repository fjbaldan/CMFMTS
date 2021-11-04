# [CMFMTS](https://fjbaldan.github.io/CMFMTS)
R package for multivariate time series features extraction

# Installation

```
# Install from GitHub using devtools package
devtools::install_github("fjbaldan/CMFMTS")
```


# Examples

```
# Examples of use
scmfts::scmfts(list(ts(c(1,2,3,4,5),frequency = 5)))
scmfts::scmfts(list(ts(sample(1:50,size = 100, replace = T),frequency = 5)))
```
