freeR: Miscellaneous functions in R
======================================================================

Installation
------------

The "freeR" R package can be installed from GitHub with:

``` r
# Run if you don't already have devtools installed
install.packages("devtools")

# Run once devtools is successfully installed
devtools::install_github("cfree14/freeR")
library(freeR)
```

Functions
---------

The package implements miscellaneous functions including functions to:

- Check the completeness of a dataframe: `?complete`
- Extract linear regression info: `?r2`, `?pval`, `?slope`
- Raise or lower a number to the nearest x: `?floor1`, `?ceiling1`
- Round and format number to specified decimal place: `?roundf`
- Make colors transparent: `?tcolor`
- Interpolate expanded color palettes: `?colorpal`
- Convert text to sentence case: `?sentcase`
- Check marine fish and invertebrate scientific names: `?check_names`
- Suggest correct scientific names for invalid fish and invert names: `?suggest_names`
- Retrieve life history trait predictions from FishLife: `?fishlife`




