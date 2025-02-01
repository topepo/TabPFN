---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# TabPFN

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/TabPFN)](https://CRAN.R-project.org/package=TabPFN)
<!-- badges: end -->

The goal of TabPFN is to ...

## Installation

You can install the development version of TabPFN like so:


``` r
require(pak)
pak(c("topepo/TabPFN"), ask = FALSE)
```

## Example


The package requires a virtual environment to be created and registered with reticulated. If you don't have one, you can create one. First, load the reticulate package: 


``` r
require(reticulate)
```

and this code can be used to create an enironment and install the relavant packages: 


``` r
virtualenv_create(
	"r-tabpfn",
	packages = c("numpy", "tabpfn"),
	python_version = "<3.12"
)
```

then tell reticulate to use it: 


``` r
use_virtualenv("~/.virtualenvs/r-tabpfn")
#> Error in use_python(python, required = required): failed to initialize requested version of Python
```

On starting the TabPFN, it will see if the python packages are installed. 


``` r
library(TabPFN)
```

There is also a function to check too: 


``` r
TabPFN:::check_py_packages()
```

To fit a model: 


``` r
reg_mod <- TabPFN(mtcars[1:25, -1], mtcars$mpg[1:25])
reg_mod
#> TabPFN Regression Model
#> Training set
#> ℹ 25 data points
#> ℹ 10 predictors
```

In addition to the x/y interface shown above, there are also formula and recipes interfaces. 

Prediction follows the usual S3 `predict()` method: 


``` r
predict(reg_mod, mtcars[26:32, -1])
#> # A tibble: 7 × 1
#>   .pred
#>   <dbl>
#> 1  31.2
#> 2  23.7
#> 3  25.5
#> 4  14.9
#> 5  19.2
#> 6  13.9
#> 7  22.6
```

While TabPFN isn’t a tidymodels package, it follows their prediction convention: a data frame is always returned with a standard set of column names. 

For a classification model, the outcome should always be a factor vector. For example, using these data from the modeldata package: 


``` r
require(modeldata)
require(ggplot2)

two_cls_train <- parabolic[1:400,  ]
two_cls_val   <- parabolic[401:500,]
grid <- expand.grid(X1 = seq(-5.1, 5.0, length.out = 25), 
                    X2 = seq(-5.5, 4.0, length.out = 25))

cls_mod <- TabPFN(class ~ ., data = two_cls_train)

grid_pred <- predict(cls_mod, grid)
grid_pred
#> # A tibble: 625 × 3
#>    .pred_Class1 .pred_Class2 .pred_class
#>           <dbl>        <dbl> <chr>      
#>  1        0.988      0.0116  Class1     
#>  2        0.991      0.00850 Class1     
#>  3        0.994      0.00647 Class1     
#>  4        0.994      0.00571 Class1     
#>  5        0.993      0.00677 Class1     
#>  6        0.989      0.0107  Class1     
#>  7        0.975      0.0246  Class1     
#>  8        0.928      0.0719  Class1     
#>  9        0.830      0.170   Class1     
#> 10        0.628      0.372   Class1     
#> # ℹ 615 more rows
```

The fit looks fairly good when shown with out-of-sample data: 


``` r
cbind(grid, grid_pred) |>
 ggplot(aes(X1, X2)) + 
 geom_point(data = two_cls_val, aes(col = class, pch = class), 
            alpha = 3 / 4, cex = 3) +
 geom_contour(aes(z = .pred_Class1), breaks = 1/ 2, col = "black", linewidth = 1) +
 coord_equal(ratio = 1)
```

<div class="figure" style="text-align: center">
<img src="man/figures/README-boundaries-1.png" alt="plot of chunk boundaries" width="50%" />
<p class="caption">plot of chunk boundaries</p>
</div>

## Code of Conduct
  
Please note that the TabPFN project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.
