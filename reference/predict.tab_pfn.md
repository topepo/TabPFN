# Predict using `TabPFN`

Predict using `TabPFN`

## Usage

``` r
# S3 method for class 'tab_pfn'
predict(object, new_data, ...)

# S3 method for class 'tab_pfn'
augment(x, new_data, ...)
```

## Arguments

- object, x:

  A `tab_pfn` object.

- new_data:

  A data frame or matrix of new predictors.

- ...:

  Not used, but required for extensibility.

## Value

[`predict()`](https://rdrr.io/r/stats/predict.html) returns a tibble of
predictions and
[`augment()`](https://generics.r-lib.org/reference/augment.html) appends
the columns in `new_data`. In either case, the number of rows in the
tibble is guaranteed to be the same as the number of rows in `new_data`.

For regression data, the prediction is in the column `.pred`. For
classification, the class predictions are in `.pred_class` and the
probability estimates are in columns with the pattern `.pred_{level}`
where `level` is the levels of the outcome factor vector.

## Examples

``` r
car_train <- mtcars[ 1:20,   ]
car_test  <- mtcars[21:32, -1]

# Fit
if (is_tab_pfn_installed()) {
 mod <- tab_pfn(mpg ~ cyl + log(drat), car_train)

 # Predict
 predict(mod, car_test)
 augment(mod, car_test)
}
#> # A tibble: 12 × 11
#>    .pred   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  25.5     4 120.     97  3.7   2.46  20.0     1     0     3     1
#>  2  12.5     8 318     150  2.76  3.52  16.9     0     0     3     2
#>  3  15.3     8 304     150  3.15  3.44  17.3     0     0     3     2
#>  4  16.5     8 350     245  3.73  3.84  15.4     0     0     3     4
#>  5  15.1     8 400     175  3.08  3.84  17.0     0     0     3     2
#>  6  27.4     4  79      66  4.08  1.94  18.9     1     1     4     1
#>  7  29.3     4 120.     91  4.43  2.14  16.7     0     1     5     2
#>  8  25.5     4  95.1   113  3.77  1.51  16.9     1     1     5     2
#>  9  17.3     8 351     264  4.22  3.17  14.5     0     1     5     4
#> 10  20.4     6 145     175  3.62  2.77  15.5     0     1     5     6
#> 11  16.5     8 301     335  3.54  3.57  14.6     0     1     5     8
#> 12  27.6     4 121     109  4.11  2.78  18.6     1     1     4     2
```
