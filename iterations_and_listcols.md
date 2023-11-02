Iterations and Listcols
================
Christina Dai

## Lists

You can put anything in a list

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)
```

``` r
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.2147 -0.4942  0.1139  0.1089  0.6915  2.4016

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## `for` loop

Create a new list.

``` r
list_norm = 
  list(
    a = rnorm(20, mean = 3, sd = 1),
    b = rnorm(30, mean = 0, sd = 5),
    c = rnorm(40, mean = 10, sd = 2),
    d = rnorm(20, mean = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 2.379633 3.042116 2.089078 3.158029 2.345415 4.767287 3.716707 3.910174
    ##  [9] 3.384185 4.682176 2.364264 2.538355 4.432282 2.349304 2.792619 2.607192
    ## [17] 2.680007 2.720887 3.494188 2.822670
    ## 
    ## $b
    ##  [1] -2.5297873  6.7151941 -1.0728970 -0.8977827 -0.5009537  3.5633315
    ##  [7] -0.3678220 -0.1881709 -3.4083024 -1.6213514  0.3008022 -2.9444724
    ## [13]  2.6574810 -7.5919704  1.5327893 -7.6822491 -1.5048806 -2.6413995
    ## [19] -3.2604739 -0.2844839 -9.5717971  5.8829166 -8.3248622 -2.3176520
    ## [25] -5.5796005 -3.7540950 10.4358327  0.0869781 -6.4315027 -8.2030277
    ## 
    ## $c
    ##  [1] 10.900374  9.962880  9.363863  8.141276  7.025079  7.849615 12.000058
    ##  [8]  8.757467  7.231146 13.738581 10.850201  9.522706 12.116966 11.772845
    ## [15]  8.761514 14.412205  9.489946  7.151011  9.711201 10.415077 14.615957
    ## [22] 10.211605 10.913998  9.845694  9.331998  9.930548 11.575279 14.150490
    ## [29] 12.054785 12.415817  7.537353 11.967791 10.439850  7.065500 11.042045
    ## [36]  9.682491 12.929175  8.467836  9.139576  8.147781
    ## 
    ## $d
    ##  [1] -3.1771040 -2.5979882 -3.7317482 -2.1696268 -4.2080828 -4.0479844
    ##  [7] -1.5588423 -4.0158475 -2.5880253 -3.3810761 -2.5905982 -1.3111267
    ## [13] -1.4134116 -3.3309078 -5.2852355 -0.5023384 -2.3329338 -2.4586727
    ## [19] -3.0133995 -2.4898916

Pause and get my old function.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

I can apply that function to each list element.

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.11 0.814

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.65  4.61

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.3  2.08

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.81  1.14

Lets use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
output[[i]] = mean_and_sd(list_norm[[i]])

}
```

## Let’s try map

``` r
output = map(list_norm, mean_and_sd)
```

What if you want a different function?

``` r
output = map(list_norm, median)
```