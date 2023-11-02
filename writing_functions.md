Writing Functions
================
Christina Dai

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.46457181 -2.94529937  0.58700242 -0.26607221 -0.84653988  0.65033163
    ##  [7]  0.06567186  0.03067863  1.51170483 -1.30684019 -0.44851447  1.05707958
    ## [13] -0.67569701  0.91211922  0.53198221  0.72798481 -0.37468153 -0.97159197
    ## [19]  0.88146493 -0.67512900 -0.88240014  0.56275847 -0.83082749 -1.29415402
    ## [25]  0.80576965  0.47663933  0.06827208  1.20708601  1.61600725 -0.63937744

I want a function to compute z-scores

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("input must be numeric")
  }
  
  if (length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1]  0.46457181 -2.94529937  0.58700242 -0.26607221 -0.84653988  0.65033163
    ##  [7]  0.06567186  0.03067863  1.51170483 -1.30684019 -0.44851447  1.05707958
    ## [13] -0.67569701  0.91211922  0.53198221  0.72798481 -0.37468153 -0.97159197
    ## [19]  0.88146493 -0.67512900 -0.88240014  0.56275847 -0.83082749 -1.29415402
    ## [25]  0.80576965  0.47663933  0.06827208  1.20708601  1.61600725 -0.63937744

Try my function on some other things. These should give errors.

``` r
z_scores(3)
```

    ## Error in z_scores(3): Input must have at least three numbers

``` r
z_scores("ny name is Jeff")
```

    ## Error in z_scores("ny name is Jeff"): input must be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): input must be numeric
