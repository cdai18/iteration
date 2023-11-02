Writing Functions
================
Christina Dai

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.63271037 -0.14164184 -1.15497301  1.72001426 -1.59043358  0.26442906
    ##  [7]  0.02093010 -1.42501544 -0.24359570  1.84646413 -1.74563637  1.55580439
    ## [13]  1.54432031  1.12847047 -0.87594135 -0.35498041 -0.70816795 -0.25867022
    ## [19]  0.33381499 -1.51145996  1.04774294  0.11164123  0.70895543 -0.04501870
    ## [25]  0.00389671  0.65551983 -0.96360744  0.10509441 -0.35967713 -0.30098955

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

    ##  [1]  0.63271037 -0.14164184 -1.15497301  1.72001426 -1.59043358  0.26442906
    ##  [7]  0.02093010 -1.42501544 -0.24359570  1.84646413 -1.74563637  1.55580439
    ## [13]  1.54432031  1.12847047 -0.87594135 -0.35498041 -0.70816795 -0.25867022
    ## [19]  0.33381499 -1.51145996  1.04774294  0.11164123  0.70895543 -0.04501870
    ## [25]  0.00389671  0.65551983 -0.96360744  0.10509441 -0.35967713 -0.30098955

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

## Multiple outputs

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

Check that the function works

``` r
x_vec = rnorm(1000)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##      mean    sd
    ##     <dbl> <dbl>
    ## 1 0.00156  1.00

## Multiple inputs

I’d like to do this with a function.

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.60  2.97

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 4) {
  
  sim_data = 
    tibble(
      x = rnorm(n = samp_size, mean = mu, sd = sigma)
    )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )

}

sim_mean_sd(samp_size = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.24  3.11
