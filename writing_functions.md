Writing Functions
================
Christina Dai

## Do something simple

``` r
x_vec = rnorm(30, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  1.304708310 -0.678446298  0.358243731  1.168876442 -0.009321415
    ##  [6] -0.787431241  0.879285864 -0.804975268 -0.283021876 -1.011367377
    ## [11] -0.512039962  0.774731504 -1.296804570  0.595409174 -0.653363952
    ## [16]  0.366952513 -0.926321844  1.456089030  1.798802370 -1.673016090
    ## [21] -1.184137440 -0.476781750 -0.127328666  2.054555670 -1.257219329
    ## [26]  0.647874546 -0.245405937  0.976495211 -0.836193920  0.381152568

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

    ##  [1]  1.304708310 -0.678446298  0.358243731  1.168876442 -0.009321415
    ##  [6] -0.787431241  0.879285864 -0.804975268 -0.283021876 -1.011367377
    ## [11] -0.512039962  0.774731504 -1.296804570  0.595409174 -0.653363952
    ## [16]  0.366952513 -0.926321844  1.456089030  1.798802370 -1.673016090
    ## [21] -1.184137440 -0.476781750 -0.127328666  2.054555670 -1.257219329
    ## [26]  0.647874546 -0.245405937  0.976495211 -0.836193920  0.381152568

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
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0354  1.02

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
    ## 1  3.92  3.02

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
    ## 1  5.91  2.98

## Revisiting past examples

Loading the LoTR data.

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")

lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) |>
  janitor::clean_names() |>
  pivot_longer(
    female:male,
    names_to = "sex",
    values_to = "words") |> 
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything()) 
```

Create a function to load and tidy the data.

``` r
lotr_load_and_tidy = function(path, range, movie_name) {
  
  df = 
    readxl::read_excel(path, range = range) |>
    janitor::clean_names() |>
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words") |>
    mutate(
      race = str_to_lower(race),
      movie = movie_name) |> 
    select(movie, everything())
  
  df
  
}

lotr_tidy = 
  bind_rows(
    lotr_load_and_tidy("data/LotR_Words.xlsx", "B3:D6", "fellowship_ring"),
    lotr_load_and_tidy("data/LotR_Words.xlsx", "F3:H6", "two_towers"),
    lotr_load_and_tidy("data/LotR_Words.xlsx", "J3:L6", "return_king"))
```

## Mean scoping example

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4
