# https://stackoverflow.com/questions/14482502/histogram-with-curve-in-r

population_mean <- 125
population_sd <- 7
n <- 1000
x <- rnorm(n, population_mean, population_sd)

population_x <- seq(
  qnorm(0.001, population_mean, population_sd), 
  qnorm(0.999, population_mean, population_sd), 
  length.out = 1000
)

binwidth <- 5
breaks <- seq(floor(min(x)), ceiling(max(x)), binwidth)

hist(x)

# The count curve is the normal density times the number of data points divided by the binwidth.
lines(
  population_x, 
  n * dnorm(population_x, population_mean, population_sd) * binwidth, 
  col = "red"
)

# Let's see that again with the sample distribution rather than the population distribution.

sample_mean <- mean(x)
sample_sd <- sd(x)
sample_x <- seq(
  qnorm(0.001, sample_mean, sample_sd), 
  qnorm(0.999, sample_mean, sample_sd), 
  length.out = 1000
)
lines(
  population_x, 
  n * dnorm(sample_x, sample_mean, sample_sd) * binwidth, 
  col = "blue"
)
