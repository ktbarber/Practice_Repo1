Monte_Carlo_diff <- function(dist, n){
  if(dist == "Discrete Uniform"){
    x <- sample(x = 1:10, size = n, replace = TRUE)
  } else if(dist == "Normal"){
    x <- rnorm(n, mean = 0, sd = 1)
  } else if(dist == "Poisson"){
    x <- rpois(n, lambda = 0.75)
  } else if(dist == "Exponential"){
    x <- rexp(n, rate = 0.5)
  }
  return(mean(x) - median(x))
}
#1
Monte_Carlo_DF_1 <- data.frame(
  distribution = "Discrete Uniform",
  sample_size = 10,
  difference = simResults_1
  )

simResults_1 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Discrete Uniform",
    n = 10))
#2
simResults_2 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Discrete Uniform",
    n = 50))

Monte_Carlo_DF_2 <- data.frame(
  distribution = "Discrete Uniform",
  sample_size = 50,
  difference = simResults_2
)
#3
simResults_3 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Discrete Uniform",
    n = 100))
Monte_Carlo_DF_3 <- data.frame(
  distribution = "Discrete Uniform",
  sample_size = 100,
  difference = simResults_3
)

#4
simResults_4 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Normal",
    n = 10))
Monte_Carlo_DF_4 <- data.frame(
  distribution = "Normal",
  sample_size = 10,
  difference = simResults_4
)

#5
simResults_5 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Normal",
    n = 50))
Monte_Carlo_DF_5 <- data.frame(
  distribution = "Normal",
  sample_size = 50,
  difference = simResults_5
)

#6
simResults_6 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Normal",
    n = 100))
Monte_Carlo_DF_6 <- data.frame(
  distribution = "Normal",
  sample_size = 100,
  difference = simResults_6
)

#7
simResults_7 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Poisson",
    n = 10))
Monte_Carlo_DF_7 <- data.frame(
  distribution = "Poisson",
  sample_size = 10,
  difference = simResults_7
)

#8
simResults_8 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Poisson",
    n = 50))
Monte_Carlo_DF_8 <- data.frame(
  distribution = "Poisson",
  sample_size = 50,
  difference = simResults_8
)

#9
simResults_9 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Poisson",
    n = 100))
Monte_Carlo_DF_9 <- data.frame(
  distribution = "Poisson",
  sample_size = 100,
  difference = simResults_9
)

#10
simResults_10 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Exponential",
    n = 10))
Monte_Carlo_DF_10 <- data.frame(
  distribution = "Exponential",
  sample_size = 10,
  difference = simResults_10
)

#11
simResults_11 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Exponential",
    n = 50))
Monte_Carlo_DF_11 <- data.frame(
  distribution = "Exponential",
  sample_size = 50,
  difference = simResults_11
)

#12
simResults_12 <- replicate(
  n = 10000,
  expr = Monte_Carlo_diff(
    dist = "Exponential",
    n = 100))
Monte_Carlo_DF_12 <- data.frame(
  distribution = "Exponential",
  sample_size = 100,
  difference = simResults_12
)
Full_Monte_Carlo_DF <- bind_rows(Monte_Carlo_DF_1, Monte_Carlo_DF_2, Monte_Carlo_DF_3,
                                 Monte_Carlo_DF_4, Monte_Carlo_DF_5, Monte_Carlo_DF_6,
                                 Monte_Carlo_DF_7, Monte_Carlo_DF_8, Monte_Carlo_DF_9,
                                 Monte_Carlo_DF_10, Monte_Carlo_DF_11, Monte_Carlo_DF_12)
library(ggplot2)
ggplot(Full_Monte_Carlo_DF, aes(x = difference, fill = distribution)) +
  geom_density() + 
  geom_vline(xintercept = 0, linetype = 2) + 
  facet_grid(distribution ~ sample_size) + 
  labs(
    title = "Plot of mean and median difference in accordance to different
    distribution and sample size",
    x = "mean(x) - median(x)",
    y = "Density",
    fill = "Distribution"
  ) 

