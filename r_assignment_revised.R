#!/usr/bin/env r

# Export plots to a pdf file
pdf("revised_r.pdf")

# Initializing variables and vectors to be used later
n <- 10000
v <- c(0.1, 0.5, 1, 2, 5, 10, 100)

parameters <- c()
s_mean <- c()
s_var <- c()
t_mean <- c()
t_var <- c()
dist <- c()
running_mean <- c()

# Every alpha in v 
for (alpha in v)
{
  # Iterating over every lambda for each alpha
  for (lambda in v)
  {
    # Store the sample parameters and creating name for each histogram
    title <- paste("Gamma(", alpha, " - ", lambda, ")", sep="")
    parameters <- append(parameters, title)
    
    # Generate a gamma distribution of size n for each of the pairs
    d <- rgamma(n, alpha, lambda)
    
    # Plot on a histogram
    hist(d,
         main = title,
         xlab = "Alpha values",
         col = "light green",
         breaks = "scott")

    # Store the sample means and variances of each pair in vectors
    s_mean <- append(s_mean, mean(d))
    s_var <- append(s_var, var(d))
    
    # Store the theoretical means and variances of the gamma distributions
    t_mean <- append(t_mean, alpha/lambda)
    t_var <- append(t_var, alpha/lambda^2)

    # Create a running sample mean from 1 to 1000 and storing it in the running_mean vector
    means <- c()
    
    n = 1
    while (n <= 1000)
    {
      means <- append(means, mean(d[1:n]))
      n = n + 1
    }
    
    running_mean <- append(running_mean, means)
  }
}

# Generate a table comparing each theoretical expected value and variance
# of the distribution with the sample mean and variance

table <- data.frame(gamma=parameters,
                  expectation=t_mean,
                  variance=t_var,
                  sample_mean=s_mean,
                  sample_var=s_var)

print(table)

# For each 1 < n < 1000, calculate the sample mean of the first n sample
# values, and plot them in a graph against n.

i = 1
# Iterate through every pair of alpha and lambda
# Every alpha in v 
for (alpha in v)
{
  # Iterating over every lambda for each alpha
  for (lambda in v)
  {
    # Initialize means, the vector to be plotted
    means <- c()
    
    # Iterate i through every 1000 instances of running_mean 49 times
    j = i
    while (i < (j + 1000))
    {
      means <- append(means, running_mean[i])
      i = i + 1
    }
    
    # Create title of the plot
    title <- paste("Gamma(", alpha, ", ", lambda, ") Sample Means", sep="")
    
    # Plot the means against n
    plot(seq(1, 1000, 1), means,
         col="orange",
         main=title,
         xlab = "Sample Size",
         ylab = "Means",
         pch=20,
         type="l")
  }
}

dev.off()