# Author DataFlair
library(boot)
# Creating Function to obtain R-Squared from the data
r_squared <- function(formula, data, indices) {
  val <- data[indices,] # selecting sample with boot 
  fit <- lm(formula, data=val)
  return(summary(fit)$r.square)
} 
# Performing 1500 replications with boot 
output <- boot(data=mtcars, statistic=r_squared, 
               R=1500, formula=mpg~wt+disp)

# Plotting the output
output 
plot(output)

# Obtaining a confidence interval of 95%
#The bootstrap CI function "boot.ci" returns five different types of confidence intervals:
#norm (Normal Representation), basic, stud (studentized), perc (percentile), bca (bias-corrected, accelerated)
boot.ci(output, type="norm")
boot.ci(output, type="basic")

