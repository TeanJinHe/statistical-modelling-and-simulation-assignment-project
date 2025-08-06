#Example 1
# Usual bootstrap of the ratio of means using the city data
ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
output <-boot(city, ratio, R = 999, stype = "w")

# Plotting the output
output 
plot(output)
# Obtaining a confidence interval of 95%
boot.ci(output, type="bca")

#EXAMPLE 2
# Stratified resampling for the difference of means. In this
# example we will look at the difference of means between the final
# two series in the gravity data.
diff.means <- function(d, f)
{ n <- nrow(d)
gp1 <- 1:table(as.numeric(d$series))[1]
m1 <- sum(d[gp1,1] * f[gp1])/sum(f[gp1])
m2 <- sum(d[-gp1,1] * f[-gp1])/sum(f[-gp1])
ss1 <- sum(d[gp1,1]^2 * f[gp1]) - (m1 * m1 * sum(f[gp1]))
ss2 <- sum(d[-gp1,1]^2 * f[-gp1]) - (m2 * m2 * sum(f[-gp1]))
c(m1 - m2, (ss1 + ss2)/(sum(f) - 2))
}
grav1 <- gravity[as.numeric(gravity[,2]) >= 7,]
output1 <-boot(grav1, diff.means, R = 999, stype = "f", strata = grav1[,2])

# Plotting the output
output1 
plot(output1)
# Obtaining a confidence interval of 95%
boot.ci(output1, type="bca")



