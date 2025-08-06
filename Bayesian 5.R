#EXAMPLE TEMPERATURE NORMAL LAKE

temp <- c( 19, 23, 20, 17, 23 )
#f = 66, 73, 68, 63, 73

#Use the Normal distribution as a generative model for values centered about a mean value.
rnorm( n = 5, mean =  20, sd = 2 )

#use dnorm() to look at how likely an outcome is #given some fixed parameters
like <- dnorm( x = temp, mean = 20, sd = 2 )
like

#the results are the relative likelihoods of the data points.
#log likelihood
log(like)

###using rnorm & dnorm

# Assign mu and sigma
mu <- 3500
sigma <- 600

par(mfrow = c( 1,2 ) ) 
weight_distr <- rnorm(n = 100000, mean = mu, sd = sigma)
p1 <- hist(weight_distr, 60, xlim = c(0, 6000), col = "lightgreen")

# Create weight
weight <- seq(0, 6000, by = 100)

# Calculate likelihood
likelihood <- dnorm(weight, mu, sigma)

# Plot the distribution of weight
p2 <- plot( x = weight,
            y = likelihood,
            type = 'h' )


temp <- c( 19, 23, 20, 17, 23 )
mu <- seq( 8, 30, by = 0.5 )
sigma <- seq( 0.1, 10, by = 0.3 )
pars <- expand.grid( mu = mu, sigma = sigma )

plot(pars, pch = 19, main = 'The Parameter Space')

#the priors
pars$mu_prior <- dnorm( pars$mu, mean = 18, sd = 5 )
pars$sigma_prior <- dunif( pars$sigma, min = 0, max = 10 )
pars$prior <- pars$mu_prior * pars$sigma_prior
for( i in 1:nrow( pars ) ) {
  likelihoods <- dnorm( temp, pars$mu[i], pars$sigma[i])
  pars$likelihood[i] <- prod( likelihoods )
}

#calculate the posterior probability
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum( pars$probability )

summary( pars )

#Visualise
ggplot( pars, aes( x = mu, y = sigma,
                   fill = probability ) ) +
  geom_tile() +
  xlim( c( 10, 30 ) ) +
  ylim( c( 0, 10 ) ) +
  scale_fill_gradient(low="white", high="darkblue")

#BASED ON CERTAIN QUESTION
temp <- c( 19, 23, 20, 17, 23 )
mu <- seq( 8, 30, by = 0.5 )
sigma <- seq( 0.1, 10, by = 0.3 )
pars <- expand.grid( mu = mu, sigma = sigma )
#the priors
pars$mu_prior <- dnorm( pars$mu, mean = 18, sd = 5 )
pars$sigma_prior <- dunif( pars$sigma, min = 0, max = 10 )
pars$prior <- pars$mu_prior * pars$sigma_prior
for( i in 1:nrow( pars ) ) {
  likelihoods <- dnorm( temp, pars$mu[i], pars$sigma[i])
  pars$likelihood[i] <- prod( likelihoods )
}

#calculate the posterior probability
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum( pars$probability )

summary( pars )

#SAMPLE
sample_indices <- sample( 1:nrow( pars ), size = 10000,
                          replace = TRUE, prob = pars$probability )
head( sample_indices )

pars_sample <- pars[sample_indices, c( 'mu', 'sigma' ) ]
head( pars_sample )

#plot
ggplot( pars_sample, aes( x = mu ) ) +
  geom_histogram( bins = 38 ) +
  ggtitle( 'Histogram of Mean Temps Simulated Data' )

#What's a good confidence interval for the temp?
quantile(pars_sample$mu, c( 0.05, 0.95 ))

#What is the probability that the temp is 18 or greater on July 20th?
#feed in the simulated data
pred_temp <- data.frame( 
  'pred_temp' = rnorm( 10000, 
                       mean = pars_sample$mu,
                       sd = pars_sample$sigma ) )

ggplot( pred_temp, aes( x = pred_temp ) ) +
  geom_histogram( bins = 38 ) +
  ggtitle( 'Histogram of Mean Temps Predicted Data' ) +
  geom_vline( xintercept = 18, color = 'red' )

#The probability that the temp is >= 18:
sum( pred_temp >= 18 ) / length( pred_temp )




  
