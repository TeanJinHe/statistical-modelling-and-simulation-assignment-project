#EXAMPLE ZOMBIE
# The IQ of a bunch of zombies
iq <- c(55, 44, 34, 18, 51, 40, 40, 49, 48, 46)
# Defining the parameter grid
pars <- expand.grid(mu = seq(0, 150, length.out = 100), 
                    sigma = seq(0.1, 50, length.out = 100))
# Defining and calculating the prior density for each parameter combination
pars$mu_prior <- dnorm(pars$mu, mean = 100, sd = 100)
pars$sigma_prior <- dunif(pars$sigma, min = 0.1, max = 50)
pars$prior <- pars$mu_prior * pars$sigma_prior
# Calculating the likelihood for each parameter combination
for(i in 1:nrow(pars)) {
  likelihoods <- dnorm(iq, pars$mu[i], pars$sigma[i])
  pars$likelihood[i] <- prod(likelihoods)
}
# Calculate the probability of each parameter combination
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum( pars$probability )

# Visualize
ggplot( pars, aes( x = mu, y = sigma,
                   fill = probability ) ) +
  geom_tile() +
  xlim( c( 30, 60 ) ) +
  ylim( c( 5, 20 ) ) +
  scale_fill_gradient(low="white", high="darkblue")

#Generate the same visualization as in the course 
#using the levelplot from the lattice library

coul <- colorRampPalette(brewer.pal(8, "PuRd"))(25)
levelplot(probability ~ mu * sigma, data = pars, , col.regions = coul)

