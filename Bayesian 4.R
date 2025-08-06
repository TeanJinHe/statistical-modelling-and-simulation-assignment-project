#CALCULATE THE LIKELIHOOD
#calculate P(n_visitors = 13 | prob_success = 10%)
dbinom( 13, size = 100, prob = 0.1)

#calculate P(n_visitors = 13 or n_visitors = 14 | prob_success = 10 )
dbinom( 13, size = 100, prob = 0.1 ) + dbinom( 14, size = 100, prob = 0.1 )

#Calculate Probability Distribution
#calculate P( n_visitors | prob_success = 0.1 )
n_visitors <- seq( 0, 100, by = 1 )
probability <- dbinom( n_visitors, size = 100, prob = 0.1 )
plot(n_visitors, probability, type ='h')

#Probability Density.. Continuous Distributions
dunif( x = 0.12, min = 0, max = 0.2 )

# Explore using dbinom to calculate probability distributions
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- seq(0, 100)
prob <- dbinom(n_visitors, 
    size = n_ads_shown, prob = proportion_clicks)

# Plot the distribution
plot( n_visitors, prob, type = 'h')

# Change the code, proportion_clicks between 0-1
n_ads_shown <- 100
proportion_clicks <- seq(0, 1, by = 0.01)
n_visitors <- 13
prob <- dbinom(n_visitors, 
               size = n_ads_shown, prob = proportion_clicks)

plot(proportion_clicks, prob, type = "h")

#BAYESIAN CALCULATION
n_ads_shown <- 100
n_visitors <- seq( 0, 100, by = 1 )
proportion_clicks <- seq( 0, 1, 0.01 )
pars <-  expand.grid( proportion_clicks = proportion_clicks,
                      n_visitors = n_visitors ) 
pars$prior <- dunif( pars$proportion_clicks, min = 0, max = 0.2 )
pars$likelihood <- dbinom( pars$n_visitors, size = n_ads_shown,
                           prob = pars$proportion_clicks)
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum( pars$probability )

glimpse(pars) #use dplyr package

#PLOT
ggplot( pars, aes( x = n_visitors, y = proportion_clicks,
                   fill = probability ) ) +
  geom_tile() +
  xlim( c( 0, 35 ) ) +
  ylim( c( 0, 0.2 ) ) +
  scale_fill_gradient(low="white", high="darkblue")


pars13 <- pars[ pars$n_visitors == 13, ]
pars13$probability <- pars13$probability / sum( pars$ probability )

ggplot( pars13, aes( x = n_visitors, y = proportion_clicks,
                     fill = probability ) ) +
  geom_tile() +
  xlim( c( 0, 35 ) ) +
  ylim( c( 0, 0.2 ) ) +
  scale_fill_gradient(low="white", high="darkblue")

#A conditional shortcut: You can directly condition on the data, 
#no need to first create the joint distribution.

# Simplify the code below by directly conditioning on the data
n_ads_shown <- 100
proportion_clicks <- seq(0, 1, by = 0.01)
n_visitors <- 6
pars <- expand.grid(proportion_clicks = proportion_clicks,
                    n_visitors = n_visitors)
pars$prior <- dunif(pars$proportion_clicks, min = 0, max = 0.2)
pars$likelihood <- dbinom(pars$n_visitors, 
                          size = n_ads_shown, prob = pars$proportion_clicks)
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum(pars$probability)

plot(pars$proportion_clicks, pars$probability, type = "h")




