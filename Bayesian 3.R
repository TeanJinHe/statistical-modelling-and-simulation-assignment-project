# Fill in the parameters
n_samples <- 100000
n_ads_shown <- 100
proportion_clicks <- 0.1
n_visitors <- rbinom(n_samples, size = n_ads_shown, 
                     prob = proportion_clicks)

post_df <- data.frame( 'Site_Visits' = n_visitors )

ggplot( post_df, aes( x = Site_Visits ) ) +
  geom_histogram( binwidth = 1, fill = 'cadetblue', color = "#e9ecef" ) +
  ggtitle( 'Number of Visitors Distribution' )

#to assign the prior
n_samples <- 100000
n_ads_shown <- 100
# model a uniform probability distribution that ranges from 0 to 0.2 to represent the uncertainty of our value for this prior
proportion_clicks <- runif( n = n_samples, min = 0.0, max = 0.2 )
n_visitors <- rbinom( n_samples, n_ads_shown, proportion_clicks )

#Visualize the probability distribution for the prior, proportion_clicks.
pclick_df <- data.frame( 'probability_click' = proportion_clicks )

pclick_plot <- ggplot( pclick_df, aes( x = probability_click ) ) +
  geom_histogram( fill = 'cadetblue', color = "#e9ecef" ) +
  ggtitle( 'Probability Distribution of the Prior' )

nvis_df <- data.frame( 'Site_Visits' = n_visitors )

nvis_plot <- ggplot( nvis_df, aes( x = Site_Visits ) ) +
  geom_histogram( binwidth = 1, fill = 'cadetblue', color = "#e9ecef" ) +
  ggtitle( 'Number of Visitors Distribution' )

grid.arrange( pclick_plot, nvis_plot, ncol = 2 ) #install("gridExtra)


#Bayesian Models and Conditioning

#this dataframe represents the joint probability distribution over proportion_clicks and n_visitors together.
prior_df <- data.frame( 'proportion_clicks' = proportion_clicks, 'n_visitors' = n_visitors )

#visualize as a scatterplot....
prior_plot <- ggplot( prior_df, aes( x = n_visitors, y = proportion_clicks ) ) +
  geom_point( alpha = 1/10 ) +
  #geom_jitter(alpha = 1/10 ) +
  theme( legend.position = 'none' )
prior_plot <- ggMarginal( prior_plot, type = 'histogram', 
                          xparams = list(  bins=20 ),
                          yparams = list(  bins=20))
prior_plot

#consider the conditional
prior10_df <- prior_df %>%
  mutate( p10 = round( proportion_clicks, 2) == 0.1 ) %>%
  filter( p10 == TRUE ) %>%
  select( c( n_visitors, proportion_clicks ) )

prior_plot <- ggplot( prior_df, aes( x = n_visitors, y = proportion_clicks ) ) +
  geom_point( alpha = 1/10 ) +
  geom_point(data = prior10_df, aes(x = n_visitors, y = proportion_clicks, color = 'red' ) ) +
  theme( legend.position = 'none' )
prior_plot <- ggMarginal( prior_plot, type = 'histogram', 
                          xparams = list(  bins=20 ),
                          yparams = list(  bins=20))
prior_plot


ggplot( prior10_df, aes( x = n_visitors ) ) +
  geom_histogram( binwidth = 1, fill = 'red', color = "#e9ecef" ) +
  ggtitle( 'Probability Distribution of n_visitors given proportion_clicks = 0.1' )


ggplot( prior10_df, aes( x = n_visitors ) ) +
  geom_histogram( binwidth = 1, fill = 'red', color = "#e9ecef" ) +
  ggtitle( 'Probability Distribution of n_visitors given proportion_clicks = 0.1' )

head( prior_df )
#condition the joint distribution with the observation that there were 13 visitors
# Create the posterior data frame
posterior <- prior_df[prior_df$n_visitors == 13, ]

# Visualize posterior proportion clicks
hist( posterior$proportion_clicks )




# Assign posterior to a new variable called prior
prior <- posterior

# Take a look at the first rows in prior
head(prior)
# Replace prior$n_visitors with a new sample and visualize the result
n_samples <-  nrow(prior)
n_ads_shown <- 100
prior$n_visitors <- rbinom(n_samples, size = n_ads_shown,
                           prob = prior$proportion_clicks)
hist(prior$n_visitors)


# Calculate the probability that you will get 5 or more visitors
sum(prior$n_visitors >= 5) / length(prior$n_visitors)
