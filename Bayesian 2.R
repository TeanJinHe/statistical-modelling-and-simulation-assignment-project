#Paramters
prop_success <- 0.15
n_zombies <- 13
#simulate the data with rbinom()
data <- rbinom( n_zombies, 1, prop_success )
data

#simulate from uniform
data <- c()
for( zombie in 1 : n_zombies ) {
  data[ zombie ] <- runif( 1, min = 0, max = 1 ) < prop_success
}
data <- as.numeric( data )
data

prop_success <- 0.42
n_zombies <- 100
#simulate the data with rbinom()=rbinom(n, size, prob) size=number of trials
data <- rbinom( 1, n_zombies, prop_success )
data


prop_success <- 0.42
n_zombies <- 100
#simulate the data with rbinom()
data <- rbinom( 200, n_zombies, prop_success )
data

summary( data )
mean( data )


cured_zombies <- rbinom( n = 100000, size = 100, prob = 0.07 )
post_df <- data.frame( 'Cured_Zombies' = cured_zombies )

ggplot( post_df, aes( x = Cured_Zombies ) ) +
  geom_histogram( binwidth = 1, fill = 'palegreen4', color = "#e9ecef" ) +
  ggtitle( 'Cured Zombies Probability Distribution' )
