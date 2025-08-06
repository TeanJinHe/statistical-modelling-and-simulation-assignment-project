#using sample to generate a permutation of the sequence 1:10
sample(10)


#bootstrap sample from the same sequence
sample(10, replace=T)


#boostrap sample from the same sequence with 
#probabilities that favor the numbers 1-5
prob1 <- c(rep(.15, 5), rep(.05, 5))
prob1

sample(10, replace=T, prob=prob1)


#sample of size 5 from elements of a matrix 
#creating the data matrix
y1 <- matrix( round(rnorm(25,5)), ncol=5)
y1 


#saving the sample of size 5 in the vector x1
x1 <- y1[sample(25, 5)]
x1


#sampling the rows of the a matrix
#creating the data matrix
y2 <- matrix( round(rnorm(40, 5)), ncol=5)
y2


#saving the sample of rows in the matrix x2
x2 <- y2[sample(8, 3),  ]
x2


#calculating the standard error of the median
#creating the data set by taking 100 observations 
#from a normal distribution with mean 5 and stdev 3
#we have rounded each observation to nearest integer
data <- round(rnorm(100, 5, 3))
data[1:10] 


#obtaining 20 bootstrap samples 
#display the first of the bootstrap samples
resamples <- lapply(1:20, function(i) sample(data, replace = T))
resamples[1]


#calculating the median for each bootstrap sample 
r.median <- sapply(resamples, median)
r.median


#calculating the standard deviation of the distribution of medians
sqrt(var(r.median))


#displaying the histogram of the distribution of the medians 
hist(r.median)

#by one function
#function which will bootstrap the standard error of the median
b.median <- function(data, num) {
  resamples <- lapply(1:num, function(i) sample(data, replace=T))
  r.median <- sapply(resamples, median)
  std.err <- sqrt(var(r.median))
  list(std.err=std.err, resamples=resamples, medians=r.median)   
}

#generating the data to be used (same as in the above example)
data1 <- round(rnorm(100, 5, 3))

#saving the results of the function b.median in the object b1
b1 <- b.median(data1, 30)

#displaying the first of the 30 bootstrap samples
b1$resamples[1]
