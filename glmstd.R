#install.packages("packagename")
install.packages("aod")
library(aod)
library(ggplot2)
mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
## view the first few rows of the data
head(mydata)

summary(mydata)

#standard deviation
sapply(mydata, sd)

## two-way contingency table of categorical outcome and predictors we want
## to make sure there are not 0 cells
xtabs(~admit + rank, data = mydata)

mylogit <- glm(admit ~ gre^2, data = mydata, family = "binomial")
summary(mylogit)
mylogit1 <- lm(admit ~ gre^2, data = mydata)
summary(mylogit1)

#The code below estimates a logistic regression model using the glm (generalized linear model) function. 
#First, we convert rank to a factor to indicate that rank should be treated as a categorical variable.
mydata$rank <- factor(mydata$rank)
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
summary(mylogit)

## CIs using profiled log-likelihood
confint(mylogit)

## CIs using standard errors
confint.default(mylogit)

#b- coefficient, Sigma -  the variance covariance matrix of the error terms and 
#Terms - which terms in R to be tested
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 4:6)

## odds ratios only
exp(coef(mylogit))
## odds ratios and 95% CI
exp(cbind(OR = coef(mylogit), confint(mylogit)))


## create new data
newdata1 <- with(mydata, data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4)))

#OR the values of rankP should be predictions made using the predict( ) function.
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response")
newdata1

#TO plot and Create more data
#create a data set of 200
newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100),
                                              4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100))))

#The code to generate the predicted probabilities (the first line below) is the same as before,
#except we are also going to ask for standard errors so we can plot a confidence interval. We get the 
#estimates on the link scale and back transform both the predicted values and confidence limits into probabilities.
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type = "link",
                                    se = TRUE))
newdata3 <- within(newdata3, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

## view first few rows of final dataset
head(newdata3)

#to use graphs of predicted probabilities to understand and/or present the model
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL,
                                                                    ymax = UL, fill = rank), alpha = 0.2) + geom_line(aes(colour = rank),                                                                                                            size = 1)

#to check the model good or not Analysis of Deviance Table
summary(mylogit)
logLik(mylogit)

anova(mylogit, test = "Chisq")


