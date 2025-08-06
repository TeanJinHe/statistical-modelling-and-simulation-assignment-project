#Case Study Chapter 3
install.packages(c("mplot", "pairsD3", "Hmisc", "d3heatmap", 
                   "mfp", "dplyr"))

install.packages("d3heatmap")
install.packages("Rtools")
library(Rtools)
library(d3heatmap)

library(mplot)
library(pairsD3)
library(Hmisc)

library(mfp)
library(dplyr)


loc <- "https://www4.stat.ncsu.edu/~boos/var.select/diabetes.tab.txt"
dat <- as.matrix(read.table(loc,header=T,sep="\t"))
dim(dat)

data("diabetes", package = "mplot")
# help('diabetes', package='mplot')
str(diabetes)  # structure of the diabetes

summary(diabetes) #Descriptive Statistics
pairs(diabetes)  # traditional pairs plot
round(cor(diabetes),4)
boxplot(diabetes)  # always a good idea to check for gross outliers

pairsD3::shinypairs(diabetes)  # interactive pairs plot of the data set
d3heatmap::d3heatmap(cor(diabetes))
Hmisc::describe(diabetes, digits = 1)  # summary of the diabetes data


#Example Linear Model
Model0 = lm(y ~ 1, data = diabetes)  # Null model
summary(Model0)
Model1 = lm(y ~ ., data = diabetes)  # Full model
summary(Model1)


cbind(AIC(Model0),AIC(Model1))
cbind(BIC(Model0),BIC(Model1))

#backward selecton using BIC and AIC
step.back.bic = step(Model1, direction = "backward", trace = FALSE, k = log(442))
summary(step.back.bic)

step.back.aic = step(Model1, direction = "backward", trace = FALSE, k = 2)
summary(step.back.aic)

#Example Generalised Linear Model (GLM)
Model00 = glm(y ~ 1, data = diabetes)  # Null model
Model11 = glm(y ~ ., data = diabetes)  # Full model BY default Gaussian
summary(Model11)
summary(Model00)


Model22 <- glm(y ~ ., data = diabetes, family = "gaussian")
summary(Model22)

step.back.aic = step(Model22, direction = "backward", trace = FALSE, k = 2)
summary(step.back.aic)

anova(Model22, test = "Chisq")
