library(tidyverse)
library(modelr)
library(broom)

# Load the data
data("swiss")
# Inspect the data
sample_n(swiss, 3)

summary(swiss)
#Using Linear Model
Model0 <- lm(Fertility ~1, data = swiss)
Model1 <- lm(Fertility ~., data = swiss)
Model2 <- lm(Fertility ~. -Examination, data = swiss)
summary(Model0)
summary(Model1)
summary(Model2)


cbind(AIC(Model0),AIC(Model1),AIC(Model2))
cbind(BIC(Model0),BIC(Model0),BIC(Model2))

library(modelr)
data.frame(
  R2 = rsquare(Model1, data = swiss),
  RMSE = rmse(Model1, data = swiss),
  MAE = mae(Model1, data = swiss)
)


library(caret)
predictions <- Model1 %>% predict(swiss)
data.frame(
  R2 = R2(predictions, swiss$Fertility),
  RMSE = RMSE(predictions, swiss$Fertility),
  MAE = MAE(predictions, swiss$Fertility)
)

library(broom)
rbind(glance(Model0),glance(Model1),glance(Model2))
