
# Source: http://www.sthda.com/english/articles/38-regression-model-validation/158-regression-model-accuracy-metrics-r-square-aic-bic-cp-and-more/



# Example of data ------------------------------------------------------------------
# Load the data
data("swiss")
# Inspect the data
sample_n(swiss, 3)
dim(swiss)


# Building regression models ------------------------------------------------------
model1 <- lm(Fertility ~., data = swiss)
model2 <- lm(Fertility ~. -Examination, data = swiss)


# Assessing model quality ---------------------------------------------------------
summary(model1)
AIC(model1)
BIC(model1)

library(modelr)
data.frame(
    R2 = rsquare(model1, data = swiss),
    RMSE = rmse(model1, data = swiss),
    MAE = mae(model1, data = swiss)
)



library(caret)
predictions <- model1 %>% predict(swiss)
data.frame(
    R2 = R2(predictions, swiss$Fertility),
    RMSE = RMSE(predictions, swiss$Fertility),
    MAE = MAE(predictions, swiss$Fertility)
)


library(broom)
glance(model1)


library(tidyverse)
# Make predictions and compute the
# R2, RMSE and MAE
swiss %>%
    add_predictions(model1) %>%
    summarise(
        R2 = cor(Fertility, pred)^2,
        MSE = mean((Fertility - pred)^2),
        RMSE = sqrt(MSE),
        MAE = mean(abs(Fertility - pred))
    )


# Comparing regression models performance -------------------------------------------------
# Metrics for model 1
glance(model1) %>%
    select(adj.r.squared, sigma, AIC, BIC, p.value)

# Metrics for model 2
glance(model2) %>%
    select(adj.r.squared, sigma, AIC, BIC, p.value)


# 
# From the output above, it can be seen that:
#     
#     The two models have exactly the samed adjusted R2 (0.67), meaning that they are equivalent in explaining the outcome, here fertility score. Additionally, they have the same amount of residual standard error (RSE or sigma = 7.17). However, the model 2 is more simple than model 1 because it incorporates less variables. All things equal, the simple model is always better in statistics.
# 
# The AIC and the BIC of the model 2 are lower than those of the model1. In model comparison strategies, the model with the lowest AIC and BIC score is preferred.
# 
# Finally, the F-statistic p.value of the model 2 is lower than the one of the model 1. This means that the model 2 is statistically more significant compared to model 1, which is consistent to the above conclusion.
# 
# Note that, the RMSE and the RSE are measured in the same scale as the outcome variable. Dividing the RSE by the average value of the outcome variable will give you the prediction error rate, which should be as small as possible:
    
sigma(model1)/mean(swiss$Fertility)
# the average prediction error rate is 10%.
