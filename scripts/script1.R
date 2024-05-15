library(tidyverse)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr24/main/data/weight.csv")

M_1 <- lm(weight ~ height, data = weight_df)

coef(M_1) # coefficients of the linear model
sigma(M_1)

summary(M_1)
confint(M_1) # 95% confidence interval
round(confint(M_1, parm = 'height'), 2)

M_2 <- lm(weight ~ height + age, data = weight_df)
coef(M_2)

# predictors: height, age, gender
M_3 <- lm(weight ~ height + age + gender, data = weight_df)
summary(M_3)


# logistic regression -----------------------------------------------------

theta <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# odds of theta
odds <- theta / (1 - theta)

# log odds
log(odds)

affairs_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr24/main/data/affairs.csv")

affairs_df <- mutate(affairs_df, had_affair = affairs > 0)

M_4 <- glm(had_affair ~ yearsmarried, 
           family = binomial(link = 'logit'),
           data = affairs_df)

estimates <- coef(M_4)

# log odds of an affair if yearsmarried = 10
estimates[1] + estimates[2] * 10

# log odds of an affair if yearsmarried = 20
estimates[1] + estimates[2] * 20

# inverse logit link function in R is plogis
# PROBABILITY of an affair if yearsmarried = 20
log_odds_if_yearmarried_is_20 <- estimates[1] + estimates[2] * 20

# convert log odds to probability using inverse link function
plogis(log_odds_if_yearmarried_is_20)

# probability of an affair if yearsmarried = 5
plogis(estimates[1] + estimates[2] * 5)

# model summary 
summary(M_4)
confint.default(M_4)

# Predictions using the predict function

# predicted log odds of an affair if yearsmarried is 5 or 10 or 20
affairs_df2 <- tibble(yearsmarried = c(5, 10, 20))

predict(M_4, newdata = affairs_df2) # predicted log odds

plogis(predict(M_4, newdata = affairs_df2)) # predicted probabilities (converted from log odds)

# or alternatively
predict(M_4, newdata = affairs_df2, type = 'response')

library(modelr)

add_predictions(affairs_df2, M_4) # predicted log odds
add_predictions(affairs_df2, M_4, type = 'response') # predicted probability

# range of values for yearsmarried from 1 to 50
affairs_df3 <- tibble(yearsmarried = seq(50))

# predicted log odds
M_4_predictions_1 <- add_predictions(affairs_df3, M_4)

# plot of predicted log odds
ggplot(M_4_predictions_1,
       aes(x = yearsmarried, y = pred)
) + geom_point() + geom_line()

# predicted probability
M_4_predictions_2 <- add_predictions(affairs_df3, M_4, type = 'response')

# plot of the predicted probability
ggplot(M_4_predictions_2,
       aes(x = yearsmarried, y = pred)
) + geom_point() + geom_line()