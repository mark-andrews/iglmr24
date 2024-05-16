library(tidyverse)
library(modelr)
library(MASS)

doctor_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr24/main/data/DoctorAUS.csv")
doctor_df <- mutate(doctor_df, age = age * 100)

M_10 <- glm(doctorco ~ age, 
            data = doctor_df, 
            family = poisson(link = 'log')
)

summary(M_10)
confint.default(M_10)

estimates <- coef(M_10)

# log of the average number of visits if age = 25
estimates[1] + estimates[2] * 25

# the average number of visits if age = 25
exp(estimates[1] + estimates[2] * 25)

# the average number of visits if age = 75
exp(estimates[1] + estimates[2] * 75)

doctor_df2 <- tibble(age = seq(20, 80))

predictions1 <- add_predictions(doctor_df2, M_10)

ggplot(predictions1, aes(x = age, y = pred)) + geom_point() + geom_line()

# predicted average number of visits
predictions2 <- add_predictions(doctor_df2, M_10, type = 'response')

ggplot(predictions2, aes(x = age, y = pred)) + geom_point() + geom_line()

# meaning of coefficient
exp(estimates[2])


deviance(M_10)

M_11 <- glm(doctorco ~ 1, 
            data = doctor_df, 
            family = poisson(link = 'log')
)

deviance(M_11)

M_12 <- glm(doctorco ~ sex + age + income, 
            data = doctor_df, 
            family = poisson(link = 'log')
)

llsat <-  sum(dpois(doctor_df$doctorco, lambda = doctor_df$doctorco, log = TRUE))

deviance(M_10)
-2 * (logLik(M_10) - llsat)

deviance(M_12)
-2 * (logLik(M_12) - llsat)

anova(M_10, M_12, test = 'Chisq') # log likelihood ratio test

# Negative binomial -------------------------------------------------------

biochem_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr24/main/data/biochemist.csv")

pubs <- biochem_df$publications
mean(pubs)
var(pubs)

M_13 <- glm.nb(publications ~ prestige, data = biochem_df)

summary(M_13)

M_14 <- glm(publications ~ prestige, 
            family = poisson(link = 'log'),
            data = biochem_df)

summary(M_14)$coefficients
summary(M_13)$coefficients

estimates <- coef(M_13)

# log avg pubs if prestige = 1
estimates[1] + estimates[2] * 1

# avg pubs if prestige = 1
exp(estimates[1] + estimates[2] * 1)

# avg pubs if prestige = 5
exp(estimates[1] + estimates[2] * 5)

M_15 <- glm.nb(publications ~ gender + married + children + prestige + mentor,
               data = biochem_df)
summary(M_15)

M_16 <- glm.nb(publications ~ gender + married + I(children > 0) + prestige + mentor,
               data = biochem_df)
summary(M_16)

M_17 <- glm.nb(publications ~ gender + I(children > 0) + mentor,
               data = biochem_df)

anova(M_17, M_16)

# Zero inflated models ----------------------------------------------------

smoking_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr24/main/data/smoking.csv")

count(smoking_df, cigs)
table(smoking_df$cigs)

mean(smoking_df$cigs)

# sample some values from a Poisson with a mean of 8.7
mean(rpois(n = 1e6, lambda = 8.7) == 0)
