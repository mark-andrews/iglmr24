library(tidyverse)

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
