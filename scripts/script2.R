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
