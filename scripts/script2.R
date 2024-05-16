library(tidyverse)

doctor_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr24/main/data/DoctorAUS.csv")
doctor_df <- mutate(doctor_df, age = age * 100)

M_10 <- glm(doctorco ~ age, 
            data = doctor_df, 
            family = poisson(link = 'log')
)

summary(M_10)
