library(tidyverse)

a2 <- read_csv("data/demo_a2.csv")[-c(1, 2),] %>%
  select(age:uid)

b2 <- read_csv("data/demo_b2.csv")[-c(1, 2),] %>%
  select(age:uid)

demo <- rbind(a2, b2)

table(table(demo$uid) > 1)

demo %>%
  mutate(age = as.numeric(age)) %>%
  summarise(mean_age = (mean(age)),
            sd_age = sd(age),
            gender = mean(gender == "Female"),
            aus_born = mean(aus_born == "Yes"),
            eng_fl = mean(eng_fl == "Yes"),
            team_familiarity = mean(team_familiarity == "Yes"))