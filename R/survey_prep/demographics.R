# Demographics
tmp1 <- surveys_read$demographics_A %>% 
  select(ResponseId, StartDate, EndDate, Finished, uid, age:how_well)

tmp2 <- surveys_read$demographics_B %>% 
  select(ResponseId, StartDate, EndDate, Finished, uid, age:how_well)

demo <- rbind(tmp1, tmp2)

# checks uid match correct pattern
table(demo %>% filter(., !str_detect(uid, "[0-9]_[0-9]_[d-g][1-2]")) %>% select(uid))

# Data Screening ----------------------------------------------------------
#' identified that following uids appeared twice
#'    "17100609_1_d2" accidentally entered that they knew their teammate remove first attempt - ResponseId == "R_AmU7vl1JUzkUfLj"
#'    "17100612_2_g1" accidentally entered no for audio consent so restarted to correct remove first attempt - "R_1rqVwnuRIiGsD8G"
#'    "17103012_1_d2" accidentally entered that they knew their teammate remove first attempt - ResponseId == "R_1rOIEdVTNOdmuTM"
#'    "18041210_1_d2" assigned the same uids to two sessions - ResponseId == R_1K9KXw4IWKFmCLk should be 18041213_1_d2
#'    "18041210_1_g2" assigned the same uids to two sessions - ResponseId == R_1onAseOvLx9Zrou should be 18041213_1_g2
#'    "18041210_2_d1" assigned the same uids to two sessions - ResponseId == R_2PzYNC5RXf232Ea should be 18041213_2_d1
#'    "18041210_2_g1" assigned the same uids to two sessions - ResponseId == R_4JlqzPZq5AlYoMN should be 18041213_2_g1

demo <- demo %>% 
  mutate(
    uid = ifelse(ResponseId == "R_1K9KXw4IWKFmCLk", "18041213_1_d2",
                 ifelse(ResponseId == "R_1onAseOvLx9Zrou", "18041213_1_g2",
                        ifelse(ResponseId == "R_2PzYNC5RXf232Ea", "18041213_2_d1",
                               ifelse(ResponseId == "R_4JlqzPZq5AlYoMN", "18041213_2_g1", uid))))) %>% 
  filter(ResponseId != "R_AmU7vl1JUzkUfLj") %>% 
  filter(ResponseId != "R_1rqVwnuRIiGsD8G") %>% 
  filter(ResponseId != "R_1rOIEdVTNOdmuTM")
                          
# Check participants appear correct number of times =======================
n_questions <- 1 # number of questions in this test

x <- table(demo$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- demo %>%
  select(uid, age, gender, aus_born, eng_fl, team_familiarity)

# by column
col_miss <- sapply(x, function(col) {
  col %>%
    is.na() %>%
    sum()
})

# by row
row_miss <- apply(x, 1, function(row) {
  row %>%
    is.na() %>%
    any()
})

check <- if (any(row_miss)) {
  print("missing values for participant:")
  print(x[row_miss, ]) # to print uid if there is a missing value
}  else {
  print("no missing values")
}

# visually check that there are no missing values per user per variable
demo %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# convert character vectors to numeric
demo$dic_use[demo$dic_use == "Never"] <- 1
demo$dic_use[demo$dic_use == "Rarely"] <- 2
demo$dic_use[demo$dic_use == "Sometimes"] <- 3
demo$dic_use[demo$dic_use == "Often"] <- 4
demo$dic_use[demo$dic_use == "All of the time"] <- 5

demo$how_well[demo$how_well == "Not at all"] <- 1
demo$how_well[demo$how_well == "A little"] <- 2
demo$how_well[demo$how_well == "Reasonably well"] <- 3
demo$how_well[demo$how_well == "Well"] <- 4
demo$how_well[demo$how_well == "Very well"] <- 5

table(demo$how_well)

demo <- demo %>% 
  select(uid:how_well) %>% 
  mutate(age = as.numeric(age),
         aus_years = as.numeric(aus_years),
         dic_use = as.numeric(dic_use),
         how_long = as.numeric(how_long),
         how_well = as.numeric(how_well),
         gender = ifelse(gender == "Male", 1,
                         ifelse(gender == "Female", 0, NA)),
         aus_born = ifelse(aus_born == "Yes", TRUE,
                           ifelse(aus_born == "No", FALSE, NA)),
         aus_years = as.numeric(aus_years),
         eng_fl = ifelse(eng_fl == "Yes", TRUE,
                         ifelse( eng_fl == "No", FALSE, NA)),
         team_familiarity = ifelse(team_familiarity == "Yes", TRUE,
                         ifelse(team_familiarity == "No", FALSE, NA)))

# Create grouping variables 
# group == date + group
# member == g == 1, d == 2
demo <- demo %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                      ifelse(grepl("d[1-2]", uid), "2", 
                                             ifelse(grepl("e1", uid), "1", "2"))),
                      recruit = ifelse(grepl("[dg]", uid), "sona", "ext"),
                      group = gsub("_[a-z][12]", "", uid))

# session notes indicate several issues to be fixed
# uid == 18110716_1_g2 & uid == 18062815_1_e2 accidentally entered that they knew their teammate
# uid == 18072412_1_e1 accidentally entered that English is their first language
demo <- demo %>% mutate(team_familiarity = ifelse(uid == "18110716_1_g2", FALSE, 
                                                  ifelse(uid == "18062815_1_e2", FALSE, team_familiarity)),
                        how_long = ifelse(uid == "18110716_1_g2", NA, 
                                          ifelse(uid == "18062815_1_e2", NA, how_long)),
                        how_well = ifelse(uid == "18110716_1_g2", NA, 
                                          ifelse(uid == "18062815_1_e2", NA, how_well)),
                        eng_fl = ifelse(uid == "18072412_1_e1", FALSE, eng_fl))



# session notes indicate that uid == 19020112_1_g2 reported rarely using a dictionary despite using one frequently throughout the study
# change their respone to 4 = often
demo <- demo %>% mutate(dic_use = ifelse(uid == "19020112_1_g2", 4, dic_use))

# remove grouping variables
demo <- demo %>% select(-group, -recruit, -member)

# modify uids so data is consistent with other tests
demo <- demo %>% 
  mutate(uid = ifelse(uid == "18073109_1_e1", "18073109_1_g2", 
                      ifelse(uid == "18073109_1_e2", "18073109_1_d2", 
                             ifelse(uid == "18073110_2_e1", "18073110_2_g1", 
                                    ifelse(uid == "18073110_2_e2", "18073110_2_d1", uid)))))

