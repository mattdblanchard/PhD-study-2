# This script is intended to clean data collected by the id-info-consent survey
tmp1 <- surveys_read$consent_A_sona %>% 
  select(ResponseId, StartDate, EndDate, Finished, uid:email)

tmp2 <- surveys_read$consent_B_sona %>% 
  select(ResponseId, StartDate, EndDate, Finished, uid:email)

tmp3 <- surveys_read$consent_A_external %>% 
  select(ResponseId, StartDate, EndDate, Finished, uid:email)

tmp4 <- surveys_read$consent_B_external %>% 
  select(ResponseId, StartDate, EndDate, Finished, uid:email)

consent <- rbind(tmp1, tmp2, tmp3, tmp4)

# checks uid match correct pattern
# identified uid == "19020712_1" did not complete the survey so remove
consent <- consent %>% filter(uid != "19020712_1")

# check that uids conform to the correct structure
table(consent %>% filter(., !str_detect(uid, "[0-9]_[0-9]_[d-g][1-2]")) %>% select(uid))

# Data Screening ----------------------------------------------------------
#' identified that following uids appeared twice
#'    "17100609_1_d2" completed consent 4 times. Remove the first three attempts - ResponseId == "R_1dHcJgF7vdquD0d", "R_Ah4zGj1CKGkQwNj", "R_1OvqT75k7y7kTvG"
#'    "17100612_2_g1" accidentally entered no for audio consent so restarted to correct remove first attempt - "R_1dKtZlE8l09sOjX"
#'    "17103012_1_d2" ompleted consent 3 times. Remove the first two attempts - ResponseId == "R_2aCOU5v2LITSFtW", "R_2b35iEGt4OgOLnn"
#'    "18041210_1_d2" assigned the same uids to two sessions - ResponseId == R_1eyPbZiUlmYSstk should be 18041213_1_d2
#'    "18041210_1_g2" assigned the same uids to two sessions - ResponseId == R_2pSOEp3LGc43Ci2 should be 18041213_1_g2
#'    "18041210_2_d1" assigned the same uids to two sessions - ResponseId == R_cFGNluZTmvYEPJf should be 18041213_2_d1
#'    "18041210_2_g1" assigned the same uids to two sessions - ResponseId == R_25XyDZweAtQw1SE should be 18041213_2_g1
#'    "18062815_1_e2" first attempt incomplete. Remove this attempt
#'    "18082210_1_d2" first attempt incomplete. Remove this attempt
#'    "18082210_1_g2" first attempt incomplete. Remove this attempt
#'    "18082215_1_e2" first attempt incomplete. Remove this attempt

consent <- consent %>% 
  mutate(
    uid = ifelse(ResponseId == "R_1eyPbZiUlmYSstk", "18041213_1_d2",
                 ifelse(ResponseId == "R_2pSOEp3LGc43Ci2", "18041213_1_g2",
                        ifelse(ResponseId == "R_cFGNluZTmvYEPJf", "18041213_2_d1",
                               ifelse(ResponseId == "R_25XyDZweAtQw1SE", "18041213_2_g1", uid))))) %>%
  filter(Finished == "True") %>% 
  filter(ResponseId != "R_1dHcJgF7vdquD0d") %>% 
  filter(ResponseId != "R_Ah4zGj1CKGkQwNj") %>% 
  filter(ResponseId != "R_1OvqT75k7y7kTvG") %>% 
  filter(ResponseId != "R_1dKtZlE8l09sOjX") %>% 
  filter(ResponseId != "R_2aCOU5v2LITSFtW") %>% 
  filter(ResponseId != "R_2b35iEGt4OgOLnn")
  

# Check participants appear correct number of times =======================
n_questions <- 1 # number of questions in this test

x <- table(consent$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- consent %>%
  select(uid, CONSENT:feedback)

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
consent %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# session notes indicate that uid == 18110713_2_g1 withdrew their consent to record audio
consent <- consent %>% mutate(audio = ifelse(uid == "18110713_2_g1", 
                                             "I DO NOT GIVE PERMISSION for the researchers to make audio recordings of my participation.", 
                                             audio))


uids <- consent %>% select(uid)

