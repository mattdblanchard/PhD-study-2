# This script is intended to clean the data collected by R2F negative
# 
# if (file.exists("data/rds/r2f_neg.rds")) {
#   
#   library(tidyverse)
#   r2f_neg <- readRDS("data/rds/r2f_neg.rds")
# 
# } else {

tmp1 <- surveys_qrt$R2F_neg_A1 %>% 
  select(ResponseID, StartDate:Finished, uid, R2FItemNum,
         Ind_Stimulus.RESP, Ind_Stimulus.OnsetTime, 
         Ind_Stimulus.OffsetTime, Ind_Stimulus.RT,
         Wait.RT, Wait.OnsetTime, Wait.OffsetTime,
         Grp_Stimulus.RESP, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Stimulus.RT) %>%
  mutate(version = "a1")

tmp2 <- surveys_qrt$R2F_neg_B1 %>% 
  select(ResponseID, StartDate, EndDate, Finished, uid, R2FItemNum,
         Ind_Stimulus.RESP, Ind_Stimulus.OnsetTime, 
         Ind_Stimulus.OffsetTime, Ind_Stimulus.RT,
         Wait.RT, Wait.OnsetTime, Wait.OffsetTime,
         Grp_Stimulus.RESP, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Stimulus.RT) %>%
  mutate(version = "b1")

tmp3 <- surveys_r2f$R2F_neg_A2 %>% 
  rename(ResponseID = ResponseId, R2FItemNum = itemnum,
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Wait.OnsetTime = wait_onset,
         Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
         Grp_Stimulus.OnsetTime = grp_stimulus_onset, Grp_Stimulus.OffsetTime = grp_stimulus_offset,
         Grp_Stimulus.RT = grp_stimulus_rt, Grp_Stimulus.RESP = grp_response) %>% 
  select(ResponseID, StartDate, EndDate, Finished, uid, R2FItemNum,
         Ind_Stimulus.RESP, Ind_Stimulus.OnsetTime, 
         Ind_Stimulus.OffsetTime, Ind_Stimulus.RT,
         Wait.RT, Wait.OnsetTime, Wait.OffsetTime,
         Grp_Stimulus.RESP, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Stimulus.RT) %>%
  mutate(version = "a2")

tmp4 <- surveys_r2f$R2F_neg_B2 %>% 
    rename(ResponseID = ResponseId, R2FItemNum = itemnum,
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Wait.OnsetTime = wait_onset,
         Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
         Grp_Stimulus.OnsetTime = grp_stimulus_onset, Grp_Stimulus.OffsetTime = grp_stimulus_offset,
         Grp_Stimulus.RT = grp_stimulus_rt, Grp_Stimulus.RESP = grp_response) %>% 
  select(ResponseID, StartDate, EndDate, Finished, uid, R2FItemNum,
         Ind_Stimulus.RESP, Ind_Stimulus.OnsetTime, 
         Ind_Stimulus.OffsetTime, Ind_Stimulus.RT,
         Wait.RT, Wait.OnsetTime, Wait.OffsetTime,
         Grp_Stimulus.RESP, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Stimulus.RT) %>%
  mutate(version = "b2")

tmp5 <- surveys_multi$r2f_neg_multi[ , c(-29, -30)] %>% 
      rename(ResponseID = ResponseId, R2FItemNum = itemnum,
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Wait.OnsetTime = wait_onset,
         Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
         Grp_Stimulus.OnsetTime = grp_stimulus_onset, Grp_Stimulus.OffsetTime = grp_stimulus_offset,
         Grp_Stimulus.RT = grp_stimulus_rt, Grp_Stimulus.RESP = grp_response) %>% 
  select(ResponseID, StartDate, EndDate, Finished, uid, R2FItemNum,
         Ind_Stimulus.RESP, Ind_Stimulus.OnsetTime, 
         Ind_Stimulus.OffsetTime, Ind_Stimulus.RT,
         Wait.RT, Wait.OnsetTime, Wait.OffsetTime,
         Grp_Stimulus.RESP, Grp_Stimulus.OnsetTime, 
         Grp_Stimulus.OffsetTime, Grp_Stimulus.RT) %>%
  mutate(version = "b_multi")

# uids that completed surveys with multiple JSON columns are included in this data
# the parsing function does not work so all values are NA need to remove and read in separately
uids <- c("18073109_1_d2", "18073109_1_g2", "18073110_2_d1", "18073110_2_g1", "18073114_1_d2",
          "18073114_1_g2", "18080114_1_e1", "18080114_1_e2")

tmp4 <- tmp4 %>% filter(!uid %in% uids)

# combine data into a single df 
r2f_neg <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5)

# Data Screening ----------------------------------------------------------
# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_d1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_g1"   = 1 unfinished attempt, the other 2 attempts are legit, P from 1pm session incorrectly assigned same uid
#   "18062912_1_e2"   = 1 finished and 1 unfinished attempt
#'
#'  SOLUTION:
#'  ResponseId == R_3xFlfOk2utCxau1 becomes uid == 18041213_1_d2
#'  ResponseId == R_cOpi5xYvyr4w6fb becomes uid == 18041213_1_g2
#'  ResponseId == R_1XP3qHaowkQ73Fz becomes uid == 18041213_2_d1
#'  ResponseId == R_9tnY84dolnnTp4N becomes uid == 18041213_2_g1
r2f_neg <- r2f_neg %>% 
  ungroup() %>% 
  mutate(Finished = ifelse(Finished == 1, "True",
                           ifelse(Finished == 0, "False", Finished)),
         uid = ifelse(ResponseID == "R_3xFlfOk2utCxau1", "18041213_1_d2", uid),
         uid = ifelse(ResponseID == "R_cOpi5xYvyr4w6fb", "18041213_1_g2", uid),
         uid = ifelse(ResponseID == "R_1XP3qHaowkQ73Fz", "18041213_2_d1", uid),
         uid = ifelse(ResponseID == "R_9tnY84dolnnTp4N", "18041213_2_g1", uid)) %>% 
  filter(Finished == "True") %>% 
  filter(uid != "18041210_2_g1") %>% # this uid is not in r2f pos so remove
  filter(uid != "18041210_2_d1") # this uid is not in r2f pos so remove

n_questions <- 7 # number of questions in this test

x <- table(r2f_neg$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- r2f_neg %>%
  select(uid, R2FItemNum, Ind_Stimulus.RESP, Grp_Stimulus.RESP)

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
  x[row_miss, ] # to print uid if there is a missing value
}  else {
  print("no missing values")
}

# visually check that there are no missing values per user per variable
x %>%
  group_by(uid) %>% 
  summarise_all(funs(sum(is.na(.)))) %>% 
  gather(key, val, -uid) %>% 
  ggplot(aes(key, uid, fill = factor(val))) +
  geom_tile() +
  scale_fill_brewer(palette = "OrRd") +
  labs(title = "Number of missing values per user, per variable", x = NULL, y = NULL, fill = NULL) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Check whether vector contains acceptable values () ------------------------
# First, convert to numeric vectors
r2f_neg <- r2f_neg %>% 
  mutate(Ind_Stimulus.RESP = as.numeric(Ind_Stimulus.RESP),
         Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP))

check_values <- function(x, num_min, num_max = NULL) {
  if (is.null(num_max)) {
    any_wrong <- !all(x %in% num_min)
  } else {
    any_wrong <- any(x < num_min | x > num_max, na.rm = TRUE)
  }
  
  if (any_wrong) {
    print("Unacceptable value present in this variable.")
  } else {
    print("All good! :)")
  }
}

check_values(r2f_neg$Ind_Stimulus.RESP, 0, 6)
check_values(r2f_neg$Grp_Stimulus.RESP, 0, 6)


# Check teams are giving same response and decision =======================
# Create grouping variables 
# group == date + group
# member == g == 1, d == 2
r2f_neg <- r2f_neg %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                      ifelse(grepl("d[1-2]", uid), "2", 
                                             ifelse(grepl("e1", uid), "1", "2"))),
                      recruit = ifelse(grepl("[dg]", uid), "sona", "ext"),
                      group = gsub("_[a-z][12]", "", uid))


# x <- r2f_neg %>% 
#   group_by(group, R2FItemNum) %>% 
#   mutate(match = Grp_Stimulus.RESP[1] != Grp_Stimulus.RESP[2]) %>% 
#   group_by(group) %>% 
#   mutate(n = sum(match))

# # to see how many times each team entered different group responses
# x %>%
#   select(group, n, version) %>%
#   group_by(group) %>%
#   summarise(n = n[1] / 2,
#             version = version[1]) %>%
#   arrange(desc(n))

# Stimulus
# in cases where either is different change to missing for all variables 
# (Grp_Stimulus.RESP)
# Justification: If the response differs for members it is impossible to
# determine which response was agreed upon by members.
x <- r2f_neg %>% select(group, member, recruit, R2FItemNum, Grp_Stimulus.RESP) %>% 
  spread(member, Grp_Stimulus.RESP) %>%
  group_by(group, R2FItemNum) %>% 
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group accuracy differed between members
cat("number of times that group accuracy differed between members:", nrow(x))


for (i in seq(nrow(x))){
  group <- x$group[i]
  R2FItemNum <- x$R2FItemNum[i]
  r2f_neg[r2f_neg$group == group & r2f_neg$R2FItemNum == R2FItemNum, 
      c("Grp_Stimulus.RESP", "Grp_Stimulus.OnsetTime", "Grp_Stimulus.OffsetTime",
        "Grp_Stimulus.RT")] <- NA
}

# calculate RTs for to numeric and compute team RTs
r2f_neg <- r2f_neg %>% 
  group_by(uid, R2FItemNum) %>%
  mutate(
    # convert RT variables to numeric
    Ind_Stimulus.OffsetTime = as.numeric(Ind_Stimulus.OffsetTime),
    Ind_Stimulus.OnsetTime = as.numeric(Ind_Stimulus.OnsetTime),
    Grp_Stimulus.OffsetTime = as.numeric(Grp_Stimulus.OffsetTime),
    Grp_Stimulus.OnsetTime = as.numeric(Grp_Stimulus.OnsetTime),
    # calculate individual RTs: offset-onset
    Ind_Stimulus.RT = Ind_Stimulus.OffsetTime - Ind_Stimulus.OnsetTime) %>% 
  group_by(group, R2FItemNum) %>% 
  mutate(
    # calculate team RTs: min(offset) - min(onset)
    Grp_Stimulus.RT = min(Grp_Stimulus.OffsetTime) - min(Grp_Stimulus.OnsetTime))


# visualise RTs for each participant
# very slow RTs are not an issue
# uid == 18081613_2_e1 had 6 individual responses in less than 3 seconds. This is too fast to take in the information.
# one group had 5 reponses in less than 5 seconds, group == 18081613_2
# r2f_neg %>%
#   ggplot(aes(Ind_Stimulus.RT, colour = R2FItemNum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(r2f_neg) +
#   geom_point(aes(x = Ind_Stimulus.RT, y = R2FItemNum))
# 
# r2f_neg %>%
#   filter(Ind_Stimulus.RT < 5000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# r2f_neg %>%
#   filter(Ind_Stimulus.RT < 5000) %>%
#   ggplot() +
#   geom_point(aes(x = R2FItemNum, y = Ind_Stimulus.RT)) +
#   facet_wrap(~ uid)
# 
# r2f_neg %>%
#   ggplot(aes(Grp_Stimulus.RT, colour = R2FItemNum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(r2f_neg) +
#   geom_point(aes(x = Grp_Stimulus.RT, y = R2FItemNum))
# 
# r2f_neg %>%
#   filter(Grp_Stimulus.RT < 1000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# r2f_neg %>%
#   filter(Grp_Stimulus.RT < 1000) %>%
#   ggplot() +
#   geom_point(aes(x = R2FItemNum, y = Grp_Stimulus.RT)) +
#   facet_wrap(~ uid)

# integrity check: calculate the proportion of times each team entered the 
# same response individually. If very high (>=.85) then need to investigate further 
# need to check audio to see if they were discussing individual responses
# No issues detected
r2f_neg %>% 
  group_by(group, R2FItemNum) %>% 
  summarise(same_resp = Ind_Stimulus.RESP[1] == Ind_Stimulus.RESP[2]) %>% 
  group_by(group) %>% 
  summarise(n_same = sum(same_resp),
            items = n()) %>% 
  group_by(group) %>% 
  summarise(prop_same = n_same/items) %>% 
  filter(prop_same >= .85) %>% 
  arrange(desc(prop_same))

# Reliability ----------------------------------------------------
# # FOR INDIVIDUALS
# Negative
# x <- r2f_neg %>%
#   ungroup() %>% 
#   select(uid, R2FItemNum, Ind_Stimulus.RESP) %>% 
#   spread(R2FItemNum, Ind_Stimulus.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # # FOR GROUPS
# # Negative
# x <- r2f_neg %>%
#   ungroup() %>% 
#   select(uid, R2FItemNum, Grp_Stimulus.RESP) %>% 
#   spread(R2FItemNum, Grp_Stimulus.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha

# # Save uid cleaned data as RDS for quick load -----------------------------
# saveRDS(r2f_neg, "data/rds/r2f_neg.rds")
# 
# }

