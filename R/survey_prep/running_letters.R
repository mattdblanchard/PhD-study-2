# Running_Letters -----------------------------------------------
tmp1 <- surveys_qrt$Running_Letters_A1 %>% 
  select(ResponseID, StartDate:Finished, uid, stimulus:nRecall, 
         RUNNINGtest.TrialNr, Stimulus.CRESP, Stimulus.RESP, Stimulus.ACC, 
         Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime) %>% 
  group_by(uid, RUNNINGtest.TrialNr) %>% 
  mutate(version = "a1",
         Stimulus.ACC = Stimulus.RESP == Stimulus.CRESP,
         Stimulus.OnsetTime = as.numeric(Stimulus.OnsetTime),
         Stimulus.OffsetTime = as.numeric(Stimulus.OffsetTime),
         Stimulus.RT = Stimulus.OffsetTime - Stimulus.OnsetTime)

tmp2 <- surveys_qrt$Running_Letters_B1 %>%
  select(ResponseID, StartDate:Finished, uid, stimulus:nRecall, 
         RUNNINGtest.TrialNr, Stimulus.CRESP, Stimulus.RESP, Stimulus.ACC, 
         Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime) %>% 
  group_by(uid, RUNNINGtest.TrialNr) %>% 
  mutate(version = "b1",
         Stimulus.ACC = Stimulus.RESP == Stimulus.CRESP,
         Stimulus.OnsetTime = as.numeric(Stimulus.OnsetTime),
         Stimulus.OffsetTime = as.numeric(Stimulus.OffsetTime),
         Stimulus.RT = Stimulus.OffsetTime - Stimulus.OnsetTime)


# two variables (RUNNINGtest.TrialNr, stimulus) were not recorded in 
# the java versions of this test so they will be copied from the qrt versions
items <- tmp1 %>% ungroup() %>% distinct(RUNNINGtest.TrialNr, stimulus, Stimulus.CRESP) %>% drop_na()

tmp3 <- surveys_java$running_letters_A2 %>% 
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp,
         Stimulus.OnsetTime = onset, Stimulus.OffsetTime = offset, Stimulus.RT = rt,
         Stimulus.RESP = response) %>% 
  left_join(items, by = "Stimulus.CRESP") %>% 
  select(ResponseID, StartDate, EndDate, Finished, uid, stimulus, nTotal, nRecall, 
         RUNNINGtest.TrialNr, Stimulus.CRESP, Stimulus.RESP, 
         Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime) %>% 
  group_by(uid, RUNNINGtest.TrialNr) %>% 
  mutate(version = "a2",
         Stimulus.ACC = Stimulus.RESP == Stimulus.CRESP,
         Stimulus.RT = Stimulus.OffsetTime - Stimulus.OnsetTime)

tmp4 <- surveys_java$running_letters_B2 %>% 
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp,
         Stimulus.OnsetTime = onset, Stimulus.OffsetTime = offset, Stimulus.RT = rt,
         Stimulus.RESP = response) %>% 
  left_join(items, by = "Stimulus.CRESP") %>% 
  select(ResponseID, StartDate, EndDate, Finished, uid, stimulus, nTotal, nRecall, 
         RUNNINGtest.TrialNr, Stimulus.CRESP, Stimulus.RESP, 
         Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime) %>% 
  group_by(uid, RUNNINGtest.TrialNr) %>% 
  mutate(version = "b2",
         Stimulus.ACC = Stimulus.RESP == Stimulus.CRESP,
         Stimulus.RT = Stimulus.OffsetTime - Stimulus.OnsetTime)

# combine data into a single df 
run <- rbind(tmp1, tmp2, tmp3, tmp4)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_d1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_g1"   = all responses legit, P from 1pm session incorrectly assigned same uid

# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_cI9b1jVk2QDBRDn becomes 18041213_1_d2
# new uid for ResponseID = R_3xWkSRTNLMQ1Qup becomes 18041213_1_g2
# new uid for ResponseID = R_bqHYg5PkyjWhzwx becomes 18041213_2_d1
# new uid for ResponseID = R_8oHkgA4tnizQZJX becomes 18041213_2_g1

# Modify uids and remove unfinished attempts
run <- run %>% 
  ungroup() %>% 
  mutate(Finished = ifelse(Finished == 1, "True",
                           ifelse(Finished == 0, "False", Finished)),
         uid = ifelse(ResponseID == "R_cI9b1jVk2QDBRDn", "18041213_1_d2", uid),
         uid = ifelse(ResponseID == "R_3xWkSRTNLMQ1Qup", "18041213_1_g2", uid),
         uid = ifelse(ResponseID == "R_bqHYg5PkyjWhzwx", "18041213_2_d1", uid),
         uid = ifelse(ResponseID == "R_8oHkgA4tnizQZJX", "18041213_2_g1", uid)) %>% 
  filter(Finished == "True")

n_questions <- 15 # number of questions in this test

x <- table(run$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- run %>%
  select(uid, stimulus, nTotal, nRecall, RUNNINGtest.TrialNr, 
         Stimulus.CRESP, Stimulus.RESP, Stimulus.RT)

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


# visualise RTs for each participant
# impossible to enter reponse before stimuli have been displayed
# very slow RTs are not an issue
# RTs look reasonable
# run %>%
#   ggplot(aes(Stimulus.RT, colour = RUNNINGtest.TrialNr)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(run) +
#   geom_point(aes(x = Stimulus.RT, y = RUNNINGtest.TrialNr))
# 
# run %>%
#   filter(Stimulus.RT < 1000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# run %>%
#   filter(Stimulus.RT < 1000) %>%
#   ggplot() +
#   geom_point(aes(x = RUNNINGtest.TrialNr, y = Stimulus.RT)) +
#   facet_wrap(~ uid)


# calculate variables
run <- run %>%
  mutate(RUNNINGtest.TrialNr = as.numeric(RUNNINGtest.TrialNr))

run.uid <- run %>%
  group_by(uid) %>%
  summarise(wm.acc = 100 * mean(Stimulus.ACC),
            wm.sum = sum(Stimulus.ACC))


# Reliability ----------------------------------------------------
# # Alpha
# x <- run %>%
#   select(uid, RUNNINGtest.TrialNr, Stimulus.ACC) %>%
#   mutate(Stimulus.ACC = as.numeric(Stimulus.ACC)) %>% 
#   spread(RUNNINGtest.TrialNr, Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
