# Composite_Emotions_Task -----------------------------------------------
tmp1 <- as.data.frame(surveys_qrt$CET_A1 %>%
            select(ResponseID, StartDate:Finished, uid, CetItemNum, Stimulus.CRESP, 
                   Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime,
                   Confidence.RESP, Confidence.OnsetTime, Confidence.OffsetTime,
                   Decision.RESP, Decision.RT, Decision.OnsetTime, Decision.OffsetTime) %>%
            mutate(version = "a1") %>%
            group_by(uid, CetItemNum) %>%
            mutate(Confidence.OffsetTime = as.numeric(Confidence.OffsetTime),
                   Confidence.OnsetTime = as.numeric(Confidence.OnsetTime),
                   Confidence.OffsetTime = as.numeric(Confidence.OffsetTime),
                   Confidence.OnsetTime = as.numeric(Confidence.OnsetTime),
                   Confidence.RT = Confidence.OffsetTime - Confidence.OnsetTime,
                   Confidence.RT = Confidence.OffsetTime - Confidence.OnsetTime))

tmp2 <- as.data.frame(surveys_qrt$CET_B1 %>%
            select(ResponseID, StartDate:Finished, uid, CetItemNum, Stimulus.CRESP, 
                   Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime,
                   Confidence.RESP, Confidence.OnsetTime, Confidence.OffsetTime,
                   Decision.RESP, Decision.RT, Decision.OnsetTime, Decision.OffsetTime) %>%
            mutate(version = "b1") %>%
            group_by(uid, CetItemNum) %>%
            mutate(Confidence.OffsetTime = as.numeric(Confidence.OffsetTime),
                   Confidence.OnsetTime = as.numeric(Confidence.OnsetTime),
                   Confidence.OffsetTime = as.numeric(Confidence.OffsetTime),
                   Confidence.OnsetTime = as.numeric(Confidence.OnsetTime),
                   Confidence.RT = Confidence.OffsetTime - Confidence.OnsetTime,
                   Confidence.RT = Confidence.OffsetTime - Confidence.OnsetTime))

tmp3 <- surveys_java$CET_A2 %>%
            rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, CetItemNum = itemnum, 
                   Stimulus.OnsetTime = stimulus_onset,
                   Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt,
                   Stimulus.RESP = response, Confidence.OnsetTime = confidence_onset,
                   Confidence.OffsetTime = confidence_offset, Confidence.RT = confidence_rt,
                   Confidence.RESP = confidence, Decision.OnsetTime = decision_onset,
                   Decision.OffsetTime = decision_offset, Decision.RT = decision_rt,
                   Decision.RESP = decision) %>%
            select(ResponseID, StartDate, EndDate, Finished, uid, CetItemNum, Stimulus.CRESP, 
                   Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime,
                   Confidence.RESP, Confidence.RT, Confidence.OnsetTime, Confidence.OffsetTime,
                   Decision.RESP, Decision.RT, Decision.OnsetTime, Decision.OffsetTime) %>%
            mutate(version = "a2")

tmp4 <- surveys_java$CET_B2 %>%
            rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, CetItemNum = itemnum, 
                   Stimulus.OnsetTime = stimulus_onset,
                   Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt,
                   Stimulus.RESP = response, Confidence.OnsetTime = confidence_onset,
                   Confidence.OffsetTime = confidence_offset, Confidence.RT = confidence_rt,
                   Confidence.RESP = confidence, Decision.OnsetTime = decision_onset,
                   Decision.OffsetTime = decision_offset, Decision.RT = decision_rt,
                   Decision.RESP = decision) %>%
            select(ResponseID, StartDate, EndDate, Finished, uid, CetItemNum, Stimulus.CRESP, 
                   Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime,
                   Confidence.RESP, Confidence.RT, Confidence.OnsetTime, Confidence.OffsetTime,
                   Decision.RESP, Decision.RT, Decision.OnsetTime, Decision.OffsetTime) %>%
            mutate(version = "b2")

tmp5 <- surveys_multi$cet_multi %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, CetItemNum = itemnum, 
         Stimulus.OnsetTime = stimulus_onset,
         Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt,
         Stimulus.RESP = response, Confidence.OnsetTime = confidence_onset,
         Confidence.OffsetTime = confidence_offset, Confidence.RT = confidence_rt,
         Confidence.RESP = confidence, Decision.OnsetTime = decision_onset,
         Decision.OffsetTime = decision_offset, Decision.RT = decision_rt,
         Decision.RESP = decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, CetItemNum, Stimulus.CRESP, 
         Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, Stimulus.OffsetTime,
         Confidence.RESP, Confidence.RT, Confidence.OnsetTime, Confidence.OffsetTime,
         Decision.RESP, Decision.RT, Decision.OnsetTime, Decision.OffsetTime) %>%
  mutate(version = "b_multi")

# uids that completed surveys with multiple JSON columns are included in this data
# the parsing function does not work so all values are NA need to remove and read in separately
uids <- c("18073109_1_d2", "18073109_1_g2", "18073110_2_d1", "18073110_2_g1", "18073114_1_d2",
          "18073114_1_g2", "18080114_1_e1", "18080114_1_e2")

tmp4 <- tmp4 %>% filter(!uid %in% uids)

# combine data into a single df 
cet <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_d1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_g1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#
# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_4V1TDceif1NYMp7 becomes 18041213_1_d2
# new uid for ResponseID = R_ePx4GBGrM8iQzWd becomes 18041213_1_g2
# new uid for ResponseID = R_bIcjEckek10vtu5 becomes 18041213_1_d1
# new uid for ResponseID = R_1O06uBoWXTTiuPj becomes 18041213_1_g1

# Modify uids and remove duplicate attempts
cet <- cet %>% mutate(uid = ifelse(ResponseID == "R_4V1TDceif1NYMp7", "18041213_1_d2", uid),
                      uid = ifelse(ResponseID == "R_ePx4GBGrM8iQzWd", "18041213_1_g2", uid),
                      uid = ifelse(ResponseID == "R_bIcjEckek10vtu5", "18041213_2_d1", uid),
                      uid = ifelse(ResponseID == "R_1O06uBoWXTTiuPj", "18041213_2_g1", uid))  %>% 
  filter(!is.na(CetItemNum))


n_questions <- 36 # number of questions in this test

x <- table(cet$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- cet %>%
  select(uid, CetItemNum, Stimulus.RESP, Confidence.RESP, Decision.RESP)

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

# Check whether vector contains acceptable values ------------------------
# identified that a number of confidence responses contained spaces before the Ps answer
cet <- cet %>% mutate(Stimulus.RESP = as.numeric(Stimulus.RESP),
                      Confidence.RESP = as.numeric(Confidence.RESP),
                      Decision.RESP = tolower(Decision.RESP))

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

check_values(cet$Stimulus.RESP, 0, 6)
check_values(cet$Confidence.RESP, 13, 100)
check_values(cet$Decision.RESP, c("y", "n"))

# convert RT vars to numeric and responses to lower case for scoring
cet <- cet %>%
  group_by(uid, CetItemNum) %>% 
  mutate(
    # convert RT variables to numeric
    Stimulus.OffsetTime = as.numeric(Stimulus.OffsetTime),
    Stimulus.OnsetTime = as.numeric(Stimulus.OnsetTime),
    Confidence.OffsetTime = as.numeric(Confidence.OffsetTime),
    Confidence.OnsetTime = as.numeric(Confidence.OnsetTime),
    Decision.OffsetTime = as.numeric(Decision.OffsetTime),
    Decision.OnsetTime = as.numeric(Decision.OnsetTime),
    # calculate individual RTs: offset-onset
    Stimulus.RT = Stimulus.OffsetTime - Stimulus.OnsetTime,
    Confidence.RT = Confidence.OffsetTime - Confidence.OnsetTime,
    Decision.RT = Decision.OffsetTime - Decision.OnsetTime)

# visualise RTs for each participant
# very slow RTs are not an issue
# Three Ps responded to multiple Qs in < 500ms
# uid == 17103112_2_g1 = 10
# uid == 17103012_2_d1 = 6
# uid == 17103012_2_g1 = 4
# cet %>%
#   ggplot(aes(Stimulus.RT, colour = CetItemNum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(cet) +
#   geom_point(aes(x = Stimulus.RT, y = CetItemNum))
# 
# cet %>%
#   filter(Stimulus.RT < 500) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# cet %>%
#   filter(Stimulus.RT < 1000) %>%
#   ggplot() +
#   geom_point(aes(x = CetItemNum, y = Stimulus.RT)) +
#   facet_wrap(~ uid)

# calculate variables
# need to score CET using scoring method from Mattis Geiger.
# first create emotion categories
cet <- cet %>%
  ungroup() %>% 
  mutate(CetItemNum = as.numeric(CetItemNum))

cet <- cet %>%
  mutate(Stimulus.ACC = Stimulus.RESP == Stimulus.CRESP,
         target_emotion = ifelse(CetItemNum %in% c(10, 13, 16, 28, 31, 34), "happy",
                                 ifelse(CetItemNum %in% c(11, 14, 17, 29, 32, 35), "disgust",
                                        ifelse(CetItemNum %in% c(12, 15, 18, 30, 33, 36), "surprise",
                                               ifelse(CetItemNum %in% c(1, 2, 3, 19, 20, 21), "anger",
                                                      ifelse(CetItemNum %in% c(4, 5, 6, 22, 23, 24), "fear",
                                                             ifelse(CetItemNum %in% c(7, 8, 9, 25, 26, 27), "sad", "unknown")))))),
         emotion.RESP = ifelse(Stimulus.RESP == 1, "happy",
                               ifelse(Stimulus.RESP == 2, "surprise",
                                      ifelse(Stimulus.RESP == 3, "anger",
                                             ifelse(Stimulus.RESP == 4, "fear",
                                                    ifelse(Stimulus.RESP == 5, "sad",
                                                           ifelse(Stimulus.RESP == 6, "disgust", "unknown")))))))


x1 <- cet %>%
  group_by(uid, target_emotion) %>%
  summarise(freq_correct = sum(Stimulus.ACC == TRUE)^2,
            mean_correct = mean(Stimulus.ACC == TRUE))

x2 <- cet %>% 
  group_by(uid, emotion.RESP) %>%
  summarise(freq_resp = n()) %>%
  rename(target_emotion = emotion.RESP)

x <- x1 %>% left_join(x2, by = c("uid", "target_emotion"))

cet.uid <- x %>%
  group_by(uid, target_emotion) %>%
  summarise(mean_correct = 100 * mean_correct,
            unbiased_hit_rate = 100 * (freq_correct / (6 * freq_resp)))

# check with mattis, is it ok to calculate overall accuracy score and unbiased hit rate?
cet.uid <- cet.uid %>%
  group_by(uid) %>%
  summarise(emotion.acc = mean(mean_correct),
            emotion.ubhr = mean(unbiased_hit_rate, na.rm = T))


# Reliability ----------------------------------------------------
# Alpha for each emotion
# emotion <- unique(cet$target_emotion)
# 
# for (i in emotion) {
# x <- cet %>%
#   filter(target_emotion == i) %>% # calculate for each factor
#   select(uid, CetItemNum, Stimulus.RESP) %>%
#   spread(CetItemNum, Stimulus.RESP) %>%
#   select(-uid)
# 
# print(paste0("Alpha for ", i))
# print(psych::alpha(x)$total$raw_alpha)
# 
# }
# 
# # Alpha overall
# # could drop items 2, 5, 8, 11 (neg corr) aand 27 to increase reliabiity to .55
# x <- cet %>%
#   select(uid, CetItemNum, Stimulus.RESP) %>%
#   # filter(!CetItemNum %in% c(2, 5, 8, 11, 27)) %>%
#   spread(CetItemNum, Stimulus.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
