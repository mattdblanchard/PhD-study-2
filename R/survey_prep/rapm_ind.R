# RAPM_individual -----------------------------------------------
tmp1 <- as.data.frame(surveys_qrt$RAPM_ind_A1 %>%
            select(ResponseID, StartDate:Finished, uid, ApmItemNum, Ind_Stimulus.CRESP, 
                   Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
                   Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
                   Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
                   Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
            rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
            mutate(version = "a1") %>%
            group_by(uid, ApmItemNum) %>%
            mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
                   Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
                   Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime))

tmp2 <- as.data.frame(surveys_qrt$RAPM_ind_B1 %>%
            select(ResponseID, StartDate:Finished, uid, ApmItemNum, Ind_Stimulus.CRESP, 
                   Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
                   Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
                   Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
                   Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
            rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
            mutate(version = "b1") %>%
            group_by(uid, ApmItemNum) %>%
            mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
                   Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
                   Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime))

tmp3 <- surveys_java$RAPM_ind_A2 %>%
            rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ApmItemNum = itemnum, 
                 Ind_Stimulus.OnsetTime = ind_stimulus_onset,
                 Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
                 Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
                 Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
                 Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
                 Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
                 Ind_Decision.RESP = ind_decision) %>%
          select(ResponseID, StartDate, EndDate, Finished, uid, ApmItemNum, Stimulus.CRESP, 
                 Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
                 Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
                 Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
          mutate(version = "a2")

tmp4 <- surveys_java$RAPM_ind_B2 %>%
        rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ApmItemNum = itemnum, 
               Ind_Stimulus.OnsetTime = ind_stimulus_onset,
               Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
               Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
               Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
               Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
               Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
               Ind_Decision.RESP = ind_decision) %>%
        select(ResponseID, StartDate, EndDate, Finished, uid, ApmItemNum, Stimulus.CRESP, 
               Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
               Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
               Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
        mutate(version = "b2")

tmp5 <- surveys_multi$rapm_ind_multi %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ApmItemNum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, ApmItemNum, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
  mutate(version = "b_multi")

# uids that completed surveys with multiple JSON columns are included in this data
# the parsing function does not work so all values are NA need to remove and read in separately
uids <- c("18073109_1_d2", "18073109_1_g2", "18073110_2_d1", "18073110_2_g1", "18073114_1_d2",
          "18073114_1_g2", "18080114_1_e1", "18080114_1_e2")

tmp4 <- tmp4 %>% filter(!uid %in% uids)

# combine data into a single df 
rapm_ind <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5)

# Data Screening ----------------------------------------------------------
# identified duplicated records for most participants created when parsing
rapm_ind <- rapm_ind %>% filter(!is.na(ApmItemNum))

# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_d1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_g1"   = all responses legit, P from 1pm session incorrectly assigned same uid

# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_0JwbMy4HDQljoQR becomes 18041213_1_d2
# new uid for ResponseID = R_1MPeDnhE9yES2IB becomes 18041213_1_g2
# new uid for ResponseID = R_4UPYMp1ohvpPNPL becomes 18041213_1_d1
# new uid for ResponseID = R_7R4dstNm0GW6oxn becomes 18041213_1_g1

# ISSUE 2: Identified that uid == "18042010_1_d2" has all NAs for Q6 (Itemnum == 11)
# SOLUTION 2. 
# Remove this Q from their responses so they have responses for 17 Qs only
rapm_ind <- rapm_ind %>% mutate(uid = ifelse(ResponseID == "R_0JwbMy4HDQljoQR", "18041213_1_d2", uid),
                                uid = ifelse(ResponseID == "R_1MPeDnhE9yES2IB", "18041213_1_g2", uid),
                                uid = ifelse(ResponseID == "R_4UPYMp1ohvpPNPL", "18041213_2_d1", uid),
                                uid = ifelse(ResponseID == "R_7R4dstNm0GW6oxn", "18041213_2_g1", uid))

n_questions <- 18 # number of questions in this test

x <- table(rapm_ind$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}


# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- rapm_ind %>%
  select(uid, ApmItemNum, Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Confidence.RESP, Ind_Decision.RESP)

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
# uid == "17102710_1_d2" entered "60y" for one confidence rating. Change to "60"
rapm_ind <- rapm_ind %>% mutate(Ind_Confidence.RESP = ifelse(uid == "17102710_1_d2" & ApmItemNum == "7", "60", Ind_Confidence.RESP))

rapm_ind <- rapm_ind %>% mutate(Ind_Stimulus.RESP = as.numeric(Ind_Stimulus.RESP),
                                Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
                                Ind_Decision.RESP = tolower(Ind_Decision.RESP))

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

check_values(rapm_ind$Ind_Stimulus.RESP, 1, 8)
check_values(rapm_ind$Ind_Confidence.RESP, 0, 100)
check_values(rapm_ind$Ind_Decision.RESP, c("y", "n"))

# Calculate variables
# some Stimulus.CRESP values were missing
rapm_ind <- rapm_ind %>%  
  mutate(Stimulus.CRESP = ifelse(ApmItemNum %in% c("13", "15"), "2", 
                                 ifelse(ApmItemNum %in% c("5", "19", "35"), "3",
                                        ifelse(ApmItemNum %in% c("31"), "4",
                                               ifelse(ApmItemNum %in% c("1", "11", "33"), "5",
                                                      ifelse(ApmItemNum %in% c("7", "17", "23", "29"), "6",
                                                             ifelse(ApmItemNum %in% c("3", "25", "27"), "7",
                                                                    ifelse(ApmItemNum %in% c("9", "21"), "8", Stimulus.CRESP))))))))


# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
rapm_ind <- rapm_ind %>%
  group_by(uid, ApmItemNum) %>%
  mutate(Ind_Stimulus.ACC = Ind_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()

# Create grouping variables -----------------------------------------------
# group == date + group
# member == g == 1, d == 2
rapm_ind <- rapm_ind %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                                ifelse(grepl("d[1-2]", uid), "2", # change to team member 1 and member 2
                                                       ifelse(grepl("e1", uid), "1", "2"))), # change to team member 1 and member 2
                                recruit = ifelse(grepl("[dg]", uid), "sona", "ext"), # recruitment source
                                group = gsub("_[a-z][12]", "", uid)) # create group var


# convert RT vars to numeric
rapm_ind <- rapm_ind %>%
  group_by(uid, ApmItemNum) %>% 
  mutate(
  # convert RT variables to numeric
  Ind_Stimulus.OffsetTime = as.numeric(Ind_Stimulus.OffsetTime),
  Ind_Stimulus.OnsetTime = as.numeric(Ind_Stimulus.OnsetTime),
  Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
  Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
  Ind_Decision.OffsetTime = as.numeric(Ind_Decision.OffsetTime),
  Ind_Decision.OnsetTime = as.numeric(Ind_Decision.OnsetTime),
  # calculate individual RTs: offset-onset
  Ind_Stimulus.RT = Ind_Stimulus.OffsetTime - Ind_Stimulus.OnsetTime,
  Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime,
  Ind_Decision.RT = Ind_Decision.OffsetTime - Ind_Decision.OnsetTime)

# visualise RTs for each participant
# very slow RTs are not an issue
# identified uid == "19013010_1_g1" answered 11 items < 5 secs
# rapm_ind %>%
#   ggplot(aes(Ind_Stimulus.RT, colour = ApmItemNum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(rapm_ind) +
#   geom_point(aes(Ind_Stimulus.RT, ApmItemNum))
# 
# rapm_ind %>%
#   filter(Ind_Stimulus.RT < 5000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# rapm_ind %>%
#   filter(Ind_Stimulus.RT < 5000) %>%
#   ggplot() +
#   geom_point(aes(ApmItemNum, Ind_Stimulus.RT)) +
#   facet_wrap(~ uid)


# integrity check: calculate the proportion of times each team entered the 
# same response individually. If very high (>=.90) then need to investigate further 
# the following code is to investigate pattern of responses and timing of each item attempted
# check <- rapm_ind %>% 
#   filter(group == "18041210_2") %>% 
#   select(uid, ApmItemNum, Ind_Stimulus.ACC, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime) %>% 
#   mutate(Ind_Stimulus.OnsetTime = Ind_Stimulus.OnsetTime / 1000,
#          Ind_Stimulus.OffsetTime = Ind_Stimulus.OffsetTime / 1000,
#          Ind_Stimulus.OnsetTime = as.POSIXct(Ind_Stimulus.OnsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"),
#          Ind_Stimulus.OffsetTime = as.POSIXct(Ind_Stimulus.OffsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"))

# identified two potentially problematic groups that entered the same responses to everything
# converting epoch time to readable time reveals the following:
# group == 18062615_1 finished 5 mins apart so unlikely they were working together
# group == 18041210_2 finished each item at the same time and the session notes 
# reveal that they accidentally completed RAPM ind together so I had them complete RAPM team as individuals

rapm_ind %>% 
  group_by(group, ApmItemNum) %>% 
  summarise(same_resp = Ind_Stimulus.RESP[1] == Ind_Stimulus.RESP[2]) %>% 
  group_by(group) %>% 
  summarise(n_same = sum(same_resp),
            items = n()) %>% 
  group_by(group) %>% 
  summarise(prop_same = n_same/items) %>% 
  filter(prop_same >= .9) %>% 
  arrange(desc(prop_same))

# Create CC column --------------------------------------------------------
# need to work out calculation first
# these are 8AFC questions so multiple responses possible
# when majority response is correct = CC
# when majority response is incorrect - CW?
# is Simon's paper on response cardinality relevant?
# im <- rapm_ind %>%
#   group_by(ApmItemNum) %>%
#   summarise(item.mean = mean(Ind_Stimulus.ACC)) %>%
#   mutate(cc = ifelse(item.mean >= .5, "cc","cw"))
# 
# rapm_ind <- rapm_ind %>% left_join(im)

# Compute all variables ------------------------------------------------
rapm_ind.uid <- rapm_ind %>%
  group_by(uid) %>%
  summarise(
    group = group[1],
    member = member[1],
    recruit = recruit[1],
    version = version[1],
    rapm.ind.acc = mean(Ind_Stimulus.ACC) * 100,
    rapm.ind.conf = mean(Ind_Confidence.RESP),
    rapm.ind.bias = rapm.ind.conf - rapm.ind.acc,
    rapm.ind.discrimination = mean(Ind_Confidence.RESP[Ind_Stimulus.ACC == 1], na.rm = TRUE) - mean(Ind_Confidence.RESP[Ind_Stimulus.ACC == 0], na.rm = TRUE),
    rapm.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
    rapm.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
    rapm.ind.comp = 100 * (sum(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / length(Ind_Stimulus.ACC),
    rapm.ind.optim = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE),
    rapm.ind.hes = 100 * sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE)
    / sum(Ind_Stimulus.ACC == 1, na.rm = TRUE))


# calculate control thresholds 
# Logisitic regression: use confidence to predict the point at which a bet decision switches from no to yes
# Great tutorial for conductin logistic regression: https://www.datacamp.com/community/tutorials/logistic-regression-R
library(broom)

# prepare individual data
data <- rapm_ind %>%
  ungroup() %>% 
  select(uid, ApmItemNum, Ind_Confidence.RESP, Ind_Decision.RESP) %>%
  group_by(uid) %>% 
  filter(!all(is.na(Ind_Decision.RESP))) %>% 
  mutate(
    Ind_Decision.RESP = as.numeric(ifelse(Ind_Decision.RESP == "y", 1,
                                          ifelse(Ind_Decision.RESP == "n", 0, NA))),
    conf_range = abs(max(Ind_Confidence.RESP, na.rm = T) - min(Ind_Confidence.RESP, na.rm = T)),
    dec_range = abs(max(Ind_Decision.RESP, na.rm = T) - min(Ind_Decision.RESP, na.rm = T)))

# remove participants with no varaince in their confidence ratings or bet decisions
data <- data %>% filter(conf_range > 0 | dec_range > 0)

# fit logistic regression model and compute POST
post <- data %>% 
  nest() %>% 
  mutate(fit = map(data, ~ glm(Ind_Decision.RESP ~ Ind_Confidence.RESP, data = ., family = binomial)),
         coef = map(fit, ~ tidy(.)$estimate),
         coef = map(coef, ~ data.frame(a = .[[1]], b = .[2]))) %>% 
  unnest(coef) %>% 
  group_by(uid) %>% 
  summarise(rapm.ind.post = (-a)/b) %>% 
  filter(rapm.ind.post >= 0 & rapm.ind.post <= 100)

# join individual post data to test dataset
rapm_ind.uid <- rapm_ind.uid %>% left_join(post, by = "uid")


# Reliability ----------------------------------------------------
# remove teams that made non-genuine attempt
# rapm_ind <- rapm_ind %>% filter(!group %in% c("18041210_2", "18080915_1", "18072412_1", "19013010_1", "17102710_2", "18081613_1"))
# # #
# # # Accuracy 
# x <-  rapm_ind %>%
#   select(uid, ApmItemNum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1,
#                                    ifelse(Ind_Stimulus.ACC == FALSE, 0, Ind_Stimulus.ACC))) %>%
#   spread(ApmItemNum, Ind_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Confidence
# x <- rapm_ind %>%
#   select(uid, ApmItemNum, Ind_Confidence.RESP) %>%
#   spread(ApmItemNum, Ind_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Decisiveness
# x <- rapm_ind %>%
#   select(uid, ApmItemNum, Ind_Decision.RESP)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# x <- x %>%
#   spread(ApmItemNum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# x <- rapm_ind %>%
#   select(uid, ApmItemNum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# odd1 <- c(1, 5, 9, 13, 17, 21, 25, 29, 33)
# 
# odd <- filter(x, ApmItemNum %in% odd1)
# 
# even1 <- c(3, 7, 11, 15, 19, 23, 27, 31, 35)
# 
# even <- filter(x, ApmItemNum %in% even1)
# 
# even <- even %>%
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# odd <- odd %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# # # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.5756426) / (1+0.5756426)

