# MDMT_individual -----------------------------------------------
tmp1 <- as.data.frame(surveys_qrt$MDMT_ind_A1 %>%
            select(ResponseID, StartDate:Finished, uid, MdmtItemNum, Ind_Stimulus.CRESP, 
                   Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
                   Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
                   Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
                   Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
            rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
            mutate(version = "a1") %>%
            group_by(uid, MdmtItemNum) %>%
            mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
                   Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
                   Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime))

tmp2 <- as.data.frame(surveys_qrt$MDMT_ind_B1 %>%
            select(ResponseID, StartDate:Finished, uid, MdmtItemNum, Ind_Stimulus.CRESP, 
                   Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, 
                   Ind_Stimulus.OffsetTime, Ind_Confidence.RESP, Ind_Confidence.OnsetTime, 
                   Ind_Confidence.OffsetTime, Ind_Decision.RESP, Ind_Decision.RT, 
                   Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
            rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
            mutate(version = "b1") %>%
            group_by(uid, MdmtItemNum) %>%
            mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
                   Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
                   Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime))

tmp3 <- surveys_java$MDMT_ind_A2 %>%
            rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, MdmtItemNum = itemnum, 
                   Ind_Stimulus.OnsetTime = ind_stimulus_onset,
                   Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
                   Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
                   Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
                   Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
                   Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
                   Ind_Decision.RESP = ind_decision) %>%
            select(ResponseID, StartDate, EndDate, Finished, uid, MdmtItemNum, Stimulus.CRESP, 
                   Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
                   Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
                   Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
            mutate(version = "a2")

tmp4 <- surveys_java$MDMT_ind_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, MdmtItemNum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, MdmtItemNum, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime) %>%
  mutate(version = "b2")

tmp5 <- surveys_multi$mdmt_ind_multi %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, MdmtItemNum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, MdmtItemNum, Stimulus.CRESP, 
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
mdmt_ind <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5)

# Data Screening ----------------------------------------------------------
# identified duplicated records for most participants created when parsing
# also identified a test uid
mdmt_ind <- mdmt_ind %>% 
  filter(!is.na(MdmtItemNum)) %>% 
  filter(!str_detect(uid, "TEST"))

# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_d1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_g1"   = all responses legit, P from 1pm session incorrectly assigned same uid

# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_afVdc7n6UjyZKS1 becomes 18041213_1_d2
# new uid for ResponseID = R_4Nuw8JPU5ExWxUh becomes 18041213_1_g2
# new uid for ResponseID = R_6yUMj3s9d3otiGV becomes 18041213_1_d1
# new uid for ResponseID = R_9snmMnu9ZKkZJn7 becomes 18041213_1_g1

# ISSUE 2: Identified that uid == "17103012_2_g1" appears twice
# The browser crashed so I had to restart the survey
#
# SOLUTION 2. 
# For uid == "17103012_2_g1" take responses for Q1-3 from 1st attempt 
# ResponseID == "R_dnWhJuwpMkBcVQF" and join with responses for Q4-16 
# from 2nd attemp ResponseID == "R_eRnsJy1NFudWPnT" then remove first attempt

# Which variables will I replace?
vars <- c("Ind_Stimulus.RESP", "Ind_Stimulus.RT", "Ind_Stimulus.OnsetTime", 
          "Ind_Stimulus.OffsetTime", "Ind_Confidence.RESP", "Ind_Confidence.RT", 
          "Ind_Confidence.OnsetTime", "Ind_Confidence.OffsetTime", "Ind_Decision.RESP",
          "Ind_Decision.RT", "Ind_Decision.OffsetTime", "Ind_Decision.OnsetTime")

# For uid == "18040910_2_g1" get Attempt 1 responses
x <- mdmt_ind %>% filter(ResponseID == "R_dnWhJuwpMkBcVQF")

# x <- arrange(x, Itemnum)  # arrange by trial number

x <- x[1:3, vars]  # Take rows 1 to 3 (corresponding to answered questions)
# and variables of interest

itemnum <- c("1", "2", "26")

# Insert data from x into relevant part of df
mdmt_ind[mdmt_ind$ResponseID == "R_eRnsJy1NFudWPnT" & mdmt_ind$MdmtItemNum %in% itemnum, vars] <- x

# Modify uids and remove duplicate attempts
mdmt_ind <- mdmt_ind %>% mutate(uid = ifelse(ResponseID == "R_afVdc7n6UjyZKS1", "18041213_1_d2", uid),
                                uid = ifelse(ResponseID == "R_4Nuw8JPU5ExWxUh", "18041213_1_g2", uid),
                                uid = ifelse(ResponseID == "R_6yUMj3s9d3otiGV", "18041213_2_d1", uid),
                                uid = ifelse(ResponseID == "R_9snmMnu9ZKkZJn7", "18041213_2_g1", uid)) %>%
  filter(ResponseID != "R_dnWhJuwpMkBcVQF")

n_questions <- 16 # number of questions in this test

x <- table(mdmt_ind$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}


# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- mdmt_ind %>%
  select(uid, MdmtItemNum, Ind_Stimulus.RESP, Ind_Confidence.RESP, Ind_Decision.RESP)

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
mdmt_ind <- mdmt_ind %>% mutate(Ind_Stimulus.RESP = as.numeric(Ind_Stimulus.RESP),
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

check_values(mdmt_ind$Ind_Stimulus.RESP, 0, 3)
check_values(mdmt_ind$Ind_Confidence.RESP, 25, 100)
check_values(mdmt_ind$Ind_Decision.RESP, c("a", "b"))

# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
mdmt_ind <- mdmt_ind %>% 
  group_by(uid, MdmtItemNum) %>%
  mutate(Ind_Stimulus.ACC = Ind_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()

# Create grouping variables -----------------------------------------------
# group == date + group
# member == g == 1, d == 2
mdmt_ind <- mdmt_ind %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                                ifelse(grepl("d[1-2]", uid), "2", 
                                                       ifelse(grepl("e1", uid), "1", "2"))),
                                recruit = ifelse(grepl("[dg]", uid), "sona", "ext"),
                                group = gsub("_[a-z][12]", "", uid))

# convert RT vars to numeric
mdmt_ind <- mdmt_ind %>%
group_by(uid, MdmtItemNum) %>% 
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

# Calculate average time on task
# add 3 mins for memorising stimulus
# mdmt_ind %>% 
#   select(uid, contains(".RT")) %>% 
#   gather(var, val, -uid) %>% 
#   group_by(uid) %>% 
#   summarise(time = sum(val)/1000/60) %>% 
#   ungroup() %>% 
#   summarise(time = mean(time))


# visualise RTs for each participant
# very slow RTs are not an issue
# identified uid == "19021212_1_d1" answered 11 items < 1 sec
# uid == "18082111_1_e2" answered 4 items < 1 sec
# mdmt_ind %>%
#   ggplot(aes(Ind_Stimulus.RT, colour = MdmtItemNum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(mdmt_ind) +
#   geom_point(aes(Ind_Stimulus.RT, MdmtItemNum))
# 
# mdmt_ind %>%
#   filter(Ind_Stimulus.RT < 1000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# mdmt_ind %>%
#   filter(Ind_Stimulus.RT < 1000) %>%
#   ggplot() +
#   geom_point(aes(MdmtItemNum, Ind_Stimulus.RT)) +
#   facet_wrap(~ uid)

# integrity check: calculate the proportion of times each team entered the 
# same response individually. If very high (>=.90) then need to investigate further 
# may need to check audio to see if they were discussing individual responses
# the following code is to investigate pattern of responses and timing of each item attempted
# check <- mdmt_ind %>%
#   filter(group == "18080910_1") %>%
#   select(uid, MdmtItemNum, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime) %>%
#   mutate(Ind_Stimulus.OnsetTime = Ind_Stimulus.OnsetTime / 1000,
#          Ind_Stimulus.OffsetTime = Ind_Stimulus.OffsetTime / 1000,
#          Ind_Stimulus.OnsetTime = as.POSIXct(Ind_Stimulus.OnsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"),
#          Ind_Stimulus.OffsetTime = as.POSIXct(Ind_Stimulus.OffsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"))

# identified three potentially problematic groups however item timings make 
# illegal collaboration highly unlikely for at least 2 groups
# group == 19013110_1 entered the same individual responses for all 16 questions
# all were correct except 1 - both got the same single question wrong
# inspection of response timings reveal that team members tended to complete the 
# items about 30 seconds apart so unlikely they were working together
mdmt_ind %>% 
  group_by(group, MdmtItemNum) %>% 
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
# these are 4AFC questions so multiple responses possible
# when majority response is correct = CC
# when majority response is incorrect - CW?
# is Simon's paper on response cardinality relevant?
# im <- mdmt_ind %>%
#   group_by(MdmtItemNum) %>%
#   summarise(item.mean = mean(Ind_Stimulus.ACC)) %>%
#   mutate(cc = ifelse(item.mean >= .5, "cc","cw"))
# 
# mdmt_ind <- mdmt_ind %>% left_join(im)


# Compute all variables ------------------------------------------------
mdmt_ind.uid <- mdmt_ind %>%
  group_by(uid) %>%
  summarise(
    # group = group[1],
    mdmt.ind.acc = mean(Ind_Stimulus.ACC) * 100,
    mdmt.ind.conf = mean(Ind_Confidence.RESP),
    mdmt.ind.bias = mdmt.ind.conf - mdmt.ind.acc,
    mdmt.ind.discrimination = mean(Ind_Confidence.RESP[Ind_Stimulus.ACC == 1], na.rm = TRUE) - mean(Ind_Confidence.RESP[Ind_Stimulus.ACC == 0], na.rm = TRUE),
    mdmt.ind.dec = mean(Ind_Decision.RESP == "a") * 100,
    mdmt.ind.rec = 100 * mean(Ind_Decision.RESP == "a" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
    mdmt.ind.comp = 100 * (sum(Ind_Decision.RESP == "a" & Ind_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Ind_Decision.RESP == "b" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / length(Ind_Stimulus.ACC),
    mdmt.ind.optim = 100 * mean(Ind_Decision.RESP == "a" & Ind_Stimulus.ACC == 1, na.rm = TRUE),
    mdmt.ind.hes = 100 * sum(Ind_Decision.RESP == "b" & Ind_Stimulus.ACC == 1, na.rm = TRUE)
    / sum(Ind_Stimulus.ACC == 1, na.rm = TRUE))


# calculate control thresholds 
# Logisitic regression: use confidence to predict the point at which a bet decision switches from no to yes
# Great tutorial for conductin logistic regression: https://www.datacamp.com/community/tutorials/logistic-regression-R
library(broom)

# prepare individual data
data <- mdmt_ind %>%
  ungroup() %>% 
  select(uid, MdmtItemNum, Ind_Confidence.RESP, Ind_Decision.RESP) %>%
  group_by(uid) %>% 
  filter(!all(is.na(Ind_Decision.RESP))) %>% 
  mutate(
    Ind_Decision.RESP = as.numeric(ifelse(Ind_Decision.RESP == "a", 1,
                                          ifelse(Ind_Decision.RESP == "b", 0, NA))),
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
  summarise(mdmt.ind.post = (-a)/b) %>% 
  filter(mdmt.ind.post >= 0 & mdmt.ind.post <= 100)

# join individual post data to test dataset
mdmt_ind.uid <- mdmt_ind.uid %>% left_join(post, by = "uid")


# Reliability ----------------------------------------------------
# remove teams that made non-genuine attempt
# mdmt_ind <- mdmt_ind %>% filter(!group %in% c("18041210_2", "18080915_1", "18072412_1", "19013010_1", "17102710_2", "18081613_1"))
# #
# # # Accuracy 
# x <-  mdmt_ind %>%
#   select(uid, MdmtItemNum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(MdmtItemNum, Ind_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Confidence
# x <- mdmt_ind %>%
#   select(uid, MdmtItemNum, Ind_Confidence.RESP) %>%
#   spread(MdmtItemNum, Ind_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Decisiveness
# x <- mdmt_ind %>%
#   select(uid, MdmtItemNum, Ind_Decision.RESP)
# 
# x[x == "a"] <- 1
# x[x == "b"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# x <- x %>%
#   spread(MdmtItemNum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# x <- mdmt_ind %>%
#   select(uid, MdmtItemNum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC)
# 
# x[x == "a"] <- 1
# x[x == "b"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# odd1 <- c(1, 26, 15, 14, 5, 29, 17, 31)
# 
# odd <- filter(x, MdmtItemNum %in% odd1)
# 
# even1 <- c(2, 23, 33, 24, 11, 8, 20, 12)
# 
# even <- filter(x, MdmtItemNum %in% even1)
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
# # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.4793135) / (1+0.4793135)
