# MDMT_team -----------------------------------------------
tmp1 <- as.data.frame(surveys_qrt$MDMT_team_A1 %>%
            select(ResponseID, StartDate:Finished, uid, MdmtItemNum, Grp_Stimulus.CRESP, 
                   Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, 
                   Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.OnsetTime, 
                   Grp_Confidence.OffsetTime, Grp_Decision.RESP, Grp_Decision.RT, 
                   Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
            rename(Stimulus.CRESP = Grp_Stimulus.CRESP) %>%
            mutate(version = "a1") %>%
            group_by(uid, MdmtItemNum) %>%
            mutate(Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
                   Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
                   Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

tmp2 <- as.data.frame(surveys_qrt$MDMT_team_B1 %>%
          select(ResponseID, StartDate:Finished, uid, MdmtItemNum, Grp_Stimulus.CRESP, 
                 Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, 
                 Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.OnsetTime, 
                 Grp_Confidence.OffsetTime, Grp_Decision.RESP, Grp_Decision.RT, 
                 Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
          rename(Stimulus.CRESP = Grp_Stimulus.CRESP) %>%
          mutate(version = "b1") %>%
          group_by(uid, MdmtItemNum) %>%
          mutate(Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
                 Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
                 Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

tmp3 <- surveys_java$MDMT_team_A2 %>%
          rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, MdmtItemNum = itemnum, 
                 Grp_Stimulus.OnsetTime = grp_stimulus_onset,
                 Grp_Stimulus.OffsetTime = grp_stimulus_offset, Grp_Stimulus.RT = grp_stimulus_rt,
                 Grp_Stimulus.RESP = grp_response, Grp_Confidence.OnsetTime = grp_confidence_onset,
                 Grp_Confidence.OffsetTime = grp_confidence_offset, Grp_Confidence.RT = grp_confidence_rt,
                 Grp_Confidence.RESP = grp_confidence, Grp_Decision.OnsetTime = grp_decision_onset,
                 Grp_Decision.OffsetTime = grp_decision_offset, Grp_Decision.RT = grp_decision_rt,
                 Grp_Decision.RESP = grp_decision) %>%
          select(ResponseID, StartDate, EndDate, Finished, uid, MdmtItemNum, Stimulus.CRESP, 
                 Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
                 Grp_Confidence.RESP, Grp_Confidence.RT, Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime,
                 Grp_Decision.RESP, Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
          mutate(version = "a2")
        
tmp4 <- surveys_java$MDMT_team_B2 %>%
        rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, MdmtItemNum = itemnum, 
               Grp_Stimulus.OnsetTime = grp_stimulus_onset,
               Grp_Stimulus.OffsetTime = grp_stimulus_offset, Grp_Stimulus.RT = grp_stimulus_rt,
               Grp_Stimulus.RESP = grp_response, Grp_Confidence.OnsetTime = grp_confidence_onset,
               Grp_Confidence.OffsetTime = grp_confidence_offset, Grp_Confidence.RT = grp_confidence_rt,
               Grp_Confidence.RESP = grp_confidence, Grp_Decision.OnsetTime = grp_decision_onset,
               Grp_Decision.OffsetTime = grp_decision_offset, Grp_Decision.RT = grp_decision_rt,
               Grp_Decision.RESP = grp_decision) %>%
        select(ResponseID, StartDate, EndDate, Finished, uid, MdmtItemNum, Stimulus.CRESP, 
               Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
               Grp_Confidence.RESP, Grp_Confidence.RT, Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime,
               Grp_Decision.RESP, Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
        mutate(version = "b2")

tmp5 <- surveys_multi$mdmt_grp_multi %>%
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, MdmtItemNum = itemnum, 
         Grp_Stimulus.OnsetTime = grp_stimulus_onset,
         Grp_Stimulus.OffsetTime = grp_stimulus_offset, Grp_Stimulus.RT = grp_stimulus_rt,
         Grp_Stimulus.RESP = grp_response, Grp_Confidence.OnsetTime = grp_confidence_onset,
         Grp_Confidence.OffsetTime = grp_confidence_offset, Grp_Confidence.RT = grp_confidence_rt,
         Grp_Confidence.RESP = grp_confidence, Grp_Decision.OnsetTime = grp_decision_onset,
         Grp_Decision.OffsetTime = grp_decision_offset, Grp_Decision.RT = grp_decision_rt,
         Grp_Decision.RESP = grp_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, MdmtItemNum, Stimulus.CRESP, 
         Grp_Stimulus.RESP, Grp_Stimulus.RT, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
         Grp_Confidence.RESP, Grp_Confidence.RT, Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime,
         Grp_Decision.RESP, Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "b_multi")

# uids that completed surveys with multiple JSON columns are included in this data
# the parsing function does not work so all values are NA need to remove and read in separately
uids <- c("18073109_1_d2", "18073109_1_g2", "18073110_2_d1", "18073110_2_g1", "18073114_1_d2",
          "18073114_1_g2", "18080114_1_e1", "18080114_1_e2")

tmp4 <- tmp4 %>% filter(!uid %in% uids)

# combine data into a single df 
mdmt_team <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5)

# Data Screening ----------------------------------------------------------
# identified duplicated records for most participants created when parsing
mdmt_team <- mdmt_team %>% filter(!is.na(MdmtItemNum))


# Check participants appear correct number of times =======================
# ISSUE 1: Identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_d1"   = all responses legit, P from 1pm session incorrectly assigned same uid
#   "18041210_2_g1"   = all responses legit, P from 1pm session incorrectly assigned same uid

# SOLUTION 1: change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_7O5Z8EYXfOdaKuF becomes 18041213_1_d2
# new uid for ResponseID = R_1B35tJ8AUKVbV1b becomes 18041213_1_g2
# new uid for ResponseID = R_9pojwpBJgp6FKqF becomes 18041213_2_d1
# new uid for ResponseID = R_cT0ougvPqsWbmTP becomes 18041213_2_g1

# Modify uids
mdmt_team <- mdmt_team %>% mutate(uid = ifelse(ResponseID == "R_7O5Z8EYXfOdaKuF", "18041213_1_d2", uid),
                                  uid = ifelse(ResponseID == "R_1B35tJ8AUKVbV1b", "18041213_1_g2", uid),
                                  uid = ifelse(ResponseID == "R_9pojwpBJgp6FKqF", "18041213_2_d1", uid),
                                  uid = ifelse(ResponseID == "R_cT0ougvPqsWbmTP", "18041213_2_g1", uid))

n_questions <- 16 # number of questions in this test

x <- table(mdmt_team$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}


# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- mdmt_team %>%
  select(uid, MdmtItemNum, Grp_Stimulus.RESP, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime,
         Grp_Confidence.RESP, Grp_Decision.RESP)

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
mdmt_team <- mdmt_team %>% mutate(Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP),
                                  Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
                                  Grp_Decision.RESP = tolower(Grp_Decision.RESP))

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

check_values(mdmt_team$Grp_Stimulus.RESP, 0, 3)
check_values(mdmt_team$Grp_Confidence.RESP, 25, 100)
check_values(mdmt_team$Grp_Decision.RESP, c("a", "b"))


# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
mdmt_team <- mdmt_team %>% 
  group_by(uid, MdmtItemNum) %>%
  mutate(Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()


# Check teams are giving same response and decision =======================
# Create grouping variables -----------------------------------------------
# group == date + group
# member == g == 1, d == 2
mdmt_team <- mdmt_team %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                                  ifelse(grepl("d[1-2]", uid), "2", 
                                                         ifelse(grepl("e1", uid), "1", "2"))),
                                  recruit = ifelse(grepl("[dg]", uid), "sona", "ext"),
                                  group = gsub("_[a-z][12]", "", uid))

# session notes indicate that uid == 18081710_1_g2 meant to enter 0 for Q2
mdmt_team <- mdmt_team %>% 
  mutate(Grp_Stimulus.RESP = ifelse(group == "18081710_1" & MdmtItemNum == "33", 0, Grp_Stimulus.RESP),
         Grp_Stimulus.ACC = ifelse(group == "18081710_1" & MdmtItemNum == "33", TRUE, Grp_Stimulus.ACC),
         Grp_Confidence.RESP = ifelse(group == "18081710_1" & MdmtItemNum == "33", 100, Grp_Confidence.RESP))

# Decisions
# in cases where member's group decisions differ change to missing
# Justification: If decision is different for members it is impossible to
# determine which decision was agreed upon by members. As decision was provided last
# other data for same item remains valid, if matching.

x <- mdmt_team %>% select(group, member, MdmtItemNum, Grp_Decision.RESP) %>% 
  spread(member, Grp_Decision.RESP) %>%
  group_by(group, MdmtItemNum) %>%
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group decisions differed between members
cat("number of times that group decision differed between members:", nrow(x))

for (i in seq(nrow(x))){
  group <- x$group[i]
  MdmtItemNum <- x$MdmtItemNum[i]
  mdmt_team[mdmt_team$group == group & mdmt_team$MdmtItemNum == MdmtItemNum, 
            c("Grp_Decision.RESP", "Grp_Decision.OnsetTime", 
              "Grp_Decision.OffsetTime", "Grp_Decision.RT")] <- NA
}

# Accuracy
# in cases where either is different change to missing for all variables 
# (Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP)
# Justification: If accuracy differs for members it is impossible to
# determine which response was agreed upon by members. Confidence and decision 
# were contingent upon accuracy, as it was provided first.

x <- mdmt_team %>% select(group, member, MdmtItemNum, Grp_Stimulus.RESP) %>% 
  spread(member, Grp_Stimulus.RESP) %>%
  group_by(group, MdmtItemNum) %>% 
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group accuracy differed between members
cat("number of times that group accuracy differed between members:", nrow(x))

for (i in seq(nrow(x))){
  group <- x$group[i]
  MdmtItemNum <- x$MdmtItemNum[i]
  mdmt_team[mdmt_team$group == group & mdmt_team$MdmtItemNum == MdmtItemNum, 
            c("Grp_Stimulus.RESP", "Grp_Stimulus.OnsetTime", "Grp_Stimulus.OffsetTime",
              "Grp_Stimulus.RT", "Grp_Confidence.RESP", "Grp_Confidence.OnsetTime", 
              "Grp_Confidence.OffsetTime", "Grp_Confidence.RT", "Grp_Decision.RESP", 
              "Grp_Decision.OnsetTime", "Grp_Decision.OffsetTime", "Grp_Decision.RT")] <- NA
}


# Check item RTs are acceptable --------------------------------------------
# first convert to numeric
mdmt_team <- mdmt_team %>%
group_by(group, MdmtItemNum) %>% 
  mutate(
    # convert RT variables to numeric
    Grp_Stimulus.OffsetTime = as.numeric(Grp_Stimulus.OffsetTime),
    Grp_Stimulus.OnsetTime = as.numeric(Grp_Stimulus.OnsetTime),
    Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
    Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
    Grp_Decision.OffsetTime = as.numeric(Grp_Decision.OffsetTime),
    Grp_Decision.OnsetTime = as.numeric(Grp_Decision.OnsetTime),
    # calculate team RTs: min(offset) - min(onset)
    Grp_Stimulus.RT = min(Grp_Stimulus.OffsetTime) - min(Grp_Stimulus.OnsetTime),
    Grp_Confidence.RT = min(Grp_Confidence.OffsetTime) - min(Grp_Confidence.OnsetTime),
    Grp_Decision.RT = min(Grp_Decision.OffsetTime) - min(Grp_Decision.OnsetTime))

# visualise RTs for each participant
# very slow RTs are not an issue
# RTs seems reasonable
# mdmt_team %>%
#   ggplot(aes(Grp_Stimulus.RT, colour = MdmtItemNum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(mdmt_team) +
#   geom_point(aes(Grp_Stimulus.RT, MdmtItemNum))
# 
# mdmt_team %>%
#   filter(Grp_Stimulus.RT < 3000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# mdmt_team %>%
#   filter(Grp_Stimulus.RT < 3000) %>%
#   ggplot() +
#   geom_point(aes(MdmtItemNum, Grp_Stimulus.RT)) +
#   facet_wrap(~ uid)

# Create CC column --------------------------------------------------------
# need to work out calculation first
# these are 4AFC questions so multiple responses possible
# when majority response is correct = CC
# when majority response is incorrect - CW?
# is Simon's paper on response cardinality relevant?
# im <- mdmt_team %>%
#   group_by(MdmtItemNum) %>%
#   summarise(item.mean = mean(Ind_Stimulus.ACC)) %>%
#   mutate(cc = ifelse(item.mean >= .5, "cc","cw"))
# 
# mdmt_team <- mdmt_team %>% left_join(im)

# Compute all variables ------------------------------------------------
mdmt_team.uid <- mdmt_team %>%
  group_by(uid) %>%
  summarise(
    # group = group[1],
    mdmt.grp.acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
    mdmt.grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
    mdmt.grp.bias = mdmt.grp.conf - mdmt.grp.acc,
    mdmt.grp.discrimination = mean(Grp_Confidence.RESP[Grp_Stimulus.ACC == 1], na.rm = TRUE) - mean(Grp_Confidence.RESP[Grp_Stimulus.ACC == 0], na.rm = TRUE),
    mdmt.grp.dec = mean(Grp_Decision.RESP == "a", na.rm = TRUE) * 100,
    mdmt.grp.rec = 100 * mean(Grp_Decision.RESP == "a" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
    mdmt.grp.comp = 100 * (sum(Grp_Decision.RESP == "a" & Grp_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Grp_Decision.RESP == "b" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / length(Grp_Stimulus.ACC),
    mdmt.grp.optim = 100 * mean(Grp_Decision.RESP == "a" & Grp_Stimulus.ACC == 1, na.rm = TRUE),
    mdmt.grp.hes = 100 * sum(Grp_Decision.RESP == "b" & Grp_Stimulus.ACC == 1, na.rm = TRUE)
    / sum(Grp_Stimulus.ACC == 1, na.rm = TRUE))


# calculate control thresholds 
# Logisitic regression: use confidence to predict the point at which a bet decision switches from no to yes
# Great tutorial for conductin logistic regression: https://www.datacamp.com/community/tutorials/logistic-regression-R
library(broom)

# prepare group data
data <- mdmt_team %>%
  ungroup() %>% 
  select(uid, MdmtItemNum, Grp_Confidence.RESP, Grp_Decision.RESP) %>%
  group_by(uid) %>% 
  filter(!all(is.na(Grp_Decision.RESP))) %>% 
  mutate(
    Grp_Decision.RESP = as.numeric(ifelse(Grp_Decision.RESP == "a", 1,
                                          ifelse(Grp_Decision.RESP == "b", 0, NA))),
    conf_range = abs(max(Grp_Confidence.RESP, na.rm = T) - min(Grp_Confidence.RESP, na.rm = T)),
    dec_range = abs(max(Grp_Decision.RESP, na.rm = T) - min(Grp_Decision.RESP, na.rm = T)))

# remove participants with no varaince in their confidence ratings or bet decisions
data <- data %>% filter(conf_range > 0 | dec_range > 0)

# fit logistic regression model and compute POST
post <- data %>% 
  nest() %>% 
  mutate(fit = map(data, ~ glm(Grp_Decision.RESP ~ Grp_Confidence.RESP, data = ., family = binomial)),
         coef = map(fit, ~ tidy(.)$estimate),
         coef = map(coef, ~ data.frame(a = .[[1]], b = .[2]))) %>% 
  unnest(coef) %>% 
  group_by(uid) %>% 
  summarise(mdmt.grp.post = (-a)/b) %>% 
  filter(mdmt.grp.post >= 0 & mdmt.grp.post <= 100)

# join group post data to test dataset
mdmt_team.uid <- mdmt_team.uid %>% left_join(post, by = "uid")


# Reliability ----------------------------------------------------
# remove teams that made non-genuine attempt
# mdmt_team <- mdmt_team %>% filter(!group %in% c("18041210_2", "18080915_1", "18072412_1", "19013010_1", "17102710_2", "18081613_1"))
#
# # #  Accuracy
# x <-  mdmt_team %>%
#     select(uid, MdmtItemNum, Grp_Stimulus.ACC) %>%
#     mutate(Grp_Stimulus.ACC = ifelse(Grp_Stimulus.ACC == TRUE, 1,
#                                      ifelse(Grp_Stimulus.ACC == FALSE, 0, Grp_Stimulus.ACC))) %>%
#     spread(MdmtItemNum, Grp_Stimulus.ACC) %>%
#     select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
#   # Confidence
#   x <- mdmt_team %>%
#     select(uid, MdmtItemNum, Grp_Confidence.RESP) %>%
#     spread(MdmtItemNum, Grp_Confidence.RESP) %>%
#     select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
#   # Decisiveness
#   x <- mdmt_team %>%
#     ungroup() %>% 
#     select(uid, MdmtItemNum, Grp_Decision.RESP)
# 
#   x[x == "a"] <- 1
#   x[x == "b"] <- 0
# 
#   x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
#   x <- x %>%
#     spread(MdmtItemNum, Grp_Decision.RESP) %>%
#     select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
#   x <- mdmt_team %>%
#     select(uid, MdmtItemNum, Grp_Stimulus.ACC, Grp_Decision.RESP) %>%
#     filter(Grp_Stimulus.ACC == 0) %>%
#     select(-Grp_Stimulus.ACC)
# 
#   x[x == "a"] <- 1
#   x[x == "b"] <- 0
# 
#   x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
#   odd1 <- c(2, 6, 10, 14, 18, 22, 26, 30, 34)
# 
#   odd <- filter(x, MdmtItemNum %in% odd1)
# 
#   even1 <- c(4, 8, 12, 16, 20, 24, 28, 32, 36)
# 
#   even <- filter(x, MdmtItemNum %in% even1)
# 
#   even <- even %>%
#     group_by(uid) %>%
#     summarise(even = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
#   odd <- odd %>%
#     group_by(uid) %>%
#     summarise(odd = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
#   x <- merge(odd, even) %>%
#     mutate(total = odd + even) %>%
#     ungroup(uid) %>%
#     select(-uid)
# 
#   cor.test(x$odd, x$even)
# 
#   # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
#   (2*0.3098693) / (1+0.3098693)


# #  For control paper ------------------------------------------------------
# # calculate group variables for control paper
# uid <- mdmt_team %>%
#   group_by(group, MdmtItemNum) %>%
#   mutate(Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP,
#          Grp_Stimulus.RT = min(Grp_Stimulus.OffsetTime) - min(Grp_Stimulus.OnsetTime),
#          Grp_Confidence.RESP = mean(Grp_Confidence.RESP, na.rm = TRUE),
#          Grp_Confidence.RT = min(Grp_Confidence.OffsetTime) - min(Grp_Confidence.OnsetTime),
#          Grp_Decision.RT = min(Grp_Decision.OffsetTime) - min(Grp_Decision.OnsetTime)) %>% ungroup()
# 
# 
# # remove RTs that are negative or too fast (e.g., less than 3 seconds)
# # only 3 RTs < 3 seconds. Inspected them and they look like normal responses. Retain.
# # uid %>% filter(Grp_Stimulus.RT <= 3000) %>% select(group, Grp_Stimulus.RT)
# 
# # modify or remove RTs that are unusual
# # look for very slow RTs (e.g., > 3 SDs)
# # library(lubridate)
# # 
# # uid %>% filter(group == "18081711_1" & MdmtItemNum == 20) %>% 
# #   select(uid, MdmtItemNum, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
# #          Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime) %>%
# #   group_by(uid) %>%
# #   mutate(rt = Grp_Stimulus.OffsetTime - Grp_Stimulus.OnsetTime) %>%
# #   ungroup() %>%
# #   mutate(
# #          Grp_Stimulus.OnsetTime = Grp_Stimulus.OnsetTime / 1000,
# #          Grp_Stimulus.OffsetTime = Grp_Stimulus.OffsetTime / 1000,
# #          Grp_Stimulus.OnsetTime = as.POSIXct(Grp_Stimulus.OnsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"),
# #          Grp_Stimulus.OffsetTime = as.POSIXct(Grp_Stimulus.OffsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"))
# 
# 
# # groups that have more than 4 very fast RTs
# # none to worry about in for this task
# # uid %>% filter(Grp_Stimulus.RT < 3000) %>%
# #   group_by(group) %>%
# #   mutate(n = n()) %>%
# #   select(MdmtItemNum, n, Grp_Stimulus.ACC, Grp_Stimulus.RT, Grp_Confidence.RESP)
# 
# d_mdmt_grp <- uid %>%
#   group_by(group, MdmtItemNum) %>%
#   summarise(Grp_Stimulus.ACC = Grp_Stimulus.ACC[1],
#             Grp_Stimulus.RESP = Grp_Stimulus.RESP[1],
#             Grp_Stimulus.RT = Grp_Stimulus.RT[1],
#             Grp_Confidence.RESP = Grp_Confidence.RESP[1],
#             #        Grp_Confidence.RT = Grp_Confidence.RT[1],
#             Grp_Decision.RESP = Grp_Decision.RESP[1]
#             #        Grp_Decision.RT = Grp_Decision.RT[1]
#   ) 
# # write_csv("data/control/mdmt_team.csv")
