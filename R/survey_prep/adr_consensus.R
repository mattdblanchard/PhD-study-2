
# Applying_Decision_Rules ---------------------------------------------------------------------
tmp1 <- as.data.frame(surveys_qrt$ADR_A1 %>%
            select(ResponseID, StartDate:Finished, uid, ADRitemNum, Ind_Stimulus.CRESP, 
                   Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
                   Ind_Confidence.RESP, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
                   Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
                   Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
                   Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, 
                   Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
                   Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
            rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
            mutate(version = "a1") %>%
            group_by(uid, ADRitemNum) %>%
            mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
                   Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
                   Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
                   Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
                   Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime,
                   Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

tmp2 <- as.data.frame(surveys_qrt$ADR_B1 %>%
            select(ResponseID, StartDate:Finished, uid, ADRitemNum, Ind_Stimulus.CRESP, 
                   Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
                   Ind_Confidence.RESP, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
                   Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
                   Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
                   Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, 
                   Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
                   Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
            rename(Stimulus.CRESP = Ind_Stimulus.CRESP) %>%
            mutate(version = "b1") %>%
            group_by(uid, ADRitemNum) %>%
            mutate(Ind_Confidence.OffsetTime = as.numeric(Ind_Confidence.OffsetTime),
                   Ind_Confidence.OnsetTime = as.numeric(Ind_Confidence.OnsetTime),
                   Grp_Confidence.OffsetTime = as.numeric(Grp_Confidence.OffsetTime),
                   Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
                   Ind_Confidence.RT = Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime,
                   Grp_Confidence.RT = Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime))

tmp3 <- surveys_java$ADR_A2 %>%
            rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ADRitemNum = itemnum, 
                   Ind_Stimulus.OnsetTime = ind_stimulus_onset,
                   Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
                   Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
                   Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
                   Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
                   Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
                   Ind_Decision.RESP = ind_decision, Wait.OnsetTime = wait_onset,
                   Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
                   Grp_Stimulus.OnsetTime = grp_stimulus_onset, Grp_Stimulus.OffsetTime = grp_stimulus_offset, 
                   Grp_Stimulus.RT = grp_stimulus_rt,Grp_Stimulus.RESP = grp_response, 
                   Grp_Confidence.OnsetTime = grp_confidence_onset, Grp_Confidence.OffsetTime = grp_confidence_offset, 
                   Grp_Confidence.RT = grp_confidence_rt, Grp_Confidence.RESP = grp_confidence, 
                   Grp_Decision.OnsetTime = grp_decision_onset, Grp_Decision.OffsetTime = grp_decision_offset,
                   Grp_Decision.RT = grp_decision_rt, Grp_Decision.RESP = grp_decision) %>%
            select(ResponseID, StartDate, EndDate, Finished, uid, ADRitemNum, Stimulus.CRESP, 
                   Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
                   Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
                   Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
                   Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
                   Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.RT,
                   Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
                   Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
            mutate(version = "a2")


tmp4 <- surveys_java$ADR_B2 %>%
          rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ADRitemNum = itemnum, 
                 Ind_Stimulus.OnsetTime = ind_stimulus_onset,
                 Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
                 Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
                 Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
                 Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
                 Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
                 Ind_Decision.RESP = ind_decision, Wait.OnsetTime = wait_onset,
                 Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
                 Grp_Stimulus.OnsetTime = grp_stimulus_onset, Grp_Stimulus.OffsetTime = grp_stimulus_offset, 
                 Grp_Stimulus.RT = grp_stimulus_rt,Grp_Stimulus.RESP = grp_response, 
                 Grp_Confidence.OnsetTime = grp_confidence_onset, Grp_Confidence.OffsetTime = grp_confidence_offset, 
                 Grp_Confidence.RT = grp_confidence_rt, Grp_Confidence.RESP = grp_confidence, 
                 Grp_Decision.OnsetTime = grp_decision_onset, Grp_Decision.OffsetTime = grp_decision_offset,
                 Grp_Decision.RT = grp_decision_rt, Grp_Decision.RESP = grp_decision) %>%
          select(ResponseID, StartDate, EndDate, Finished, uid, ADRitemNum, Stimulus.CRESP, 
                 Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
                 Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
                 Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
                 Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
                 Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.RT,
                 Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
                 Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
          mutate(version = "b2")


tmp5 <- surveys_multi$adr_multi[, c(-38, -39, -40)] %>% # remove duplicated columns for itemnum, question and cresp
  rename(ResponseID = ResponseId, Stimulus.CRESP = cresp, ADRitemNum = itemnum, 
         Ind_Stimulus.OnsetTime = ind_stimulus_onset,
         Ind_Stimulus.OffsetTime = ind_stimulus_offset, Ind_Stimulus.RT = ind_stimulus_rt,
         Ind_Stimulus.RESP = ind_response, Ind_Confidence.OnsetTime = ind_confidence_onset,
         Ind_Confidence.OffsetTime = ind_confidence_offset, Ind_Confidence.RT = ind_confidence_rt,
         Ind_Confidence.RESP = ind_confidence, Ind_Decision.OnsetTime = ind_decision_onset,
         Ind_Decision.OffsetTime = ind_decision_offset, Ind_Decision.RT = ind_decision_rt,
         Ind_Decision.RESP = ind_decision, Wait.OnsetTime = wait_onset,
         Wait.OffsetTime = wait_offset, Wait.RT = wait_rt,
         Grp_Stimulus.OnsetTime = grp_stimulus_onset, Grp_Stimulus.OffsetTime = grp_stimulus_offset, 
         Grp_Stimulus.RT = grp_stimulus_rt, Grp_Stimulus.RESP = grp_response, 
         Grp_Confidence.OnsetTime = grp_confidence_onset, Grp_Confidence.OffsetTime = grp_confidence_offset, 
         Grp_Confidence.RT = grp_confidence_rt, Grp_Confidence.RESP = grp_confidence, 
         Grp_Decision.OnsetTime = grp_decision_onset, Grp_Decision.OffsetTime = grp_decision_offset,
         Grp_Decision.RT = grp_decision_rt, Grp_Decision.RESP = grp_decision) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, ADRitemNum, Stimulus.CRESP, 
         Ind_Stimulus.RESP, Ind_Stimulus.RT, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime,
         Ind_Confidence.RESP, Ind_Confidence.RT, Ind_Confidence.OnsetTime, Ind_Confidence.OffsetTime,
         Ind_Decision.RESP, Ind_Decision.RT, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
         Wait.OnsetTime, Wait.OffsetTime, Wait.RT, Grp_Stimulus.RESP, Grp_Stimulus.RT, 
         Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Confidence.RESP, Grp_Confidence.RT,
         Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.RESP,
         Grp_Decision.RT, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime) %>%
  mutate(version = "b_multi")

# uids that completed surveys with multiple JSON columns are included in this data
# the parsing function does not work so all values are NA need to remove and read in separately
uids <- c("18073109_1_d2", "18073109_1_g2", "18073110_2_d1", "18073110_2_g1", "18073114_1_d2",
          "18073114_1_g2", "18080114_1_e1", "18080114_1_e2")

tmp4 <- tmp4 %>% filter(!uid %in% uids)

# combine data into a single df 
adr <- rbind(tmp1, tmp2, tmp3, tmp4, tmp5)

# adr %>% filter(str_detect(uid, "18041210_2")) %>% select(uid, StartDate, ResponseID, Ind_Stimulus.RESP)
# Data Screening ----------------------------------------------------------
# Check participants appear correct number of times =======================
# ISSUE 1: 
# 1. identified that the following uids appear twice
#   "18041210_1_d2"   = all responses legit, P from 2pm session incorrectly assigned same uid
#   "18041210_1_g2"   = all responses legit, P from 2pm session incorrectly assigned same uid
#   "18041210_2_d1"   = duplicate contains only NAs. ResponseIDs == R_dcjLLy62SrBmSJD
#   "18041210_2_g1"   = duplicate contains only NAs. ResponseID == R_7VWnEvF8GQL0nm5
#
# SOLUTION 1: 
# 1. Legit 10am Ps have ResponseIDs = R_eWC7cASZvnfdpT7 and R_7Pvv5DcpK46NEDX
# incorrect ResponseIDs = R_6rHBoBnN9DirpWd and R_elgWv0t4ubB3y7j started at 1pm same day
# change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_6rHBoBnN9DirpWd becomes 18041213_1_d2
# new uid for ResponseID = R_elgWv0t4ubB3y7j becomes 18041213_1_g2
#
# ISSUE 2:
#  identified that the following uids were recorded incorrectly
# 1.    "18041210_2_g1"   session started at 1pm so should be 18041213_2_g1
# 2.    "18041210_2_d1"   session started at 1pm so should be 18041213_2_d1

# SOLUTION 2:
# new uid for ResponseID == R_aVq7lqcw7hdF5uB is 18041213_2_g1
# new uid for ResponseID == R_6tErKNN9fsqeBJX is 18041213_2_d1

adr <- adr %>% mutate(uid = ifelse(ResponseID == "R_6rHBoBnN9DirpWd", "18041213_1_d2", uid),
                      uid = ifelse(ResponseID == "R_elgWv0t4ubB3y7j", "18041213_1_g2", uid),
                      uid = ifelse(ResponseID == "R_aVq7lqcw7hdF5uB", "18041213_2_g1", uid),
                      uid = ifelse(ResponseID == "R_6tErKNN9fsqeBJX", "18041213_2_d1", uid)) %>%
  filter(ResponseID != "R_7VWnEvF8GQL0nm5" & ResponseID != "R_dcjLLy62SrBmSJD")

n_questions <- 10 # number of questions in this test

x <- table(adr$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# identified 2 uids with all NA. Remove
adr <- adr %>% 
  mutate(Finished = ifelse(Finished == 1, TRUE,
                           ifelse(Finished == 0, FALSE,
                                  ifelse(Finished == "True", TRUE,
                                         ifelse(Finished == "False", FALSE, Finished))))) %>% 
  filter(Finished == TRUE)

# create vector to select only uid and participants responses
# first convert confidence vars to numeric
adr <- adr %>%
  mutate(Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
         Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP))

x <- adr %>%
  select(uid, Ind_Stimulus.RESP, Grp_Stimulus.RESP, Ind_Confidence.RESP, 
         Grp_Confidence.RESP, Ind_Decision.RESP, Grp_Decision.RESP)

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

if (any(row_miss)) {
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

# Check whether vector contains acceptable values ("a-e" or "none") ------------------------
# first convert all character responses to lower case and arrange strings in 
# alphabetical order

# One participant entered "nona" which is clearly supposed to be "none"
# a number of participants entered invalid responses
# For Ind_Stimulus.RESP: "aq", "av", "f", "r", "w"
# For Grp_Stimulus.RESP: "s", "w"
# Not sure how to handle. Check with SK

# function to sort character strings
string_sort <- function(x) paste0(sort(unlist(strsplit(x, "")), decreasing = F), collapse = "")

adr <- adr %>% mutate(Ind_Stimulus.RESP = tolower(Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = tolower(Grp_Stimulus.RESP),
                      Ind_Decision.RESP = tolower(Ind_Decision.RESP),
                      Grp_Decision.RESP = tolower(Grp_Decision.RESP))

adr <- adr %>% mutate(Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == "nona", "none", Ind_Stimulus.RESP),
                      Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == "w", "e", Ind_Stimulus.RESP),
                      Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == "r", "e", Ind_Stimulus.RESP),
                      Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP == "av", "ac", Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = ifelse(Grp_Stimulus.RESP == "w", "a", Grp_Stimulus.RESP),
                      Grp_Stimulus.RESP = ifelse(Grp_Stimulus.RESP == "s", "a", Grp_Stimulus.RESP))

# the 2nd row ended with Ind_Stimulus.RESP just changed to Grp_Stimulus.RESP (need to check that this is correct)
adr <- adr %>% mutate(Ind_Stimulus.RESP = ifelse(Ind_Stimulus.RESP != "none", sapply(adr$Ind_Stimulus.RESP, string_sort), Ind_Stimulus.RESP),
                      Grp_Stimulus.RESP = ifelse(Grp_Stimulus.RESP != "none", sapply(adr$Grp_Stimulus.RESP, string_sort), Grp_Stimulus.RESP))

# add Stimulus.ACC column (Stimulus.RESP == Stimulus.CRESP)
adr <- adr %>% 
  group_by(uid, ADRitemNum) %>%
  mutate(Ind_Stimulus.ACC = Ind_Stimulus.RESP == Stimulus.CRESP,
         Grp_Stimulus.ACC = Grp_Stimulus.RESP == Stimulus.CRESP) %>%
  ungroup()

# # Check teams are giving same response and decision =======================
# Create grouping variables 
# group == date + group
# member == g == 1, d == 2
adr <- adr %>% mutate(member = ifelse(grepl("g[1-2]", uid), "1", 
                                      ifelse(grepl("d[1-2]", uid), "2", 
                                             ifelse(grepl("e1", uid), "1", "2"))),
                      recruit = ifelse(grepl("[dg]", uid), "sona", "ext"),
                      group = gsub("_[a-z][12]", "", uid))

# Session notes indicate that group members discussed the individual response for item 10. 
# Make all individual responses for this item NA
# this code is causing problems for the consensus variable so will leave in
# adr <- adr %>% mutate(Ind_Stimulus.RESP = ifelse(group == "18110713_2" & ADRitemNum == "10", NA, Ind_Stimulus.RESP),
#                       Ind_Confidence.RESP = ifelse(group == "18110713_2" & ADRitemNum == "10", NA, Ind_Confidence.RESP),
#                       Ind_Decision.RESP = ifelse(group == "18110713_2" & ADRitemNum == "10", NA, Ind_Decision.RESP))

# Decisions
# in cases where member's group decisions differ change to missing
# Justification: If decision is different for members it is impossible to
# determine which decision was agreed upon by members. As decision was provided last
# other data for same item remains valid, if matching.
x <- adr %>% select(group, member, ADRitemNum, Grp_Decision.RESP) %>%
  spread(member, Grp_Decision.RESP) %>%
  group_by(group, ADRitemNum) %>%
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)


# prints out number of times that group decisions differed between members
cat("number of times that group decision differed between members:", nrow(x))

for (i in seq(nrow(x))){
  group <- x$group[i]
  ADRitemNum <- x$ADRitemNum[i]
  adr[adr$group == group & adr$ADRitemNum == ADRitemNum, 
      c("Grp_Decision.RESP", "Grp_Decision.OnsetTime", "Grp_Decision.OffsetTime", 
        "Grp_Decision.RT")] <- NA
}


# # Response
# # in cases where either is different change to missing for all variables
# # (Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP)
# # Justification: If accuracy differs for members it is impossible to
# # determine which response was agreed upon by members. Confidence and decision
# # were contingent upon accuracy, as it was provided first.

x <- adr %>% select(group, member, ADRitemNum, Grp_Stimulus.RESP) %>%
  spread(member, Grp_Stimulus.RESP) %>%
  group_by(group, ADRitemNum) %>%
  mutate(dif = `1` != `2`) %>%
  filter(dif == TRUE)

# prints out number of times that group accuracy differed between members
cat("number of times that group accuracy differed between members:", nrow(x))


for (i in seq(nrow(x))){
  group <- x$group[i]
  ADRitemNum <- x$ADRitemNum[i]
  adr[adr$group == group & adr$ADRitemNum == ADRitemNum, 
      c("Grp_Stimulus.RESP", "Grp_Stimulus.ACC", "Grp_Confidence.RESP", "Grp_Decision.RESP")] <- NA
}


# visualise RTs for each participant
# first, convert RT vars to numeric
adr <- adr %>% 
  group_by(uid, ADRitemNum) %>% 
  mutate(Ind_Stimulus.OnsetTime = as.numeric(Ind_Stimulus.OnsetTime),
         Ind_Stimulus.OffsetTime = as.numeric(Ind_Stimulus.OffsetTime),
         Ind_Stimulus.RT = Ind_Stimulus.OffsetTime - Ind_Stimulus.OnsetTime,
         Grp_Stimulus.OnsetTime = as.numeric(Grp_Stimulus.OnsetTime),
         Grp_Stimulus.OffsetTime = as.numeric(Grp_Stimulus.OffsetTime),
         Wait.OnsetTime = as.numeric(Wait.OnsetTime)) %>% 
  group_by(group, ADRitemNum) %>% 
  mutate(Grp_Stimulus.RT = min(Grp_Stimulus.OffsetTime) - max(Wait.OnsetTime))


# identify teams with negative RTs for group responses
# this indicates that one participant may have entered a group response 
# before their teammate had entered an individual response
# adr %>% 
#   filter(Grp_Stimulus.RT < 0) %>% 
#   group_by(group) %>% 
#   summarise(n = n()/2) %>% 
#   arrange(desc(n))
# 
# # plot timing for responses for potentially problematic teams
# adr %>% 
#   filter(group == "18080710_1") %>% 
#   ungroup() %>% 
#   select(uid, ADRitemNum, 
#          # Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime, Ind_Confidence.OnsetTime, 
#          # Ind_Confidence.OffsetTime, Ind_Decision.OnsetTime, Ind_Decision.OffsetTime,
#          Wait.OnsetTime, Wait.OffsetTime, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime
#          # Grp_Confidence.OnsetTime, Grp_Confidence.OffsetTime, Grp_Decision.OnsetTime, Grp_Decision.OffsetTime
#          ) %>% 
#   gather(var, val, Wait.OnsetTime:Grp_Stimulus.OffsetTime) %>% 
#   mutate(val = as.numeric(val),
#          val = val/1000,
#          val = as.POSIXct(val, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11")) %>% 
#   ggplot(aes(x = val, y = uid)) +
#   geom_point() +
#   facet_wrap(ADRitemNum ~ ., scales = "free_x")


# very slow RTs are not an issue
# RTs look reasonable
# adr %>%
#   ggplot(aes(Ind_Stimulus.RT, colour = ADRitemNum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(adr) +
#   geom_point(aes(x = Ind_Stimulus.RT, y = ADRitemNum))
# 
# adr %>%
#   filter(Ind_Stimulus.RT < 5000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# adr %>%
#   filter(Ind_Stimulus.RT < 5000) %>%
#   ggplot() +
#   geom_point(aes(x = ADRitemNum, y = Ind_Stimulus.RT)) +
#   facet_wrap(~ uid)
# 
# adr %>%
#   ggplot(aes(Grp_Stimulus.RT, colour = ADRitemNum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(adr) +
#   geom_point(aes(x = Grp_Stimulus.RT, y = ADRitemNum))
# 
# adr %>%
#   filter(Grp_Stimulus.RT < 2000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# adr %>%
#   filter(Grp_Stimulus.RT < 2000) %>%
#   ggplot() +
#   geom_point(aes(x = ADRitemNum, y = Grp_Stimulus.RT)) +
#   facet_wrap(~ uid)

# integrity check: calculate the proportion of times each team entered the 
# same response individually. If very high (>=.90) then need to investigate further 
# may need to check audio to see if they were discussing individual responses
# the following code is to investigate pattern of responses and timing of each item attempted
# check <- adr %>%
#   filter(group == "18081518_1") %>%
#   select(uid, ADRitemNum, Ind_Stimulus.OnsetTime, Ind_Stimulus.OffsetTime, Ind_Stimulus.RESP, Stimulus.CRESP) %>%
#   mutate(Ind_Stimulus.OnsetTime = Ind_Stimulus.OnsetTime / 1000,
#          Ind_Stimulus.OffsetTime = Ind_Stimulus.OffsetTime / 1000,
#          Ind_Stimulus.OnsetTime = as.POSIXct(Ind_Stimulus.OnsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"),
#          Ind_Stimulus.OffsetTime = as.POSIXct(Ind_Stimulus.OffsetTime, origin = "1970-01-01 00:00.00 UTC", tz = "Etc/GMT-11"))

# identified two potentially problematic group
# group == 18080810_1 - entered the same response for all items and both got the last Q wrong
# group == 18081518_1 - entered the same response for 9 items and response timings were very similar for
# all items except the one that responses did not match
adr %>% 
  group_by(group, ADRitemNum) %>% 
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
# these are not 2AFC questions so multiple possible responses
# when majority response is correct = CC
# when majority response is incorrect - CW?
# im <- adr %>%
#   group_by(ADRitemNum) %>%
#   summarise(item.mean = mean(Ind_Stimulus.ACC)) %>%
#   mutate(cc = ifelse(item.mean >= .5, "cc","cw"))
# 
# adr <- adr %>% left_join(im)

# Create consensus (agree vs disagree) column -----------------------------
adr <- adr %>% 
  filter(group != "17102710_2") %>% # only one group member completed this test
  group_by(group, ADRitemNum) %>% 
  mutate(consensus = ifelse(Ind_Stimulus.RESP[1] == Ind_Stimulus.RESP[2], 
                            "agree", "disagree"))

check <- adr %>% filter(is.na(consensus))

# save cleaned item-level data
adr %>% write_csv("data/item_level/adr_item_level.csv")

# Compute all variables ------------------------------------------------
adr.uid <- adr %>%
  group_by(uid, consensus) %>%
  summarise(
    # group = group[1],
    adr.ind.acc = mean(Ind_Stimulus.ACC) * 100,
    adr.grp.acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
    adr.ind.conf = mean(Ind_Confidence.RESP),
    adr.grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
    adr.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
    adr.grp.dec = mean(Grp_Decision.RESP == "y", na.rm = TRUE) * 100,
    adr.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
    adr.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
    / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
    adr.ind.comp = 100 * (sum(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 0, na.rm = TRUE)) / length(Ind_Stimulus.ACC),
    adr.grp.comp = 100 * (sum(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE) +
                            sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 0, na.rm = TRUE)) / length(Grp_Stimulus.ACC),
    adr.ind.optim = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 1, na.rm = TRUE),
    adr.grp.optim = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 1, na.rm = TRUE),
    adr.ind.hes = 100 * sum(Ind_Decision.RESP == "n" & Ind_Stimulus.ACC == 1, na.rm = TRUE) / sum(Ind_Stimulus.ACC == 1, na.rm = TRUE),
    adr.grp.hes = 100 * sum(Grp_Decision.RESP == "n" & Grp_Stimulus.ACC == 1, na.rm = TRUE) / sum(Grp_Stimulus.ACC == 1, na.rm = TRUE))


# calculate control thresholds 
# Logisitic regression: use confidence to predict the point at which a bet decision switches from no to yes
# Great tutorial for conductin logistic regression: https://www.datacamp.com/community/tutorials/logistic-regression-R
library(broom)

# prepare individual data
data <- adr %>%
  ungroup() %>% 
  select(uid, consensus, ADRitemNum, Ind_Confidence.RESP, Ind_Decision.RESP) %>%
  group_by(uid, consensus) %>% 
  filter(!all(is.na(Ind_Decision.RESP))) %>% 
  mutate(
    Ind_Decision.RESP = as.numeric(ifelse(Ind_Decision.RESP == "y", 1,
                                          ifelse(Ind_Decision.RESP == "n", 0, NA))),
    conf_range = abs(max(Ind_Confidence.RESP, na.rm = T) - min(Ind_Confidence.RESP, na.rm = T)),
    dec_range = abs(max(Ind_Decision.RESP, na.rm = T) - min(Ind_Decision.RESP, na.rm = T)))

# remove participants with no variance in their confidence ratings or bet decisions
data <- data %>% filter(conf_range > 0 | dec_range > 0)

# fit logistic regression model and compute POST
post <- data %>% 
  nest() %>% 
  mutate(fit = map(data, ~ glm(Ind_Decision.RESP ~ Ind_Confidence.RESP, data = ., family = binomial)),
         coef = map(fit, ~ tidy(.)$estimate),
         coef = map(coef, ~ data.frame(a = .[[1]], b = .[2]))) %>% 
  unnest(coef) %>% 
  group_by(uid, consensus) %>% 
  summarise(adr.ind.post = -a/b) %>% 
  filter(adr.ind.post >= 0 & adr.ind.post <= 100)

# join individual post data to test dataset
adr.uid <- adr.uid %>% left_join(post, by = c("uid", "consensus"))

# prepare group data
data <- adr %>%
  ungroup() %>% 
  select(uid, consensus, ADRitemNum, Grp_Confidence.RESP, Grp_Decision.RESP) %>%
  group_by(uid, consensus) %>% 
  filter(!all(is.na(Grp_Decision.RESP))) %>% 
  mutate(
    Grp_Decision.RESP = as.numeric(ifelse(Grp_Decision.RESP == "y", 1,
                                          ifelse(Grp_Decision.RESP == "n", 0, NA))),
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
  group_by(uid, consensus) %>% 
  summarise(adr.grp.post = -a/b) %>% 
  filter(adr.grp.post >= 0 & adr.grp.post <= 100)

# join group post data to test dataset
adr.uid <- adr.uid %>% left_join(post, by = c("uid", "consensus"))

# create wide data
adr.uid <- adr.uid %>% 
  gather(var, val, -uid, -consensus) %>% 
  unite(var, c("var", "consensus"), sep = "_") %>% 
  spread(var, val)


# Reliability ----------------------------------------------------
# remove teams that made non-genuine attempt
# adr <- adr %>% filter(!group %in% c("18041210_2", "18080915_1", "18072412_1", "19013010_1", "17102710_2", "18081613_1"))
# # FOR INDIVIDUALS
# # Accuracy
# x <- adr %>%
#   select(uid, ADRitemNum, Ind_Stimulus.ACC) %>%
#   mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1,
#                                    ifelse(Ind_Stimulus.ACC == FALSE, 0, Ind_Stimulus.ACC))) %>%
#   spread(ADRitemNum, Ind_Stimulus.ACC) %>%
#   select(-uid)
# #
# psych::alpha(x)$total$raw_alpha
# 
# # Confidence
# x <- adr %>%
#   select(uid, ADRitemNum, Ind_Confidence.RESP) %>%
#   spread(ADRitemNum, Ind_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Decisiveness
# x <- adr %>%
#   select(uid, ADRitemNum, Ind_Decision.RESP)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Ind_Decision.RESP = as.numeric(Ind_Decision.RESP))
# 
# x <- x %>%
#   spread(ADRitemNum, Ind_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# # Using split-half reliability adjusted using the Spearman-Brown prophecy formula
# x <- adr %>%
#   select(uid, ADRitemNum, Ind_Stimulus.ACC, Ind_Decision.RESP) %>%
#   filter(Ind_Stimulus.ACC == 0) %>%
#   select(-Ind_Stimulus.ACC) %>%
#   mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1, 0))
# 
# odd1 <- c(1, 3, 5, 7, 9)
# 
# odd <- filter(x, ADRitemNum %in% odd1) %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# even1 <- c(2, 4, 6, 8, 10)
# 
# even <- filter(x, ADRitemNum %in% even1) %>%
#   group_by(uid) %>%
#   summarise(even = sum(Ind_Decision.RESP, na.rm = TRUE))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)
# 
# # Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
# (2*0.537521) / (1+0.537521)
# 
# 
# # FOR TEAMS
# # Accuracy
# 
# x <- adr %>%
#   select(uid, ADRitemNum, Grp_Stimulus.ACC) %>%
#   mutate(Grp_Stimulus.ACC = ifelse(Grp_Stimulus.ACC == TRUE, 1, 0)) %>%
#   spread(ADRitemNum, Grp_Stimulus.ACC) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Confidence
# x <- adr %>%
#   select(uid, ADRitemNum, Grp_Confidence.RESP) %>%
#   spread(ADRitemNum, Grp_Confidence.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Decisiveness
# x <- adr %>%
#   select(uid, ADRitemNum, Grp_Decision.RESP)
# 
# x[x == "y"] <- 1
# x[x == "n"] <- 0
# 
# x <- transform(x, Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))
# 
# x <- x %>%
#   spread(ADRitemNum, Grp_Decision.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
# 
# # Recklessness
# x <- adr %>%
#   select(uid, ADRitemNum, Grp_Stimulus.ACC, Grp_Decision.RESP) %>%
#   filter(Grp_Stimulus.ACC == 0) %>%
#   select(-Grp_Stimulus.ACC) %>%
#   mutate(Grp_Decision.RESP = ifelse(Grp_Decision.RESP == "y", 1, 
#                                     ifelse(Grp_Decision.RESP == "n", 0, NA)))
# 
# odd1 <- c(1, 3, 5, 7, 9)
# 
# odd <- filter(x, ADRitemNum %in% odd1) %>%
#   group_by(uid) %>%
#   summarise(odd = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
# even1 <- c(2, 4, 6, 8, 10)
# 
# even <- filter(x, ADRitemNum %in% even1) %>%
#   group_by(uid) %>%
#   summarise(even = sum(Grp_Decision.RESP, na.rm = TRUE))
# 
# x <- merge(odd, even) %>%
#   mutate(total = odd + even) %>%
#   ungroup(uid) %>%
#   select(-uid)
# 
# cor.test(x$odd, x$even)

# Adjust r with the Spearman-Brown prophecy formula == (2*r) / (1+r)
(2*0.4001525) / (1+0.4001525)
# 
# use this website to correct using the spearman-brown prophecy formula
# https://www.cedu.niu.edu/~walker/calculators/sbpf.asp
