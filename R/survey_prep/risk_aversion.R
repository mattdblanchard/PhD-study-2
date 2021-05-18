# risk scale A1 -------------------------------------------------------------
tmp1 <- surveys_qrt$risk_aversion_A1 %>% 
  select(ResponseID, StartDate:Finished, uid, itemnum,
         Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "a1")

tmp2 <- surveys_qrt$risk_aversion_B1 %>% 
  select(ResponseID, StartDate:Finished, uid, itemnum,
         Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "b1")

tmp3 <- surveys_java$risk_aversion_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.RESP = response, 
         Stimulus.OnsetTime = stimulus_onset,
         Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, itemnum,
         Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "a2")

tmp4 <- surveys_java$risk_aversion_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.RESP = response, 
         Stimulus.OnsetTime = stimulus_onset,
         Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, itemnum,
         Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "b2")

# combine data into a single df 
risk <- rbind(tmp1, tmp2, tmp3, tmp4)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE: 
# identified that the following uids appear twice
# 1.    "18041210_1_d2"   = all responses legit, P from 1pm session assigned incorrect uid
# 1.    "18041210_1_g2"   = all responses legit, P from 1pm session assigned incorrect uid
#
# SOLUTION: 
# 1. Legit 10am session Ps have ResponseID = R_ebugSjOiyVfBCIZ and R_427lQOkBiVfwuKV
# 1pm session Ps have ResponseID = R_74fwqoQQIudkKix and R_9LYbNqYCloizaG9
# change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_74fwqoQQIudkKix becomes 18041213_1_d2
# new uid for ResponseID = R_9LYbNqYCloizaG9 becomes 18041213_1_g2

# ISSUE 2:
#  identified that the following uids were recorded incorrectly
# 1.    "18041210_2_g1"   session started at 1pm so should be 18041213_2_g1
# 2.    "18041210_2_d1"   session started at 1pm so should be 18041213_2_d1
#
# SOLUTION 2:
# new uid for ResponseID == R_9vKj6lnL2fMLUl7 is 18041213_2_g1
# new uid for ResponseID == R_djs5AdgtBHo4LWt is 18041213_2_d1

risk <- risk %>% mutate(uid = ifelse(ResponseID == "R_74fwqoQQIudkKix", "18041213_1_d2", uid),
                        uid = ifelse(ResponseID == "R_9LYbNqYCloizaG9", "18041213_1_g2", uid),
                        uid = ifelse(ResponseID == "R_djs5AdgtBHo4LWt", "18041213_2_d1", uid),
                        uid = ifelse(ResponseID == "R_9vKj6lnL2fMLUl7", "18041213_2_g1", uid))

n_questions <- 10 # number of questions in this test

x <- table(risk$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- risk %>%
  select(uid, Stimulus.RESP)

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

# Check whether vector contains acceptable values (range == 1 - 7) ------------------------
# first convert responses to lower case
risk <- risk %>%
  mutate(Stimulus.RESP = tolower(Stimulus.RESP))

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

check_values(x$Stimulus.RESP, "a", "b")

# convert RT vars to numeric
risk <- risk %>%
  group_by(uid, itemnum) %>% 
  mutate(
    # convert offset and onset vars to numeric
    Stimulus.OffsetTime = as.numeric(Stimulus.OffsetTime),
    Stimulus.OnsetTime = as.numeric(Stimulus.OnsetTime),
    # calculate RT for each item
    Stimulus.RT = Stimulus.OffsetTime - Stimulus.OnsetTime)


# visualise RTs for each participant
# very slow RTs are not an issue
# RTs seem reasonable
# risk %>%
#   ggplot(aes(Stimulus.RT, colour = itemnum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(risk) +
#   geom_point(aes(x = Stimulus.RT, y = itemnum))
# 
# risk %>%
#   filter(Stimulus.RT < 500) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# risk %>%
#   filter(Stimulus.RT < 500) %>%
#   ggplot() +
#   geom_point(aes(x = itemnum, y = Stimulus.RT)) +
#   facet_wrap(~ uid)



# calculate variable -----------------------------------------------------
# Not sure if this scoring is correct
# read paper titled Risk Aversion and Incentive Effects for correct scoring
risk.uid <- risk %>%
  group_by(uid) %>% 
  summarise(risk.aversion = sum(Stimulus.RESP == "a"))



# Reliability ----------------------------------------------------
# x <- risk %>%
#   mutate(Stimulus.RESP = ifelse(Stimulus.RESP == "a", 0,
#                                 ifelse(Stimulus.RESP == "b", 1, Stimulus.RESP)),
#          Stimulus.RESP = as.numeric(Stimulus.RESP)) %>%
#   select(uid, itemnum, Stimulus.RESP) %>%
#   spread(itemnum, Stimulus.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
