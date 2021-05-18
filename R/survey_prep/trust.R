# trust scale A1 -------------------------------------------------------------
tmp1 <- surveys_qrt$Trust_Scale_A1 %>% 
  select(ResponseID, StartDate:Finished, uid, itemnum,
         key, Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "a1")

tmp2 <- surveys_qrt$Trust_Scale_B1 %>% 
  select(ResponseID, StartDate:Finished, uid, itemnum,
         key, Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "b1")

tmp3 <- surveys_java$trust_scale_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.RESP = response, 
         Stimulus.OnsetTime = stimulus_onset,
         Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, itemnum,
         key, Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "a2")

tmp4 <- surveys_java$trust_scale_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.RESP = response, 
         Stimulus.OnsetTime = stimulus_onset,
         Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, itemnum,
         key, Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "b2")

# combine data into a single df 
trust <- rbind(tmp1, tmp2, tmp3, tmp4)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE 1: 
# identified that the following uids appear twice
# 1.    "18041210_1_d2"   = all responses legit, P from 1pm session assigned incorrect uid
# 1.    "18041210_1_g2"   = all responses legit, P from 1pm session assigned incorrect uid
#
# SOLUTION 1: 
# 1. Legit 10am session Ps have ResponseID = R_6F0rJkl6b2rShbT and R_dhZ7j4ey2BQry7z
# 1pm session Ps have ResponseID = R_bkEea0hFHqoyrQN and R_3f8cWMJB6idhNwp
# change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_bkEea0hFHqoyrQN becomes 18041213_1_d2
# new uid for ResponseID = R_3f8cWMJB6idhNwp becomes 18041213_1_g2

# ISSUE 2:
#  identified that the following uids were recorded incorrectly
# 1.    "18041210_2_g1"   session started at 1pm so should be 18041213_2_g1
# 2.    "18041210_2_d1"   session started at 1pm so should be 18041213_2_d1
#
# SOLUTION 2:
# new uid for ResponseID == R_0krCYt3xuJuEREx is 18041213_2_g1
# new uid for ResponseID == R_8cOBOd4U9OmpJKR is 18041213_2_d1

trust <- trust %>% mutate(uid = ifelse(ResponseID == "R_bkEea0hFHqoyrQN", "18041213_1_d2", uid),
                          uid = ifelse(ResponseID == "R_3f8cWMJB6idhNwp", "18041213_1_g2", uid),
                          uid = ifelse(ResponseID == "R_8cOBOd4U9OmpJKR", "18041213_2_d1", uid),
                          uid = ifelse(ResponseID == "R_0krCYt3xuJuEREx", "18041213_2_g1", uid))

n_questions <- 5 # number of questions in this test

x <- table(trust$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
# first convert responses to numeric vector
trust <- trust %>%
  mutate(Stimulus.RESP = as.numeric(Stimulus.RESP))

x <- trust %>%
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

check_values(x$Stimulus.RESP, 1, 5)

# convert RT vars to numeric
trust <- trust %>%
  mutate(Stimulus.OffsetTime = as.numeric(Stimulus.OffsetTime),
         Stimulus.OnsetTime = as.numeric(Stimulus.OnsetTime),
         Stimulus.RT = Stimulus.OffsetTime - Stimulus.OnsetTime)

# session notes reveal that uid == "18080910_1_g2" misread the instructions 
# and entered the wrong responses for items 1 & 2. They requested that these 
# responses be changed to the inverse of what was entered (e.g., 7-x so 1 becomes 6)
trust <- trust %>% 
  mutate(Stimulus.RESP = ifelse(uid == "18080910_1_g2" & itemnum == "1", 7 - Stimulus.RESP, 
                                ifelse(uid == "18080910_1_g2" & itemnum == "2", 7 - Stimulus.RESP,
                                Stimulus.RESP)))

# visualise RTs for each participant
# very slow RTs are not an issue
# identified uid == "18042013_1_d2" answered 1 items in less than 500ms
# uid == "18110810_2_g1" answered 1 item in less than 100ms
# trust %>%
#   ggplot(aes(Stimulus.RT, colour = itemnum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(trust) +
#   geom_point(aes(x = Stimulus.RT, y = itemnum))
# 
# trust %>%
#   filter(Stimulus.RT < 1000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# trust %>%
#   filter(Stimulus.RT < 1000) %>%
#   ggplot() +
#   geom_point(aes(x = itemnum, y = Stimulus.RT)) +
#   facet_wrap(~ uid)

# calculate variables
trust.uid <- trust %>%
  mutate(Stimulus.RESP = ifelse(key == 1, Stimulus.RESP, 6 - Stimulus.RESP)) %>%  # Reverse score negative items
  group_by(uid) %>%
  summarise(
    trust = mean(Stimulus.RESP, na.rm = TRUE))

# Reliability ----------------------------------------------------
# x <- trust %>%
#   mutate(Stimulus.RESP = ifelse(key == 1, Stimulus.RESP,
#                                 6 - Stimulus.RESP))  %>% # Reverse score negative items
#   select(uid, itemnum, Stimulus.RESP) %>%
#   spread(itemnum, Stimulus.RESP) %>%
#   select(-uid)
# 
# psych::alpha(x)$total$raw_alpha
