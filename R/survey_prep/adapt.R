# Adapt scale A1 -------------------------------------------------------------
tmp1 <- surveys_qrt$adaptability_A1 %>% 
  select(ResponseID, StartDate:Finished, uid, itemnum,
         factor, Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "a1")

tmp2 <- surveys_qrt$adaptability_B1 %>% 
  select(ResponseID, StartDate:Finished, uid, itemnum,
         factor, Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "b1")

tmp3 <- surveys_java$adaptability_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.RESP = response, 
         Stimulus.OnsetTime = stimulus_onset,
         Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, itemnum,
         factor, Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "a2")

tmp4 <- surveys_java$adaptability_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.RESP = response, 
         Stimulus.OnsetTime = stimulus_onset,
         Stimulus.OffsetTime = stimulus_offset, Stimulus.RT = stimulus_rt) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid, itemnum,
         factor, Stimulus.RESP, Stimulus.OnsetTime, 
         Stimulus.OffsetTime, Stimulus.RT) %>%
  mutate(version = "b2")

# combine data into a single df 
adapt <- rbind(tmp1, tmp2, tmp3, tmp4)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE 1: 
# identified that the following uids appear twice
# 1.    "18041210_1_d2"   = all responses legit, P from 1pm session assigned incorrect uid
# 1.    "18041210_1_g2"   = all responses legit, P from 1pm session assigned incorrect uid
# 2.    "18042013_1_g2"   = duplicate contains only NAs. ResponseID == R_3JiH3ZnPGYh1jEx
#
# SOLUTION 1: 
# 1. Legit 10am session Ps have ResponseID = R_3L5P9mV1i0KZ2DP and R_bBj3nNp3amrpb9z
# 1pm session Ps have ResponseID = R_eJV4vfqtz9NxZqZ and R_3qQjxqxllJ5bcTb
# change uids for these Ps to reflect the correct start time
# new uid for ResponseID = R_eJV4vfqtz9NxZqZ becomes 18041213_1_d2
# new uid for ResponseID = R_3qQjxqxllJ5bcTb becomes 18041213_1_g2
#
# 2. Remove ResponseID == R_3JiH3ZnPGYh1jEx from data

# ISSUE 2:
#  identified that the following uids were recorded incorrectly
# 1.    "18041210_2_g1"   session started at 1pm so should be 18041213_2_g1
# 2.    "18041210_2_d1"   session started at 1pm so should be 18041213_2_d1

# SOLUTION 2:
# new uid for ResponseID == R_8vVbeNLAf4j2wYZ is 18041213_2_g1
# new uid for ResponseID == R_5tkA1uAHpTCFpu5 is 18041213_2_d1

adapt <- adapt %>% mutate(uid = ifelse(ResponseID == "R_eJV4vfqtz9NxZqZ", "18041213_1_d2", uid),
                          uid = ifelse(ResponseID == "R_3qQjxqxllJ5bcTb", "18041213_1_g2", uid),
                          uid = ifelse(ResponseID == "R_8vVbeNLAf4j2wYZ", "18041213_2_g1", uid),
                          uid = ifelse(ResponseID == "R_5tkA1uAHpTCFpu5", "18041213_2_d1", uid)) %>%
  filter(ResponseID != "R_3JiH3ZnPGYh1jEx")

n_questions <- 9 # number of questions in this test

x <- table(adapt$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
# first convert responses to numeric vector
adapt <- adapt %>%
  mutate(Stimulus.RESP = as.numeric(Stimulus.RESP))

x <- adapt %>%
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

check_values(x$Stimulus.RESP, 1, 7)


# factor == "cognitive" is spelt wrong
adapt$factor[adapt$factor == "cogntive"] <- "cognitive"

# convert RT vars to numeric
adapt <- adapt %>%
  group_by(uid, itemnum) %>% 
  mutate(Stimulus.OffsetTime = as.numeric(Stimulus.OffsetTime),
         Stimulus.OnsetTime = as.numeric(Stimulus.OnsetTime),
         Stimulus.RT = Stimulus.OffsetTime - Stimulus.OnsetTime)

# visualise RTs for each participant
# very slow RTs are not an issue
# identified uid == "19020812_2_d2" answered 7 items in less than 300ms
# uid == "19020812_2_g2" answered 6 items in less than 600ms
# adapt %>%
#   ggplot(aes(Stimulus.RT, colour = itemnum)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(adapt) +
#   geom_point(aes(x = Stimulus.RT, y = itemnum))
# 
# adapt %>%
#   filter(Stimulus.RT < 1000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# adapt %>%
#   filter(Stimulus.RT < 1000) %>%
#   ggplot() +
#   geom_point(aes(x = itemnum, y = Stimulus.RT)) +
#   facet_wrap(~ uid)


# calculate variables
adapt.uid <- adapt %>%
  group_by(uid) %>%
  summarise(adapt.cognitive = mean(Stimulus.RESP[factor == "cognitive"], na.rm = T),
            adapt.affective = mean(Stimulus.RESP[factor == "affective"], na.rm = T),
            adapt.global = mean(Stimulus.RESP, na.rm = T))


# Reliability ----------------------------------------------------
# facet <- unique(adapt$factor)
# 
# for (i in facet) {
# x <- adapt %>%
#   filter(factor == i) %>% # calculate for each factor
#   select(uid, itemnum, Stimulus.RESP) %>%
#   spread(itemnum, Stimulus.RESP) %>%
#   select(-uid)
# 
# print(paste0("Alpha for ", i))
# print(psych::alpha(x)$total$raw_alpha)
# 
# }
# 
# # overall reliability
# x <- adapt %>%
#   select(uid, itemnum, Stimulus.RESP) %>%
#   spread(itemnum, Stimulus.RESP) %>%
#   select(-uid)
# 
# print(psych::alpha(x)$total$raw_alpha)
