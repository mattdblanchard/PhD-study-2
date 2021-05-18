# MiniIPIP ----------------------------------------------------------------
tmp1 <- surveys_qrt$mini_IPIP_A1 %>% 
  select(ResponseID, StartDate:Finished, uid, # MINIIPIPtest.TrialNr,
         factor, key, Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, 
         Stimulus.OffsetTime) %>%
  #  rename(Itemnum = MINIIPIPtest.TrialNr) %>%
  mutate(version = "a1")

tmp2 <- surveys_qrt$mini_IPIP_B1 %>% 
  select(ResponseID, StartDate:Finished, uid, # MINIIPIPtest.TrialNr,
         factor, key, Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, 
         Stimulus.OffsetTime) %>%
  #  rename(Itemnum = MINIIPIPtest.TrialNr) %>%
  mutate(version = "b1")

# surveys_java$MiniIPIP_A2 %>%
#   group_by(uid) %>%
#   mutate(Itemnum = n()) %>%
#   select(uid, item, Itemnum)

tmp3 <- surveys_java$mini_IPIP_A2 %>%
  rename(ResponseID = ResponseId, Stimulus.RESP = response, 
         Stimulus.OnsetTime = onset, Stimulus.OffsetTime = offset, 
         Stimulus.RT = rt, factor = facet) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid,
         factor, key, Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, 
         Stimulus.OffsetTime) %>%
  mutate(version = "a2")

tmp4 <- surveys_java$mini_IPIP_B2 %>%
  rename(ResponseID = ResponseId, Stimulus.RESP = response, 
         Stimulus.OnsetTime = onset, Stimulus.OffsetTime = offset, 
         Stimulus.RT = rt, factor = facet) %>%
  select(ResponseID, StartDate, EndDate, Finished, uid,
         factor, key, Stimulus.RESP, Stimulus.RT, Stimulus.OnsetTime, 
         Stimulus.OffsetTime) %>%
  mutate(version = "b2")

# combine data into a single df 
ipip <- rbind(tmp1, tmp2, tmp3, tmp4)

# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
# ISSUE 1: 
# identified that the following uids appear twice
# 1.    "18041210_1_d2"   = all responses legit, P from 1pm session assigned incorrect uid
# 1.    "18041210_1_g2"   = all responses legit, P from 1pm session assigned incorrect uid
# 1.    "18041210_2_d1"   = all responses legit, P from 1pm session assigned incorrect uid
# 1.    "18041210_2_g1"   = all responses legit, P from 1pm session assigned incorrect uid
# SOLUTION: 
# change uids for Ps in 1pm session to reflect the correct start time
# new uid for ResponseID = R_5zh4gP0x8XGWWs5 becomes 18041213_1_d2
# new uid for ResponseID = R_d7tWP92nnNjv6C1 becomes 18041213_1_g2
# new uid for ResponseID = R_9skqG9xRVOs6Xrv becomes 18041213_2_d1
# new uid for ResponseID = R_82CDpPsNNqz6c85 becomes 18041213_2_g1
#
# ISSUE 2:
# Identified that uid == "18081416_2_e1" completed the mini-ipip twice
# SOLUTION:
# Remove second attempt from data, ResponseID == R_2eaPPKJnrYg2zaI 

ipip <- ipip %>% mutate(uid = ifelse(ResponseID == "R_5zh4gP0x8XGWWs5", "18041213_1_d2", uid),
                        uid = ifelse(ResponseID == "R_d7tWP92nnNjv6C1", "18041213_1_g2", uid),
                        uid = ifelse(ResponseID == "R_9skqG9xRVOs6Xrv", "18041213_2_d1", uid),
                        uid = ifelse(ResponseID == "R_82CDpPsNNqz6c85", "18041213_2_g1", uid)) %>%
  filter(ResponseID != "R_2eaPPKJnrYg2zaI")

n_questions <- 20 # number of questions in this test

x <- table(ipip$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================
# create vector to select only uid and participants responses
x <- ipip %>%
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

# convert RT vars to numeric
ipip <- ipip %>%
  group_by(uid) %>% 
  mutate(Stimulus.RESP = as.numeric(Stimulus.RESP),
         Stimulus.OffsetTime = as.numeric(Stimulus.OffsetTime),
         Stimulus.OnsetTime = as.numeric(Stimulus.OnsetTime),
         Stimulus.RT = Stimulus.OffsetTime - Stimulus.OnsetTime)

check_values(ipip$Stimulus.RESP, 1, 5)


# visualise RTs for each participant
# very slow RTs are not an issue
# identified 6 participants with problematic patterns of RTs
#' uid == "17103112_2_g1" 17 responses < 1sec
#' uid == "18101713_1_d1" 16 responses < 1sec
#' uid == "18082008_1_e1" 14 responses < 1sec
#' uid == "19020112_1_g2" 13 responses < 1sec
#' uid == "17101809_1_g2" 11 responses < 1sec
#' uid == "19021212_1_d1" 8 responses < 1sec
# 
# ipip %>%
#   ggplot(aes(Stimulus.RT, colour = factor)) +
#   geom_density() +
#   theme_minimal()
# 
# ggplot(ipip) +
#   geom_point(aes(x = Stimulus.RT, y = factor))
# 
# ipip %>%
#   filter(Stimulus.RT < 1000) %>%
#   group_by(uid) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n))
# 
# ipip %>%
#   group_by(uid) %>%
#   mutate(itemnum = 1:n()) %>%
#   filter(Stimulus.RT < 1000) %>%
#   ggplot() +
#   geom_point(aes(x = itemnum, y = Stimulus.RT)) +
#   facet_wrap(~ uid)

# session notes indicate that uid == 18082114_2_e1 misread the instructions and requested 
# their responses to first 2 questions be reversed (e.g., 1 becomes 5)
ipip <- ipip %>% 
  mutate(Stimulus.RESP = ifelse(uid == "18082114_2_e1" & factor == "extraversion" & Stimulus.RESP == 1, 6 - Stimulus.RESP, Stimulus.RESP))


# Compute Variables -------------------------------------------------------
# score personality factors

ipip.uid <- as.data.frame(ipip %>%
  mutate(Stimulus.RESP = ifelse(key == 1, Stimulus.RESP, 6 - Stimulus.RESP)) %>%  # Reverse score negative items
  group_by(uid, factor) %>%
  summarise(
    score = mean(Stimulus.RESP, na.rm = TRUE)
  ) %>%
  spread(factor, score))


# Reliability ----------------------------------------------------
# facet <- unique(ipip$factor)
# 
# for (i in facet) {
# x <- ipip %>%
#   group_by(uid, factor) %>%
#   mutate(itemnum = 1:n()) %>%
#   ungroup() %>%
#   mutate(Stimulus.RESP = ifelse(key == 1, Stimulus.RESP,
#                                 6 - Stimulus.RESP))  %>% # Reverse score negative items
#   filter(factor == i) %>% # calculate for each factor
#   select(uid, itemnum, Stimulus.RESP) %>%
#   spread(itemnum, Stimulus.RESP) %>%
#   select(-uid)
# 
# print(paste0("Alpha for ", i))
# print(psych::alpha(x)$total$raw_alpha)
# 
# }
