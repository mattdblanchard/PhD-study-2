library(tidyverse)
library(qrtenginer)
library(psych)
library(jsonlite)
library(rjson)


# PLAY - new survey data--------------------------------------------------------------------
d <- parse_csv("data/test_java/gktest.csv")
d <- read_csv("data/test_java/gktest.csv")

json <- d[-1,] %>% select(contains("stimulus"))

do.call(rbind.data.frame, rjson::fromJSON(json))

p <- rjson::newJSONParser()
p$addData(d)
df <- p$getObject()
do.call(rbind, df)

# END PLAY ----------------------------------------------------------------




# OLD SURVEYS -------------------------------------------------------------
# Trust scale
d <- parse_csv("data/test_b/trust.csv")

x <- d %>%
  select(uid, TrustScale.TrialNr, Stimulus.RESP)

# Adaptability scale
d <- parse_csv("data/test_b/adapt.csv")

x <- d %>%
  select(uid, AdaptScale.TrialNr, Stimulus.RESP)

# Risk aversion scale
d <- parse_csv("data/test_b/risk.csv")

x <- d %>%
  select(uid, RiskAversion.TrialNr, Stimulus.RESP)

# for the new measures only Qualtrics gives an extra ExitTest_1 column 
# so may need to change parse script so that matches = number of valid items
#     select(matches("ExitTest_1_TEXT.[0-9].")) %>%
d <- parse_csv("data/test_data/RAPM_team_test.csv")

d <- rename(d, ResponseID = V1, ResponseSet = V2, Name = V3, ExternalDataReference = V4, EmailAddress = V5, 
            IPAddress = V6, Status = V7, StartDate = V8, EndDate = V9, Finished = V10)

colnames(d)

x <- d %>% select(ResponseID, uid, StartDate, ApmItemNum, Grp_Stimulus.RESP, Grp_Stimulus.ACC, Grp_Confidence.RESP, Grp_Decision.RESP) %>%
  mutate(
    Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP),
    Grp_Stimulus.ACC = as.numeric(Grp_Stimulus.ACC),
    Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
    Grp_Decision.RESP = as.numeric(Grp_Decision.RESP))

x[, sapply(x, is.numeric)] %>%
  describe()

x %>% filter(Grp_Confidence.RESP < 13)

# NEED TO CHECK CALCULATION OF RECKLESSNESS

# NEED TO CHECK CALCULATION OF DECISIVENESS


# Running Letters
# test data
# no RT?
d <- parse_csv("data/test_b/run.csv")

d <- rename(d, ResponseID = V1, ResponseSet = V2, Name = V3, ExternalDataReference = V4, EmailAddress = V5, 
            IPAddress = V6, Status = V7, StartDate = V8, EndDate = V9, Finished = V10)

x <- d %>% select(ResponseID, uid, Stimulus.CRESP, Stimulus.RESP, Stimulus.ACC, Stimulus.RT, RUNNINGtest.TrialNr)

n_questions <- 14  # Number of questions in this test


# Data Screening ----------------------------------------------------------

# Check participants appear correct number of times =======================
x <- table(x$uid)

if (any(x != n_questions)) {
  print("Participants not appearing the correct number of times:")
  x[x != n_questions]
} else {
  print("All good! :)")
}

# Check for missing values ================================================

# create vector to select only uid and stimulus
x <- tmp %>%
  select(uid, response)

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


# visually check that there are no missing values per user pervariable
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


check_values(x$response, 1, 5)


d %>%
  group_by(uid) %>%
  summarise(acc = mean(Stimulus.ACC.),
            rt = mean(Stimulus.OffsetTime. - Stimulus.OnsetTime.))

colnames(d)

# RAPM - individual
d <- parse_csv("data/test_b/rapm_ind.csv")

table(d$uid)

x <- d %>%
  select(uid, ApmItemNum, Ind_Stimulus.ACC, Ind_Stimulus.RESP, Ind_Stimulus.CRESP, 
         Ind_Stimulus.RT, Ind_Confidence.RESP, Ind_Confidence.OffsetTime, 
         Ind_Confidence.OnsetTime, Ind_Decision.RESP, 
         Ind_Decision.RT, Ind_Decision.OffsetTime, Ind_Decision.OnsetTime) %>%
  mutate(
    Ind_Stimulus.ACC = as.numeric(Ind_Stimulus.ACC),
    Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
    Ind_Stimulus.RT = as.numeric(Ind_Stimulus.RT),
    Ind_Decision.RT = as.numeric(Ind_Decision.RT),
    Ind_Decision.OffsetTime = as.numeric(Ind_Decision.OffsetTime),
    Ind_Decision.OnsetTime = as.numeric(Ind_Decision.OnsetTime))

x %>%
  group_by(uid) %>%
  summarise(rapm.ind.acc = mean(Ind_Stimulus.ACC),
            rapm.ind.acc.rt = mean(Ind_Stimulus.RT),
            rapm.ind.conf = mean(Ind_Confidence.RESP),
            rapm.ind.conf.rt = mean(Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime),
            rapm.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
            rapm.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
            / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
            rapm.ind.decision.rt = mean(Ind_Decision.RT))


d %>% select(uid, Ind_Stimulus.RESP., Ind_Confidence.RESP., Ind_Decision.RESP.) %>%
  filter(uid == "17100612_2_g1")

# RAPM - team
d <- parse_csv("data/test_b/rapm_team.csv")

table(d$uid)

x <- d %>%
  select(uid, ApmItemNum, Grp_Stimulus.ACC, Grp_Stimulus.RESP, Grp_Stimulus.CRESP, 
         Grp_Stimulus.RT, Grp_Confidence.RESP,
         Grp_Decision.RESP, Grp_Decision.RT) %>%
  mutate(
    Grp_Stimulus.ACC = as.numeric(Grp_Stimulus.ACC),
    Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
    # Grp_Confidence.OffsetTime - as.numeric(Grp_Confidence.OffsetTime),
    # Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
    Grp_Stimulus.RT = as.numeric(Grp_Stimulus.RT),
    Grp_Decision.RT = as.numeric(Grp_Decision.RT))
    # Grp_Decision.OffsetTime = as.numeric(Grp_Decision.OffsetTime),
    # Grp_Decision.OnsetTime = as.numeric(Grp_Decision.OnsetTime))

x %>%
  group_by(uid) %>%
  summarise(rapm.grp.acc = mean(Grp_Stimulus.ACC) * 100,
            rapm.grp.acc.rt = mean(Grp_Stimulus.RT),
            rapm.grp.conf = mean(Grp_Confidence.RESP),
            # rapm.grp.conf.rt = mean(Grp_Confidence.RT),
            rapm.grp.dec = mean(Grp_Decision.RESP == "y") * 100,
            rapm.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
            / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
            rapm.grp.decision.rt = mean(Grp_Decision.RT))


# MDMT - individual
d <- parse_csv("data/test_b/mdmt_ind.csv")

table(d$uid)

x <- d %>%
  select(uid, MDMTindividual.TrialNr, Ind_Stimulus.ACC, Ind_Stimulus.RESP, Ind_Stimulus.CRESP, 
         Ind_Stimulus.RT, Ind_Confidence.RESP, Ind_Confidence.OffsetTime, 
         Ind_Confidence.OnsetTime, Ind_Decision.RESP, 
         Ind_Decision.RT, Ind_Decision.OffsetTime, Ind_Decision.OnsetTime) %>%
  mutate(
    Ind_Stimulus.ACC = as.numeric(Ind_Stimulus.ACC),
    Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
    Ind_Stimulus.RT = as.numeric(Ind_Stimulus.RT),
    Ind_Decision.RT = as.numeric(Ind_Decision.RT),
    Ind_Decision.OffsetTime = as.numeric(Ind_Decision.OffsetTime),
    Ind_Decision.OnsetTime = as.numeric(Ind_Decision.OnsetTime))

x %>%
  group_by(uid) %>%
  summarise(mdmt.ind.acc = mean(Ind_Stimulus.ACC) * 100,
            mdmt.ind.acc.rt = mean(Ind_Stimulus.RT),
            mdmt.ind.conf = mean(Ind_Confidence.RESP),
            mdmt.ind.conf.rt = mean(Ind_Decision.OffsetTime - Ind_Decision.OnsetTime),
            mdmt.ind.dec = mean(Ind_Decision.RESP == "a") * 100,
            mdmt.ind.rec = 100 * mean(Ind_Decision.RESP == "a" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
            / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
            mdmt.ind.decision.rt = mean(Ind_Decision.RT))


# MDMT - team
d <- parse_csv("data/test_b/mdmt_team.csv")

table(d$uid)

x <- d %>%
  select(uid, MDMTteam.TrialNr, Grp_Stimulus.ACC, Grp_Stimulus.RESP, Grp_Stimulus.CRESP, 
         Grp_Stimulus.RT, Grp_Confidence.RESP,
         Grp_Decision.RESP, Grp_Decision.RT) %>%
  mutate(
    Grp_Stimulus.ACC = as.numeric(Grp_Stimulus.ACC),
    Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
    # Grp_Confidence.OffsetTime - as.numeric(Grp_Confidence.OffsetTime),
    # Grp_Confidence.OnsetTime = as.numeric(Grp_Confidence.OnsetTime),
    Grp_Stimulus.RT = as.numeric(Grp_Stimulus.RT),
    Grp_Decision.RT = as.numeric(Grp_Decision.RT))
# Grp_Decision.OffsetTime = as.numeric(Grp_Decision.OffsetTime),
# Grp_Decision.OnsetTime = as.numeric(Grp_Decision.OnsetTime))

x %>%
  group_by(uid) %>%
  summarise(mdmt.grp.acc = mean(Grp_Stimulus.ACC) * 100,
            mdmt.grp.acc.rt = mean(Grp_Stimulus.RT),
            mdmt.grp.conf = mean(Grp_Confidence.RESP),
            # mdmt.grp.conf.rt = mean(Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime),
            mdmt.grp.dec = mean(Grp_Decision.RESP == "a") * 100,
            mdmt.grp.rec = 100 * mean(Grp_Decision.RESP == "a" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
            / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
            mdmt.grp.decision.rt = mean(Grp_Decision.RT))

# GK test
d <- parse_csv("data/test_b/gk.csv")

table(d$uid)

x <- d %>%
  select(uid, GKtest.TrialNr, Ind_Stimulus.ACC, Ind_Stimulus.RESP, Ind_Stimulus.CRESP, Ind_Stimulus.RT,
         Grp_Stimulus.ACC, Grp_Stimulus.RESP, Grp_Stimulus.CRESP, Grp_Stimulus.RT,
         Ind_Confidence.RESP, Ind_Confidence.OffsetTime, Ind_Confidence.OnsetTime, 
         Ind_Confidence.RESP, Grp_Confidence.RESP, Ind_Decision.RESP, Grp_Decision.RESP,
         Ind_Decision.RT, Grp_Decision.RT, Ind_Decision.OffsetTime, Ind_Decision.OnsetTime, 
         Grp_Decision.OffsetTime, Grp_Decision.OnsetTime) %>%
  mutate(
    Ind_Stimulus.ACC = as.numeric(Ind_Stimulus.ACC),
    Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
    Ind_Stimulus.RT = as.numeric(Ind_Stimulus.RT),
    Ind_Decision.RT = as.numeric(Ind_Decision.RT),
    Ind_Decision.OffsetTime = as.numeric(Ind_Decision.OffsetTime),
    Ind_Decision.OnsetTime = as.numeric(Ind_Decision.OnsetTime),
    Grp_Stimulus.ACC = as.numeric(Grp_Stimulus.ACC),
    Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
    Grp_Stimulus.RT = as.numeric(Grp_Stimulus.RT),
    Grp_Decision.RT = as.numeric(Grp_Decision.RT),
    Grp_Decision.OffsetTime = as.numeric(Grp_Decision.OffsetTime),
    Grp_Decision.OnsetTime = as.numeric(Grp_Decision.OnsetTime))


x %>%
  group_by(uid) %>%
  summarise(gk.ind.acc = mean(Ind_Stimulus.ACC) * 100,
            gk.grp.acc = mean(Grp_Stimulus.ACC) * 100,
            gk.ind.acc.rt = mean(Ind_Stimulus.RT),
            gk.grp.acc.rt = mean(Grp_Stimulus.RT),
            gk.ind.conf = mean(Ind_Confidence.RESP),
            gk.grp.conf = mean(Grp_Confidence.RESP),
            # gk.ind.conf.rt = mean(Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime),
            # gk.grp.conf.rt = mean(Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime),
            gk.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
            gk.grp.dec = mean(Grp_Decision.RESP == "y") * 100,
            gk.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
            / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
            gk.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
            / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
            gk.ind.decision.rt = mean(Ind_Decision.RT),
            gk.grp.decision.rt = mean(Grp_Decision.RT))


# Resistance to Framing

# Positive
d <- parse_csv("data/test_b/r2f_pos.csv")

table(d$uid)
colnames(d)

# pos %>%
#   group_by(uid) %>%
#   summarise(rtf.ind.resp_pos = mean(Ind_Stimulus.RESP.),
#             rtf.grp.resp_pos = mean(Grp_Stimulus.RESP.),
#             rtf.ind.resp.rt_pos = mean(Ind_Stimulus.RT.),
#             rtf.grp.resp.rt_pos = mean(Grp_Stimulus.RT.))

# add R2FItemNum and separate letter and number so that numbers are same for merge
x <- d %>%
  select(uid, R2FItemNum, Ind_Stimulus.RESP, Grp_Stimulus.RESP)

# change names to match neg df
names(x) <- c("uid", "R2FItemNum", "Ind_Stimulus_Pos.RESP", "Grp_Stimulus_Pos.RESP.")

# Negative
d <- parse_csv("data/test_b/r2f_neg.csv")

x <- d %>%
  select(uid, R2FItemNum, Ind_Stimulus.RESP, Grp_Stimulus.RESP)

# Parse creates variables with character class, need to convert varaibles to numeric
# This code has issues, use code below
# neg[, c("R2FItemNum", "R2Fnegative.TrialNr", "Ind_Stimulus.RESP", "Grp_Stimulus.RESP", "Ind_Stimulus.RT", "Grp_Stimulus.RT", 
#       "Grp_Stimulus.OffsetTime", "Grp_Stimulus.OnsetTime", "Wait.OnsetTime", "Wait.OffsetTime")] <- 
#   neg[, c("R2FItemNum", "R2Fnegative.TrialNr", "Ind_Stimulus.ACC", "Grp_Stimulus.ACC", "Ind_Stimulus.RT", "Grp_Stimulus.RT", 
#         "Grp_Stimulus.OffsetTime", "Grp_Stimulus.OnsetTime", "Wait.OnsetTime", "Wait.OffsetTime")] %>%
#   map(as.numeric)

neg <- transform(neg, Ind_Stimulus.RESP = as.numeric(Ind_Stimulus.RESP))
neg <- transform(neg, Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP))
# neg <- transform(neg, R2FItemNum = as.numeric(R2FItemNum))
neg <- transform(neg, R2Fnegative.TrialNr = as.numeric(R2Fnegative.TrialNr))
neg <- transform(neg, Ind_Stimulus.RT = as.numeric(Ind_Stimulus.RT))
neg <- transform(neg, Grp_Stimulus.RT = as.numeric(Grp_Stimulus.RT))
neg <- transform(neg, uid = as.factor(uid))
neg <- transform(neg, R2FItemNum = as.factor(R2FItemNum))

# Parse creates extra columns full of NAs, need to remove these columns
neg <- neg %>%
  filter(!is.na(Ind_Stimulus.RESP)) 

table(neg$uid)

# add R2FItemNum and separate letter and number so that numbers are same for merge
y <- neg %>%
  select(uid, R2FItemNum, Ind_Stimulus.RESP, Grp_Stimulus.RESP)

names(y) <- c("uid", "R2FItemNum", "Ind_Stimulus_Neg.RESP", "Grp_Stimulus_Neg.RESP.")

# neg %>%
#   group_by(uid) %>%
#   summarise(rtf.ind.resp_neg = mean(Ind_Stimulus.RESP),
#             rtf.grp.resp_neg = mean(Grp_Stimulus.RESP),
#             rtf.ind.resp.rt_neg = mean(Ind_Stimulus.RT),
#             rtf.grp.resp.rt_neg = mean(Grp_Stimulus.RT))

# Not working. Creating 7x7 rows per person.
x <- x %>% left_join(y, by = "uid")


# Cognitive Reflection Test
d <- parse_csv("data/test_b/crt.csv")

table(d$uid)

x <- d %>%
  select(uid, CRtest.TrialNr, Ind_Stimulus.ACC, Ind_Stimulus.RESP, Ind_Stimulus.CRESP, Ind_Stimulus.RT,
         Grp_Stimulus.ACC, Grp_Stimulus.RESP, Grp_Stimulus.CRESP, Grp_Stimulus.RT,
         Ind_Confidence.RESP, Ind_Confidence.OffsetTime, Ind_Confidence.OnsetTime, 
         Ind_Confidence.RESP, Grp_Confidence.RESP, Ind_Decision.RESP, Grp_Decision.RESP,
         Ind_Decision.RT, Grp_Decision.RT, Ind_Decision.OffsetTime, Ind_Decision.OnsetTime, 
         Grp_Decision.OffsetTime, Grp_Decision.OnsetTime) %>%
  mutate(
    Ind_Stimulus.ACC = as.numeric(Ind_Stimulus.ACC),
    Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
    Ind_Stimulus.RT = as.numeric(Ind_Stimulus.RT),
    Ind_Decision.RT = as.numeric(Ind_Decision.RT),
    Ind_Decision.OffsetTime = as.numeric(Ind_Decision.OffsetTime),
    Ind_Decision.OnsetTime = as.numeric(Ind_Decision.OnsetTime),
    Grp_Stimulus.ACC = as.numeric(Grp_Stimulus.ACC),
    Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
    Grp_Stimulus.RT = as.numeric(Grp_Stimulus.RT),
    Grp_Decision.RT = as.numeric(Grp_Decision.RT),
    Grp_Decision.OffsetTime = as.numeric(Grp_Decision.OffsetTime),
    Grp_Decision.OnsetTime = as.numeric(Grp_Decision.OnsetTime))

x %>%
  group_by(uid) %>%
  summarise(gk.ind.acc = mean(Ind_Stimulus.ACC) * 100,
            gk.grp.acc = mean(Grp_Stimulus.ACC) * 100,
            # gk.ind.acc.rt = mean(Ind_Stimulus.RT),
            # gk.grp.acc.rt = mean(Grp_Stimulus.RT),
            gk.ind.conf = mean(Ind_Confidence.RESP),
            gk.grp.conf = mean(Grp_Confidence.RESP),
            # gk.ind.conf.rt = mean(Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime),
            # gk.grp.conf.rt = mean(Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime),
            gk.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
            gk.grp.dec = mean(Grp_Decision.RESP == "y") * 100,
            gk.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
            / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
            gk.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
            / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE))
            # gk.ind.decision.rt = mean(Ind_Decision.RT),
            # gk.grp.decision.rt = mean(Grp_Decision.RT))

# Applying Decision Rules
d <- parse_csv("data/test_b/adr.csv")

table(d$uid)

x <- d %>%
  select(uid, ADRtest.TrialNr, Ind_Stimulus.ACC, Ind_Stimulus.RESP, Ind_Stimulus.CRESP, Ind_Stimulus.RT,
         Grp_Stimulus.ACC, Grp_Stimulus.RESP, Grp_Stimulus.CRESP, Grp_Stimulus.RT,
         Ind_Confidence.RESP, Ind_Confidence.OffsetTime, Ind_Confidence.OnsetTime, 
         Ind_Confidence.RESP, Grp_Confidence.RESP, Ind_Decision.RESP, Grp_Decision.RESP,
         Ind_Decision.RT, Grp_Decision.RT, Ind_Decision.OffsetTime, Ind_Decision.OnsetTime, 
         Grp_Decision.OffsetTime, Grp_Decision.OnsetTime) %>%
  mutate(
    Ind_Stimulus.ACC = as.numeric(Ind_Stimulus.ACC),
    Ind_Confidence.RESP = as.numeric(Ind_Confidence.RESP),
    Ind_Stimulus.RT = as.numeric(Ind_Stimulus.RT),
    Ind_Decision.RT = as.numeric(Ind_Decision.RT),
    Ind_Decision.OffsetTime = as.numeric(Ind_Decision.OffsetTime),
    Ind_Decision.OnsetTime = as.numeric(Ind_Decision.OnsetTime),
    Grp_Stimulus.ACC = as.numeric(Grp_Stimulus.ACC),
    Grp_Confidence.RESP = as.numeric(Grp_Confidence.RESP),
    Grp_Stimulus.RT = as.numeric(Grp_Stimulus.RT),
    Grp_Decision.RT = as.numeric(Grp_Decision.RT),
    Grp_Decision.OffsetTime = as.numeric(Grp_Decision.OffsetTime),
    Grp_Decision.OnsetTime = as.numeric(Grp_Decision.OnsetTime))

x %>%
  group_by(uid) %>%
  summarise(gk.ind.acc = mean(Ind_Stimulus.ACC) * 100,
            gk.grp.acc = mean(Grp_Stimulus.ACC) * 100,
            # gk.ind.acc.rt = mean(Ind_Stimulus.OffsetTime - Ind_Stimulus.OnsetTime),
            # gk.grp.acc.rt = mean(Grp_Stimulus.OffsetTime - Grp_Stimulus.OnsetTime),
            gk.ind.conf = mean(Ind_Confidence.RESP),
            gk.grp.conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
            # gk.ind.conf.rt = mean(Ind_Confidence.OffsetTime - Ind_Confidence.OnsetTime),
            # gk.grp.conf.rt = mean(Grp_Confidence.OffsetTime - Grp_Confidence.OnsetTime),
            gk.ind.dec = mean(Ind_Decision.RESP == "y") * 100,
            gk.grp.dec = mean(Grp_Decision.RESP == "y") * 100,
            gk.ind.rec = 100 * mean(Ind_Decision.RESP == "y" & Ind_Stimulus.ACC == 0, na.rm = TRUE) 
            / mean(Ind_Stimulus.ACC == 0, na.rm = TRUE),
            gk.grp.rec = 100 * mean(Grp_Decision.RESP == "y" & Grp_Stimulus.ACC == 0, na.rm = TRUE) 
            / mean(Grp_Stimulus.ACC == 0, na.rm = TRUE),
            gk.ind.decision.rt = mean(Ind_Decision.RT),
            gk.grp.decision.rt = mean(Grp_Decision.RT))


# Composite Emotions Task
d <- parse_csv("data/test_b/cet.csv")

table(d$uid)
colnames(d)
x <- d %>%
  select(uid, CetItemNum, Stimulus.ACC, Stimulus.RESP, Stimulus.CRESP, 
         Stimulus.RT, Confidence.RESP, Confidence.OffsetTime, 
         Confidence.OnsetTime, Decision.RESP, 
         Decision.RT, Decision.OffsetTime, Decision.OnsetTime) %>%
  mutate(
    Stimulus.ACC = as.numeric(Stimulus.ACC),
    Confidence.RESP = as.numeric(Confidence.RESP),
    Stimulus.RT = as.numeric(Stimulus.RT),
    Decision.RT = as.numeric(Decision.RT),
    Decision.OffsetTime = as.numeric(Decision.OffsetTime),
    Decision.OnsetTime = as.numeric(Decision.OnsetTime))

x %>%
  group_by(uid) %>%
  summarise(cet.acc = mean(Stimulus.ACC) * 100,
            cet.acc.rt = mean(Stimulus.RT),
            cet.conf = mean(Confidence.RESP),
            # cet.conf.rt = mean(Decision.OffsetTime. - Decision.OnsetTime.),
            cet.dec = mean(Decision.RESP == "y") * 100,
            cet.rec = 100 * mean(Decision.RESP == "y" & Stimulus.ACC == 0, na.rm = TRUE) 
            / mean(Stimulus.ACC == 0, na.rm = TRUE),
            cet.decision.rt = mean(Decision.RT))


# Mini-IPIP
d <- parse_csv("data/test_b/ipip.csv")

table(d$uid)

x <- d %>%
  select(uid, MINIIPIPtest.TrialNr, Stimulus.RESP, Stimulus.RT, key, factor) %>%
  mutate(
    Stimulus.RESP = as.numeric(Stimulus.RESP),
    Stimulus.RT = as.numeric(Stimulus.RT))

x %>%
  group_by(uid) %>%
  mutate(Stimulus.RESP = ifelse(key == 1, Stimulus.RESP, 6 - Stimulus.RESP)) %>%  # Reverse score negative items
  group_by(uid, factor) %>%
  summarise(
    score = mean(Stimulus.RESP, na.rm = TRUE)
  ) %>%
  spread(factor, score)

