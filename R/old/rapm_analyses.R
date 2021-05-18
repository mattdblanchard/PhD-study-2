library("corrr")
library("tidyverse")

# function to calculate Gamma correlations
goodman <- function(x,y){
  Rx <- outer(x,x,function(u,v) sign(u-v))
  Ry <- outer(y,y,function(u,v) sign(u-v))
  S1 <- Rx*Ry
  return(sum(S1)/sum(abs(S1)))}

# Function to mean centre data
standardise <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}



# INDIVIDUALS -------------------------------------------------------------
d_rapm_uid <- read_csv("data/control/rapm_ind.csv")

d_rapm_uid <- d_rapm_uid %>%
  group_by(uid) %>%
  mutate(# standard_conf = standardise(Ind_Confidence.RESP),
         cen_conf = scale(Ind_Confidence.RESP, center = TRUE, scale = FALSE),
         # standard_conf2 = standard_conf^2,
         cen_conf2 = cen_conf^2,
         log.RT = log(Ind_Stimulus.RT))

# remove rows with missing values
x <- x[!is.na(x$cen_conf), ]
x <- x[!is.na(x$log.RT), ]

# quantile(x$cen_conf, probs = c(.33, .67))
# quantile(x$log.RT, probs = c(.33, .67))

# gamma at the individual level for group accuracy and confidence
x <- d_rapm_uid %>%
  mutate(conf = ifelse(cen_conf <= -0.2436543, "lo", ifelse(cen_conf >= 0.5745408, "hi", "mid")),
         Grp_Decision.RESP = tolower(Ind_Decision.RESP),
         speed = ifelse(log.RT >= 10.73091, "slow", ifelse(log.RT <= 10.07294, "fast", "mid")))




# CONFIDENCE
# Pearson correlation between standarised confidence and log time
x %>% group_by(uid, conf) %>%
  summarise(r = cor(standard_conf, log.RT)) %>%
  group_by(conf) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma between standarised confidence and log time
x %>% group_by(uid, conf) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(conf) %>%
  summarise(r = mean(r, na.rm = TRUE))


# SPEED
# Pearson correlation between standarised confidence and log time
x %>% group_by(uid, speed) %>%
  summarise(r = cor(standard_conf, log.RT)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma between standarised confidence and log time
x %>% group_by(uid, speed) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))


# BET DECISION
x <- x[!is.na(x$Ind_Decision.RESP), ]

# Pearson correlation between standarised confidence and log time
x %>% group_by(uid, Ind_Decision.RESP) %>%
  summarise(r = cor(standard_conf, log.RT)) %>%
  group_by(Ind_Decision.RESP) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma between standarised confidence and log time
x %>% group_by(uid, Ind_Decision.RESP) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(Ind_Decision.RESP) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson correlations for groups at the individual level
hi <- x %>% filter(conf == "hi")
lo <- x %>% filter(conf == "lo")

hi[, sapply(hi, is.numeric)] %>% 
  correlate() %>% fashion()

lo[, sapply(lo, is.numeric)] %>% 
  correlate() %>% fashion()

yes <- x %>% filter(Grp_Decision.RESP == "y") %>%
  select(-Grp_Decision.RESP)

no <- x %>% filter(Grp_Decision.RESP == "n") %>%
  select(-Grp_Decision.RESP)

yes[, sapply(yes, is.numeric)] %>% 
  correlate() %>% fashion()

no[, sapply(no, is.numeric)] %>% 
  correlate() %>% fashion()

# create combined confidence and speed categories
x <- x %>%
  unite(confXspeed, conf, speed, sep = "_")

x %>% group_by(uid, confXspeed) %>%
  summarise(r = cor(standard_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

x %>% group_by(uid, confXspeed) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

yes <- x %>% filter(Grp_Decision.RESP == "y") %>%
  select(-Grp_Decision.RESP)

yes %>% group_by(uid, confXspeed) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

no <- x %>% filter(Grp_Decision.RESP == "n") %>%
  select(-Grp_Decision.RESP)

no %>% group_by(uid, confXspeed) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

yes[, sapply(yes, is.numeric)] %>% 
  correlate() %>% fashion()

no[, sapply(no, is.numeric)] %>% 
  correlate() %>% fashion()






















# TEAMS -------------------------------------------------------------------
d_rapm_grp <- read_csv("data/control/rapm_team.csv")

d_rapm_grp <- d_rapm_grp %>%
  group_by(group) %>%
  mutate(standard_conf = standardise(Grp_Confidence.RESP),
         log.RT = log(Grp_Stimulus.RT))


# gamma at the individual level for group accuracy and confidence
x <- d_rapm_grp %>%
  select(group, Grp_Stimulus.ACC, Grp_Stimulus.RT, Grp_Confidence.RESP, standard_conf, log.RT, Grp_Decision.RESP) %>%
  mutate(conf = ifelse(standard_conf <= -0.0729706, "lo", ifelse(standard_conf >= 0.5332491, "hi", "mid")),
         Grp_Decision.RESP = tolower(Grp_Decision.RESP),
         speed = ifelse(log.RT >= 11.44803, "slow", ifelse(log.RT <= 10.51426, "fast", "mid")),
         conf1 = conf,
         speed1 = speed) %>%
  unite(confXspeed, conf1, speed1, sep = "_")

# remove rows with missing values
x <- x[!is.na(x$standard_conf), ]
x <- x[!is.na(x$log.RT), ]
x <- x[!is.na(x$Grp_Decision.RESP), ]

# quantile(x$standard_conf, probs = c(.33, .67))
# quantile(x$log.RT, probs = c(.33, .67))


# Standarised confidence and log time -------------------------------------
# pearson overall
x %>% group_by(group) %>%
  summarise(r = cor(standard_conf, log.RT)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by confidence level
x %>% group_by(group, conf) %>%
  summarise(r = cor(standard_conf, log.RT)) %>%
  group_by(conf) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by speed
x %>% group_by(group, speed) %>%
  summarise(r = cor(standard_conf, log.RT)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = cor(standard_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by bet decision
x %>% group_by(group, Grp_Decision.RESP) %>%
  summarise(r = cor(standard_conf, log.RT)) %>%
  group_by(Grp_Decision.RESP) %>%
  summarise(r = mean(r, na.rm = TRUE))


# gamma overall
x %>% group_by(group) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level
x %>% group_by(group, conf) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(conf) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by speed
x %>% group_by(group, speed) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by bet decision
x %>% group_by(group, Grp_Decision.RESP) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(Grp_Decision.RESP) %>%
  summarise(r = mean(r, na.rm = TRUE))


# Standardised confidence and Accuracy --------------------------------
x <- x %>% mutate(Grp_Decision.RESP = ifelse(Grp_Decision.RESP == "y", 1, 0))

# pearson overall 
x %>% group_by(group) %>%
  summarise(r = cor(standard_conf, Grp_Stimulus.ACC)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by confidence level
x %>% group_by(group, conf) %>%
  summarise(r = cor(standard_conf, Grp_Stimulus.ACC)) %>%
  group_by(conf) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by speed
x %>% group_by(group, speed) %>%
  summarise(r = cor(standard_conf, Grp_Stimulus.ACC)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = cor(standard_conf, Grp_Stimulus.ACC)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma overall
x %>% group_by(group) %>%
  summarise(r = goodman(standard_conf, Grp_Stimulus.ACC)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level
x %>% group_by(group, conf) %>%
  summarise(r = goodman(standard_conf, Grp_Stimulus.ACC)) %>%
  group_by(conf) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by speed
x %>% group_by(group, speed) %>%
  summarise(r = goodman(standard_conf, Grp_Stimulus.ACC)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = goodman(standard_conf, Grp_Stimulus.ACC)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))



# Standardised confidence and bet decision --------------------------------
x <- x %>% mutate(Grp_Decision.RESP = ifelse(Grp_Decision.RESP == "y", 1, 0))

# pearson overall 
x %>% group_by(group) %>%
  summarise(r = cor(standard_conf, Grp_Decision.RESP)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by confidence level
x %>% group_by(group, conf) %>%
  summarise(r = cor(standard_conf, Grp_Decision.RESP)) %>%
  group_by(conf) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by speed
x %>% group_by(group, speed) %>%
  summarise(r = cor(standard_conf, Grp_Decision.RESP)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = cor(standard_conf, Grp_Decision.RESP)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma overall
x %>% group_by(group) %>%
  summarise(r = goodman(standard_conf, Grp_Decision.RESP)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level
x %>% group_by(group, conf) %>%
  summarise(r = goodman(standard_conf, Grp_Decision.RESP)) %>%
  group_by(conf) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by speed
x %>% group_by(group, speed) %>%
  summarise(r = goodman(standard_conf, Grp_Decision.RESP)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# pearson by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = goodman(standard_conf, Grp_Decision.RESP)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))


x[, sapply(x, is.numeric)] %>% 
  correlate() %>% fashion()

# pearson correlations for groups at the individual level
hi <- x %>% filter(conf == "hi")
lo <- x %>% filter(conf == "lo")

hi[, sapply(hi, is.numeric)] %>% 
  correlate() %>% fashion()

lo[, sapply(lo, is.numeric)] %>% 
  correlate() %>% fashion()


x <- x[!is.na(x$Grp_Decision.RESP), ]


x %>% group_by(group, Grp_Decision.RESP) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(Grp_Decision.RESP) %>%
  summarise(r = mean(r, na.rm = TRUE))

yes <- x %>% filter(Grp_Decision.RESP == "y") %>%
  select(-Grp_Decision.RESP)

no <- x %>% filter(Grp_Decision.RESP == "n") %>%
  select(-Grp_Decision.RESP)

yes[, sapply(yes, is.numeric)] %>% 
  correlate() %>% fashion()

no[, sapply(no, is.numeric)] %>% 
  correlate() %>% fashion()

# create combined confidence and speed categories

  



yes <- x %>% filter(Grp_Decision.RESP == "y") %>%
  select(-Grp_Decision.RESP)

yes %>% group_by(group, confXspeed) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

no <- x %>% filter(Grp_Decision.RESP == "n") %>%
  select(-Grp_Decision.RESP)

no %>% group_by(group, confXspeed) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

yes[, sapply(yes, is.numeric)] %>% 
  correlate() %>% fashion()

no[, sapply(no, is.numeric)] %>% 
  correlate() %>% fashion()




# original confidence and time variables
# remove rows with missing values
x <- x[!is.na(x$Grp_Confidence.RESP), ]
x <- x[!is.na(x$Grp_Stimulus.RT), ]

# hi <- x %>%
#   filter(standard_conf >= 0)
# 
# lo <- x %>%
#   filter(standard_conf < 0)
)
x %>% group_by(group, conf) %>%
  summarise(r = goodman(Grp_Confidence.RESP, Grp_Stimulus.RT)) %>%
  group_by(conf) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson correlations for groups at the individual level
hi <- x %>% filter(conf == "hi")
lo <- x %>% filter(conf == "lo")

hi[, sapply(hi, is.numeric)] %>% 
  correlate() %>% fashion()

lo[, sapply(lo, is.numeric)] %>% 
  correlate() %>% fashion()


x <- x[!is.na(x$Grp_Decision.RESP), ]


x %>% group_by(group, Grp_Decision.RESP) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(Grp_Decision.RESP) %>%
  summarise(r = mean(r, na.rm = TRUE))

yes <- x %>% filter(Grp_Decision.RESP == "y") %>%
  select(-Grp_Decision.RESP)

no <- x %>% filter(Grp_Decision.RESP == "n") %>%
  select(-Grp_Decision.RESP)

yes[, sapply(yes, is.numeric)] %>% 
  correlate() %>% fashion()

no[, sapply(no, is.numeric)] %>% 
  correlate() %>% fashion()

# create combined confidence and speed categories
x <- x %>%
  unite(confXspeed, conf, speed, sep = "_")

x %>% group_by(group, confXspeed) %>%
  summarise(r = goodman(standard_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

yes <- x %>% filter(Grp_Decision.RESP == "y") %>%
  select(-Grp_Decision.RESP)

no <- x %>% filter(Grp_Decision.RESP == "n") %>%
  select(-Grp_Decision.RESP)

yes[, sapply(yes, is.numeric)] %>% 
  correlate() %>% fashion()

no[, sapply(no, is.numeric)] %>% 
  correlate() %>% fashion()