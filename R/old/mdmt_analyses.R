# Notes from RA meeting about cleaning responses
# look at RT for each individual to identify any outliers
# RA identifies anything above or below 2 SD and investigates
# look for with person patterns. TRy to identify which were genuine attempts and which were not and remove those that werent.
# This last point not as important for me as they completed the tasks in lab under supervision
# remove people who have no variance in confidence (e.g., enter 100 or 70 everytime)
# look at those who are fast and have low accuracy (e.g., not trying)
# if partipant has less than 80% of responses left after cleaning drop them.
# search for indications of shallow processing and remove 


library(corrr)
library(tidyverse)
library(lme4)
library(lmerTest)
library(grid)
library(lattice)
library(plotrix)

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
d_mdmt_uid <- read_csv("data/control/mdmt_ind.csv")

# remove RTs less than 1 second
# doesn't seem possible to make a genuine attempt in such little time
d_mdmt_uid[d_mdmt_uid$Ind_Stimulus.RT < 1000, "Ind_Stimulus.RT"] <- NA

x <- d_mdmt_uid %>%
  group_by(uid) %>%
  mutate(cen_conf = scale(Ind_Confidence.RESP, center = TRUE, scale = FALSE),
         cen_conf2 = cen_conf^2,
         cen_conf3 = cen_conf^3,
         log.RT = log(Ind_Stimulus.RT))

# check there is variance in confidence resposes
# if no variance in confidence and confidence is not accurate remove
# Also check that Ps are making a genuine attempt and not pressing any key
# investigate mean RT < 5 seconds and P with large proportion of RTs less than 3 seconds
# the following Ps are to be removed:
# uid == 17102312_2_g1 confidence = 50 (0) for each response accuracy = 100
# uid == 17110110_1_g2 confidence = 100 (0) for each response accuracy = 56
# uid == 18061214_1_e2 confidence = 50 (0) for each response accuracy = 17
# also, RTs got progressively faster as they appeared to try less
# uid == 18082114_2_e2 confidence = 25 (0) for each response accuracy = 25
# fast RTs indicate shallow processing and mostly guessing, mean = 3.6 seconds
# uid == 18062912_1_e1 confidence = 27 (6.3) accuracy = 44
# fast RTs indicate shallow processing mean = 3.8 seconds
# uid == 18072412_1_e1 confidence = 30 (8.4) accuracy = 25
# fast RTs indicate shallow processing mean = 2.5 seconds
# uid = 18072412_1_e2 confidence = 26 (2) accuracy = 31
# first half of test looks genuine mean RT = 9 seconds but 
# 2nd half looks like guessing mean RT = 3.5 seconds
# uid = 17103012_2_g1 confidence = 44 (11) accuracy = 31
# fast RTs indicate shallow processing mean = 3.4 seconds
# uid = 18041110_1_d2 confidence = 45 (29) accuracy = 44
# 1st half was genuine attempt RT = +30 seconds each and acc was 70%
# 2nd half was guessing RT = 3 seconds each and acc was 22%
# uid = 18042013_1_d2 confidence = 50 (24) accuracy = 25
# guessed for the last 5 questions, RTs <= 3 seconds
# and confidence ratings reflect this change
# uid = 18080913_2_g1 confidence = 52 (12) accuracy = 44
# Q1-3 effortful, indicated by RTs ~20 seconds and acc ~70%
# then 12 of the remaining Qs had RTs ~3 seconds acc ~40%
# uid = 18081613_2_e1 confidence 64 (23) accuracy 19
# fast RTs indicate shallow processing mean = 4.5 seconds
# uid = 18081613_2_e2 confidence = 61 (15) accuracy = 25
# Q1-7 effortful, indicated by RTs ~16 seconds
# then the remaining Qs had RTs ~2 seconds
# uid = 18082008_1_e1 confidence 45 (24) accuracy 25
# looks effortful until the last 4 questions RT < 2 seconds
# uid - 18082111_1_e2 stopped trying halfway through. For Q1-8
# mean rt = 20 seconds. For Q9-16 mean rt = 2 seconds with 4 under 1 second


# identified a number of additional Ps that may not have made a genuine attempt/shallow processing
# remove? Check with SK
# uid == 18081416_2_e1 confidence = 50 (9.1) and accuracy = 25
# fast RTs indicate shallow processing and mostly guessing, mean = 5.2 seconds
# uid == 17100612_1_d2 confidence = 25 (2.56) for most accuracy = 37.5
# fast RTs indicate shallow processing and mostly guessing, mean = 4.8 seconds
# uid == 17103012_2_d1 confidence = 39 (8.6) accuracy = 25
# fast RTs indicate shallow processing and mostly guessing, mean = 5.3 seconds
# uid == 17110310_1_g2 confidence = 28 (3) accuracy = 19
# fast RTs indicate shallow processing and mostly guessing, mean = 5 seconds
# uid = 18081416_2_e1 confidence = 50 (9.1) accuracy = 25
# fast RTs indicate shallow processing mean = 5.2 seconds
# also responses look liks sequences of 1,2,3
# uid = 17100609_1_d2 confidence = 25 (0) accuracy = 18.8
# fast RTs indicate shallow processing mean = 5.4 seconds
# uid = 17100609_1_g2 confidence = 25 (0) accuracy = 13.3
# fast RTs indicate shallow processing mean = 5.8 seconds
# uid = 17100612_2_d1 confidence = 42 (13.5) accuracy = 40
# fast RTs indicate shallow processing mean = 4.5 seconds
# uid = 17102710_1_g2 confidence = 25 (0) accuracy = 25
# fast RTs indicate shallow processing mean = 6 seconds
# looks like a pattern in responses 3,1,3,2,3,1,2,3...

# uid = 17110310_1_d2 confidence = 48 (20) accuracy = 25
# fast RTs indicate shallow processing mean = 5.5 seconds

# uid = 18041313_2_d1 confidence = 12.5 (14) accuracy = 45
# fast RTs indicate shallow processing mean = 5.7 seconds



# check <- x %>%
#   group_by(uid) %>%
#   mutate(conf_sd = sd(Ind_Confidence.RESP, na.rm = T)) %>%
#   filter(conf_sd > 0 & conf_sd < 10) %>%
#   group_by(uid) %>%
#   mutate(
#     conf = mean(Ind_Confidence.RESP),
#     acc = mean(Ind_Stimulus.ACC) * 100,
#     diff = abs(conf - acc),
#     mean_rt = mean(Ind_Stimulus.RT)) %>%
#   select(uid, Ind_Stimulus.ACC, Ind_Stimulus.RESP, Ind_Confidence.RESP, conf_sd, acc, conf, diff, Ind_Stimulus.RT, mean_rt) %>%
#   arrange(uid)
# 
# 

x %>% filter(uid == "18082210_2_e1" & Ind_Stimulus.RT < 12000) %>%
  select(Ind_Stimulus.ACC, Ind_Stimulus.RESP, Ind_Confidence.RESP, Ind_Stimulus.RT) %>%
  mutate(
    conf = mean(Ind_Confidence.RESP),
    acc = mean(Ind_Stimulus.ACC) * 100,
    diff = abs(conf - acc),
    sd = sd(Ind_Confidence.RESP),
    rt = mean(Ind_Stimulus.RT))

check <- x %>%
  select(uid, Ind_Stimulus.ACC, Ind_Confidence.RESP, Ind_Stimulus.RT) %>%
  group_by(uid) %>%
  mutate(conf = mean(Ind_Confidence.RESP),
    acc = mean(Ind_Stimulus.ACC) * 100,
    diff = abs(conf - acc),
    sd = sd(Ind_Confidence.RESP),
    rt = mean(Ind_Stimulus.RT)) %>%
  filter(Ind_Stimulus.RT < 7000) %>%
  group_by(uid) %>%
  mutate(n = n()) %>%
  filter(n > 8) %>%
  arrange(uid)


x <- x %>% filter(uid != "17102312_2_g1", uid != "17110110_1_g2", 
                  uid != "18061214_1_e2", uid != "18082114_2_e2",
                  uid != "18062912_1_e1", uid != "18072412_1_e1",
                  uid != "18072412_1_e2", uid != "17103012_2_g1",
                  uid != "18041110_1_d2", uid != "18042013_1_d2",
                  uid != "18080913_2_g1", uid != "18081613_2_e1",
                  uid != "18081613_2_e2", uid != "18082008_1_e1",
                  uid != "18082111_1_e2")
                  

# Check for within-person RT outliers
# identify those that are over 3 SDs 
# if response is correct then it appears the process is effortful
# if they are wrong its hard to know if they are drinking water, zoning out,
# or involved in effortful processing
# 23 cases identified, each P only has one each
# remove these cases?
# x <- x %>%
#   group_by(uid) %>%
#   mutate(rt_mean = mean(Ind_Stimulus.RT),
#   rt_sd = sd(Ind_Stimulus.RT),
#   dist_sd = abs((Ind_Stimulus.RT - rt_mean)/rt_sd)) 
# 
# table(x %>%
#   filter(dist_sd >= 3) %>%
#   select(uid))
# 
#   filter(!(dist > 3 & Ind_Stimulus.ACC == FALSE))


# remove rows with missing values
x <- x[!is.na(x$cen_conf), ]
x <- x[!is.na(x$log.RT), ]


# remove Ps with more than 20% NA responses
# none at present
x %>%
  group_by(uid) %>%
  summarise(
    p_na = 1 - (n()/16)) %>%
  filter(p_na > .2)

# quantile(x$cen_conf, probs = c(.33, .67))
# quantile(x$log.RT, probs = c(.33, .67))

x <- x %>%
  mutate(conf_level = ifelse(cen_conf <= -3.1250, "lo", ifelse(cen_conf >= 4.6875 , "hi", "mid")),
         Ind_Decision.RESP = tolower(Ind_Decision.RESP),
         speed = ifelse(log.RT >= 9.592002, "slow", ifelse(log.RT <= 8.940989, "fast", "mid")),
         conf_level1 = conf_level,
         speed1 = speed) %>%
  unite(confXspeed, conf_level1, speed1, sep = "_")

# 200+ responses in each confidence category except 40-47.5 = 30, 70-77.5 = 104, 85-92.5 = 148
# table(x$Ind_Confidence.RESP > 40 & x$Ind_Confidence.RESP <= 47.5)
x <- x %>% mutate(conf_cat = ifelse(Ind_Confidence.RESP <= 32.5, 1,
                             ifelse(Ind_Confidence.RESP > 32.5 & Ind_Confidence.RESP <= 40, 2,
                             ifelse(Ind_Confidence.RESP > 40 & Ind_Confidence.RESP <= 47.5, 3,
                             ifelse(Ind_Confidence.RESP > 47.5 & Ind_Confidence.RESP <= 55, 4,
                             ifelse(Ind_Confidence.RESP > 55 & Ind_Confidence.RESP <= 62.5, 5,
                             ifelse(Ind_Confidence.RESP > 62.5 & Ind_Confidence.RESP <= 70, 6,
                             ifelse(Ind_Confidence.RESP > 70 & Ind_Confidence.RESP <= 77.5, 7,
                             ifelse(Ind_Confidence.RESP > 77.5 & Ind_Confidence.RESP <= 85, 8,
                             ifelse(Ind_Confidence.RESP > 85 & Ind_Confidence.RESP <= 92.5, 9, 10))))))))))

x %>% filter(cen_conf <= -57.03125)

x <- x %>% mutate(cen_conf_cat = ifelse(cen_conf <= -57.03125, 1,
                                  ifelse(cen_conf > -57.03125 & cen_conf <= -43.75, 2,
                                  ifelse(cen_conf > -43.75 & cen_conf <= -30.46875, 3,
                                  ifelse(cen_conf > -30.46875 & cen_conf <= -17.1875, 4,
                                  ifelse(cen_conf > -17.1875 & cen_conf <= -3.90625, 5,
                                  ifelse(cen_conf > -3.90625 & cen_conf <= 9.375, 6,
                                  ifelse(cen_conf > 9.375 & cen_conf <= 22.65625, 7,
                                  ifelse(cen_conf > 22.65625 & cen_conf <= 35.9375, 8,
                                  ifelse(cen_conf > 35.9375 & cen_conf <= 49.21875, 9, 10))))))))))


x %>%
  group_by(cen_conf_cat) %>%
  summarise(m_acc = mean(Ind_Stimulus.ACC),
            m_conf = mean(cen_conf),
            m_rt = mean(log.RT)) %>%
  ggplot(aes(m_rt, m_conf)) + geom_line()


x %>%
  group_by(conf_cat) %>%
  summarise(m_acc = mean(Ind_Stimulus.ACC),
            m_conf = mean(Ind_Confidence.RESP),
            m_rt = mean(Ind_Stimulus.RT)) %>%
ggplot(aes(m_rt, m_conf)) + geom_line()

x %>%
  ggplot(aes(Ind_Stimulus.RT, Ind_Confidence.RESP)) + geom_smooth()

x %>%
  ggplot(aes(log.RT, cen_conf)) + geom_smooth()

# remove Ps with no betting variance (i.e.,bet on nothing or everything)
# identified uid = 18062815_1_e2 bet on everything and had 25% accuracy
# identified uid = 18041213_2_g1 bet on nothing and had 56% accuracy
x %>% 
  group_by(uid) %>%
  mutate(acc = mean(Ind_Stimulus.ACC == TRUE)*100,
    p_bet = mean(Ind_Decision.RESP == "a")) %>%
  filter(p_bet == 0) %>%
  select(uid, acc, p_bet, Ind_Confidence.RESP)

x %>% filter(uid == "18062815_1_e2") %>% summarise(acc = mean(Ind_Stimulus.ACC == TRUE)*100,
                                                   conf = mean(Ind_Confidence.RESP),
                                                p_bet = mean(Ind_Decision.RESP == "a"))

bet %>%
  group_by(conf_cat) %>%
  summarise(m_acc = mean(Ind_Stimulus.ACC),
            m_conf = mean(Ind_Confidence.RESP),
            m_rt = mean(Ind_Stimulus.RT)) %>%
  ggplot(aes(m_rt, m_conf)) + geom_smooth()

bet %>%
  ggplot(aes(Ind_Confidence.RESP, Ind_Stimulus.RT)) + geom_smooth()

table(bet %>% filter(Ind_Decision.RESP == "b" & Ind_Confidence.RESP == 100) %>% select(uid))

bet %>% filter(uid == "18081410_1_d2") %>% select(p_bet)

bet %>% filter(Ind_Decision.RESP == "b") %>%
  group_by(conf_cat) %>%
  summarise(m_acc = mean(Ind_Stimulus.ACC),
            m_conf = mean(Ind_Confidence.RESP),
            m_rt = mean(Ind_Stimulus.RT),
            n = n()) %>%
  ggplot(aes(m_conf, m_rt)) + geom_smooth()

bet %>% filter(Ind_Decision.RESP == "b") %>% ggplot(aes(Ind_Confidence.RESP, Ind_Stimulus.RT)) + geom_smooth()





#Regression
#model0: Random intercepts for subjects and items 
model0 <- lmer(log.RT ~ 1 + cen_conf + cen_conf2 + cen_conf3 + (1 | uid) + (1 | MdmtItemNum), REML = F, data = x)

#model1: Random intercepts for subjects and items PLUS variance in slope and curvilinearity across subjects
model1 <- lmer(log.RT ~ 1 + cen_conf + cen_conf2 + cen_conf3 + (1 | uid) + 
                 (1 | cen_conf) + (1 | cen_conf2) + (1 | MdmtItemNum), REML = F, data = x)

#tests whether model1 fits better than model0 => Chi-square test
anova(model0,model1)

#prints out results from model0
summary(model0)
coef(model0)$uid
ranef(model0)$uid

summary(model1)
coef(model1)$uid
ranef(model1)$uid










# betweeen correaltions for everything
x[, sapply(x, is.numeric)] %>% 
  correlate() %>% fashion()


# Standarised confidence and log time -------------------------------------
# pearson overall
x %>% group_by(uid) %>%
  summarise(r = cor(cen_conf, log.RT)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma overall
x %>% group_by(uid) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level
x %>% group_by(uid, conf_level) %>%
  summarise(r = cor(cen_conf, log.RT)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level
x %>% group_by(uid, conf_level) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by speed
x %>% group_by(uid, speed) %>%
  summarise(r = cor(cen_conf, log.RT)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by speed
x %>% group_by(uid, speed) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level and speed
x %>% group_by(uid, confXspeed) %>%
  summarise(r = cor(cen_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level and speed
x %>% group_by(uid, confXspeed) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by bet decision
x %>% group_by(uid, Ind_Decision.RESP) %>%
  summarise(r = cor(cen_conf, log.RT)) %>%
  group_by(Ind_Decision.RESP) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by bet decision
x %>% group_by(uid, Ind_Decision.RESP) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
  group_by(Ind_Decision.RESP) %>%
  summarise(r = mean(r, na.rm = TRUE))


# Standardised confidence and Accuracy --------------------------------
# pearson overall 
x %>% group_by(uid) %>%
  summarise(r = cor(cen_conf, Ind_Stimulus.ACC)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma overall
x %>% group_by(uid) %>%
  summarise(r = goodman(cen_conf, Ind_Stimulus.ACC)) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level
x %>% group_by(uid, conf_level) %>%
  summarise(r = cor(cen_conf, Ind_Stimulus.ACC)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level
x %>% group_by(uid, conf_level) %>%
  summarise(r = goodman(cen_conf, Ind_Stimulus.ACC)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by speed
x %>% group_by(uid, speed) %>%
  summarise(r = cor(cen_conf, Ind_Stimulus.ACC)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by speed
x %>% group_by(uid, speed) %>%
  summarise(r = goodman(cen_conf, Ind_Stimulus.ACC)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level and speed
x %>% group_by(uid, confXspeed) %>%
  summarise(r = cor(cen_conf, Ind_Stimulus.ACC)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level and speed
x %>% group_by(uid, confXspeed) %>%
  summarise(r = goodman(cen_conf, Ind_Stimulus.ACC)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))



# Standardised confidence and bet decision --------------------------------
x <- x %>% mutate(Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "a", 1, 0))

# pearson overall 
x %>% group_by(uid) %>%
  summarise(r = cor(cen_conf, Ind_Decision.RESP)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma overall
x %>% group_by(uid) %>%
  summarise(r = goodman(cen_conf, Ind_Decision.RESP)) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level
x %>% group_by(uid, conf_level) %>%
  summarise(r = cor(cen_conf, Ind_Decision.RESP)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level
x %>% group_by(uid, conf_level) %>%
  summarise(r = goodman(cen_conf, Ind_Decision.RESP)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by speed
x %>% group_by(uid, speed) %>%
  summarise(r = cor(cen_conf, Ind_Decision.RESP)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by speed
x %>% group_by(uid, speed) %>%
  summarise(r = goodman(cen_conf, Ind_Decision.RESP)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level and speed
x %>% group_by(uid, confXspeed) %>%
  summarise(r = cor(cen_conf, Ind_Decision.RESP)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level and speed
x %>% group_by(uid, confXspeed) %>%
  summarise(r = goodman(cen_conf, Ind_Decision.RESP)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))























# TEAMS -------------------------------------------------------------
d_mdmt_grp <- read_csv("data/control/mdmt_team.csv")


# No RTs less than 1 second
# there are 4 < 3000 but 2 of them are correct so appears to be adequate time
# d_mdmt_grp[d_mdmt_grp$Grp_Stimulus.RT < 1000, "Grp_Stimulus.RT"] <- NA

# calculate variables for the regression
# compute the log of RT time, center confidence 
# and created quadratic and cubic terms
x <- d_mdmt_grp %>%
  group_by(group) %>%
  mutate(cen_conf = scale(Grp_Confidence.RESP, center = TRUE, scale = FALSE),
         cen_conf2 = cen_conf^2,
         cen_conf3 = cen_conf^3,
         log.RT = log(Grp_Stimulus.RT))

# check there is variance in confidence resposes
# if no variance in confidence and confidence is not accurate remove
# group == 18080915_1 confidence = 100 (0) accuracy = 67
# group == 18081718_1 confidence = 100 (0) accuracy = 75
# group == 17110110_2 confidence = 90 (0) accuracy = 69

# Also check that Ps are making a genuine attempt
# investigate mean RT < 5 seconds and P with large proportion of RTs less than 5 seconds
# group == 17103112_2 confidence = 94 accuracy = 44
# RTs were less than 5 seconds for half the questions, only got 2 of 8 correct

# check <- x %>%
#   group_by(group) %>%
#   mutate(conf_sd = sd(Grp_Confidence.RESP, na.rm = T)) %>%
#   filter(conf_sd == 0) %>%
#   group_by(group) %>%
#   mutate(
#     conf = mean(Grp_Confidence.RESP, na.rm = TRUE),
#     acc = mean(Grp_Stimulus.ACC, na.rm = TRUE) * 100,
#     diff = abs(conf - acc),
#     mean_rt = mean(Grp_Stimulus.RT, na.rm = TRUE)) %>%
#   select(group, Grp_Stimulus.ACC, Grp_Stimulus.RESP, Grp_Confidence.RESP, 
#          conf_sd, acc, conf, diff, Grp_Stimulus.RT, mean_rt) %>%
#   arrange(group)

x <- x %>% filter(group != "18080915_1", group != "18081718_1", 
                  group != "17110110_2")


# Check for within-person RT outliers
# identify those that are over 3 SDs 
# if response is correct then it appears the process is effortful
# if they are wrong its hard to know if they are drinking water, zoning out,
# or involved in effortful processing
# 26 cases identified, each P only has one each and the majority are correct
# DECISION: retain these responses
# check <- x %>%
#   group_by(group) %>%
#   mutate(rt_mean = mean(Grp_Stimulus.RT),
#   rt_sd = sd(Grp_Stimulus.RT),
#   dist_sd = abs((Grp_Stimulus.RT - rt_mean)/rt_sd)) %>%
#   filter(dist_sd >=3) %>%
#   select(group, Grp_Stimulus.ACC, dist_sd)

# remove rows with missing values
x <- x[!is.na(x$cen_conf), ]
x <- x[!is.na(x$log.RT), ]


# remove Ps with more than 20% NA responses
# none at present
x %>%
  group_by(group) %>%
  summarise(
    p_na = 1 - (n()/16)) %>%
  filter(p_na > .2)

# quantile(x$cen_conf, probs = c(.33, .67))
# quantile(x$log.RT, probs = c(.33, .67))

x <- x %>%
  mutate(conf_level = ifelse(cen_conf <= 0, "lo", ifelse(cen_conf >= 1.875 , "hi", "mid")),
         Grp_Decision.RESP = tolower(Grp_Decision.RESP),
         speed = ifelse(log.RT >= 9.654088, "slow", ifelse(log.RT <= 9.112284, "fast", "mid")),
         conf_level1 = conf_level,
         speed1 = speed) %>%
  unite(confXspeed, conf_level1, speed1, sep = "_")

# groups were highly confident so most categories have smaller numbers 
# <= 32.5 = 27, 32.5-40 = 21, 40-47.5 = 13, 47.5-55 = 46, 
# 55-62.5 = 47, 62.5-70 = 76, 70-77.5 = 95
table(x$Grp_Confidence.RESP > 92.5 & x$Grp_Confidence.RESP <= 100)
x <- x %>% mutate(conf_cat = ifelse(Grp_Confidence.RESP <= 32.5, 1,
                             ifelse(Grp_Confidence.RESP > 32.5 & Grp_Confidence.RESP <= 40, 2,
                             ifelse(Grp_Confidence.RESP > 40 & Grp_Confidence.RESP <= 47.5, 3,
                             ifelse(Grp_Confidence.RESP > 47.5 & Grp_Confidence.RESP <= 55, 4,
                             ifelse(Grp_Confidence.RESP > 55 & Grp_Confidence.RESP <= 62.5, 5,
                             ifelse(Grp_Confidence.RESP > 62.5 & Grp_Confidence.RESP <= 70, 6,
                             ifelse(Grp_Confidence.RESP > 70 & Grp_Confidence.RESP <= 77.5, 7,
                             ifelse(Grp_Confidence.RESP > 77.5 & Grp_Confidence.RESP <= 85, 8,
                             ifelse(Grp_Confidence.RESP > 85 & Grp_Confidence.RESP <= 92.5, 9, 10))))))))))


# small n for the following centered confidence categories
# cat 1 = 1, cat 2 = 4, cat 3 = 4, cat 4 = 22, cat 5 = 44, cat 9 = 26, cat 10 = 8
x <- x %>% mutate(cen_conf_cat = ifelse(cen_conf <= -59.21458, 1,
                                 ifelse(cen_conf > -59.21458 & cen_conf <= -48.11666, 2,
                                 ifelse(cen_conf > -48.11666 & cen_conf <= -37.01874, 3,
                                 ifelse(cen_conf > -37.01874 & cen_conf <= -25.92082, 4,
                                 ifelse(cen_conf > -25.92082 & cen_conf <= -14.8229, 5,
                                 ifelse(cen_conf > -14.8229 & cen_conf <= -3.72498, 6,
                                 ifelse(cen_conf > -3.72498 & cen_conf <= 7.37294, 7,
                                 ifelse(cen_conf > 7.37294 & cen_conf <= 18.47086, 8,
                                 ifelse(cen_conf > 18.47086 & cen_conf <= 29.56878, 9, 10))))))))))


x %>%
  group_by(cen_conf_cat) %>%
  summarise(m_acc = mean(Grp_Stimulus.ACC),
            m_conf = mean(cen_conf),
            m_rt = mean(log.RT)) %>%
  ggplot(aes(m_rt, m_conf)) + geom_smooth()


x %>%
  group_by(conf_cat) %>%
  summarise(m_acc = mean(Grp_Stimulus.ACC),
            m_conf = mean(Grp_Confidence.RESP),
            m_rt = mean(Grp_Stimulus.RT)) %>%
  ggplot(aes(m_rt, m_conf)) + geom_smooth()

x %>%
  ggplot(aes(Grp_Stimulus.RT, Grp_Confidence.RESP)) + geom_smooth()

x %>%
  ggplot(aes(log.RT, cen_conf)) + geom_smooth()



#Regression
#model0: Random intercepts for subjects and items 
model0 <- lmer(log.RT ~ 1 + cen_conf + cen_conf2 + cen_conf3 + (1 | group) + (1 | MdmtItemNum), REML = F, data = x)

#model1: Random intercepts for subjects and items PLUS variance in slope and curvilinearity across subjects
model1 <- lmer(log.RT ~ 1 + cen_conf + cen_conf2 + cen_conf3 + (1 | group) + 
                 (1 | cen_conf) + (1 | cen_conf2) + (1 | MdmtItemNum), REML = F, data = x)

#tests whether model1 fits better than model0 => Chi-square test
anova(model0,model1)

#prints out results from model0
summary(model0)
coef(model0)$group
ranef(model0)$group

summary(model1)
coef(model1)$group
ranef(model1)$group



# Standarised confidence and log time -------------------------------------
# pearson overall
x %>% group_by(group) %>%
  summarise(r = cor(cen_conf, log.RT)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma overall
x %>% group_by(group) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level
x %>% group_by(group, conf_level) %>%
  summarise(r = cor(cen_conf, log.RT)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level
x %>% group_by(group, conf_level) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by speed
x %>% group_by(group, speed) %>%
  summarise(r = cor(cen_conf, log.RT)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by speed
x %>% group_by(group, speed) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = cor(cen_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by bet decision
x %>% group_by(group, Grp_Decision.RESP) %>%
  summarise(r = cor(cen_conf, log.RT)) %>%
  group_by(Grp_Decision.RESP) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by bet decision
x %>% group_by(group, Grp_Decision.RESP) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
  group_by(Grp_Decision.RESP) %>%
  summarise(r = mean(r, na.rm = TRUE))


# Standardised confidence and Accuracy --------------------------------
# pearson overall 
x %>% group_by(group) %>%
  summarise(r = cor(cen_conf, Grp_Stimulus.ACC)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma overall
x %>% group_by(group) %>%
  summarise(r = goodman(cen_conf, Grp_Stimulus.ACC)) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level
x %>% group_by(group, conf_level) %>%
  summarise(r = cor(cen_conf, Grp_Stimulus.ACC)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level
x %>% group_by(group, conf_level) %>%
  summarise(r = goodman(cen_conf, Grp_Stimulus.ACC)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by speed
x %>% group_by(group, speed) %>%
  summarise(r = cor(cen_conf, Grp_Stimulus.ACC)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by speed
x %>% group_by(group, speed) %>%
  summarise(r = goodman(cen_conf, Grp_Stimulus.ACC)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = cor(cen_conf, Grp_Stimulus.ACC)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = goodman(cen_conf, Grp_Stimulus.ACC)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))



# Standardised confidence and bet decision --------------------------------
x <- x %>% mutate(Grp_Decision.RESP = ifelse(Grp_Decision.RESP == "a", 1, 0))

# pearson overall 
x %>% group_by(group) %>%
  summarise(r = cor(cen_conf, Grp_Decision.RESP)) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma overall
x %>% group_by(group) %>%
  summarise(r = goodman(cen_conf, Grp_Decision.RESP)) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level
x %>% group_by(group, conf_level) %>%
  summarise(r = cor(cen_conf, Grp_Decision.RESP)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level
x %>% group_by(group, conf_level) %>%
  summarise(r = goodman(cen_conf, Grp_Decision.RESP)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by speed
x %>% group_by(group, speed) %>%
  summarise(r = cor(cen_conf, Grp_Decision.RESP)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by speed
x %>% group_by(group, speed) %>%
  summarise(r = goodman(cen_conf, Grp_Decision.RESP)) %>%
  group_by(speed) %>%
  summarise(r = mean(r, na.rm = TRUE))


# pearson by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = cor(cen_conf, Grp_Decision.RESP)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

# gamma by confidence level and speed
x %>% group_by(group, confXspeed) %>%
  summarise(r = goodman(cen_conf, Grp_Decision.RESP)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))



x[, sapply(x, is.numeric)] %>% 
  correlate() %>% fashion()

























# between pearson correlations by confidence level
hi <- x %>% filter(conf_level == "hi")
lo <- x %>% filter(conf_level == "lo")

hi[, sapply(hi, is.numeric)] %>% 
  correlate() %>% fashion()

lo[, sapply(lo, is.numeric)] %>% 
  correlate() %>% fashion()


x <- x[!is.na(x$Grp_Decision.RESP), ]


x %>% group_by(group, Grp_Decision.RESP) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
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
  summarise(r = goodman(cen_conf, log.RT)) %>%
  group_by(confXspeed) %>%
  summarise(r = mean(r, na.rm = TRUE))

no <- x %>% filter(Grp_Decision.RESP == "n") %>%
  select(-Grp_Decision.RESP)

no %>% group_by(group, confXspeed) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
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
#   filter(cen_conf >= 0)
# 
# lo <- x %>%
#   filter(cen_conf < 0)

x %>% group_by(group, conf_level) %>%
  summarise(r = goodman(Grp_Confidence.RESP, Grp_Stimulus.RT)) %>%
  group_by(conf_level) %>%
  summarise(r = mean(r, na.rm = TRUE))


# between pearson correlations by confidence level
hi <- x %>% filter(conf_level == "hi")
lo <- x %>% filter(conf_level == "lo")

hi[, sapply(hi, is.numeric)] %>% 
  correlate() %>% fashion()

lo[, sapply(lo, is.numeric)] %>% 
  correlate() %>% fashion()


x <- x[!is.na(x$Grp_Decision.RESP), ]


x %>% group_by(group, Grp_Decision.RESP) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
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
  unite(confXspeed, conf_level, speed, sep = "_")

x %>% group_by(group, confXspeed) %>%
  summarise(r = goodman(cen_conf, log.RT)) %>%
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