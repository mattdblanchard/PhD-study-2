source("R/survey_prep/r2f_neg.R")
source("R/survey_prep/r2f_pos.R")

# Need to create a new itemnum variable that is uniform across both frames
# some participants did not complete both frames of R2F
# need to remove these teams and calculate vars using those that compelted both Pos & Neg
# used the following code to identify these teams
p_uid <- unique(r2f_pos$uid)
n_uid <- unique(r2f_neg$uid)
unique(r2f_neg %>% filter(!uid %in% p_uid) %>% select(uid))
unique(r2f_pos %>% filter(!uid %in% n_uid) %>% select(uid))

r2f_neg <- r2f_neg %>% 
  group_by(uid) %>% 
  mutate(ItemNum = 1:n(),
         frame = "N") %>% 
  select(-R2FItemNum) %>% 
  filter(group != "18080610_1") %>% 
  filter(group != "17102710_2")

r2f_pos <- r2f_pos %>% 
  group_by(uid) %>% 
  mutate(ItemNum = 1:n(),
         frame = "P") %>% 
  select(-R2FItemNum) %>% 
  filter(group != "18081413_1") %>%
  filter(group != "18110716_1") %>%
  filter(group != "19013010_1") %>% 
  filter(group != "17102710_2")

r2f <- rbind(r2f_pos, r2f_neg)

r2f.uid <- r2f %>% 
  group_by(uid, ItemNum) %>% 
  summarise(ind.resp = abs(Ind_Stimulus.RESP[frame == "P"] - Ind_Stimulus.RESP[frame == "N"]),
            grp.resp = abs(Grp_Stimulus.RESP[frame == "P"] - Grp_Stimulus.RESP[frame == "N"])) %>% 
  group_by(uid) %>% 
  summarise(r2f.ind = mean(ind.resp, na.rm = TRUE),
            r2f.grp = mean(grp.resp, na.rm = TRUE))

# to check which uids differ between the two r2f frames
# p <- unique(r2f_pos$uid)
# n <- unique(r2f_neg$uid)
# 
# r2f_pos %>% filter(!uid %in% n) %>% select(uid)

# r2f_pos %>% filter(str_detect(uid, "18041213")) %>% select(uid, ResponseID, StartDate)
# r2f_neg %>% filter(str_detect(uid, "18041213")) %>% select(uid, ResponseID, StartDate)

# groups less influenced by framing
# t.test(r2f$ind_resp, r2f$grp_resp, paired = TRUE)

# Reiliability - currently not working
# # FOR INDIVIDUALS
# x <- r2f %>%
#   drop_na() %>% 
#   select(uid, ItemNum, Grp_Stimulus.RESP) %>%
#   # mutate(n = 1:n()) %>%
#   spread(ItemNum, Grp_Stimulus.RESP) %>%
#   select(-uid)
# #
# psych::alpha(x)$total$raw_alpha
