# all g!
a1 <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI_data/qrt/Cognitive_Reflection_Test_A1.csv")

b1 <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI_data/qrt/Cognitive_Reflection_Test_B1.csv")

a2 <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI_data/java/Cognitive_Reflection_Test_A2.csv")

b2 <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI_data/java/Cognitive_Reflection_Test_B2.csv")

names(a1)



library(jsonlite)
library(purrr)
library(dplyr)


# A1 - ALL G! -------------------------------------------------------------
a <- a1 %>% 
  select(contains("ExitTest_1_TEXT")) %>% 
  map(parse_col) %>%
  map(as_data_frame) %>%
  bind_rows()
  
# Fix [] in names
names(a) <- sub("\\[", ".", names(a))
names(a) <- sub("\\]", "", names(a))

# Repeat all other columns
# ExitTest_[0-9]_TEXT.[0-9].
# [0-9]_stimulus_[0-9]
b <- a1 %>% select(-contains("ExitTest_1_TEXT"))
b <- do.call(rbind, replicate(nrow(a) / nrow(a1), b, simplify = FALSE))

# Return binded columns
a1 <- cbind(b, a)

a1$uid

a1 %>% filter(str_detect(uid, "17102710_2")) %>% 
  select(uid, CRTItemNum, Ind_Stimulus.RESP, Grp_Stimulus.RESP) %>% 
  gather(var, val, -uid, -CRTItemNum) %>% 
  mutate(var = factor(var, levels = c("Ind_Stimulus.RESP", "Grp_Stimulus.RESP"))) %>% 
  spread(uid, val)



# B1 ALL G! ---------------------------------------------------------------
a <- b1 %>% 
  select(contains("ExitTest_1_TEXT")) %>% 
  map(parse_col) %>%
  map(as_data_frame) %>%
  bind_rows()

# Fix [] in names
names(a) <- sub("\\[", ".", names(a))
names(a) <- sub("\\]", "", names(a))

# Repeat all other columns
# ExitTest_[0-9]_TEXT.[0-9].
# [0-9]_stimulus_[0-9]
b <- b1 %>% select(-contains("ExitTest_1_TEXT"))
b <- do.call(rbind, replicate(nrow(a) / nrow(b1), b, simplify = FALSE))

# Return binded columns
b1 <- cbind(b, a)

b1$uid

b1 %>% filter(str_detect(uid, "18050509_1")) %>% 
  select(uid, CRTItemNum, Ind_Stimulus.RESP, Grp_Stimulus.RESP) %>% 
  gather(var, val, -uid, -CRTItemNum) %>% 
  mutate(var = factor(var, levels = c("Ind_Stimulus.RESP", "Grp_Stimulus.RESP"))) %>% 
  spread(uid, val)  

b1 %>% filter(str_detect(uid, "18050509_1")) %>% 
  select(CRTItemNum, Grp_Stimulus.RESP) %>% 
  mutate(Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP)) %>% 
  group_by(CRTItemNum) %>% 
  summarise(same = Grp_Stimulus.RESP[1] == Grp_Stimulus.RESP[2]) %>% 
  ungroup() %>% 
  summarise(n = sum(same))





?matches()
select(matches("[0-9]_stimulus|[0-9]_grp_decision|[0-9]_item|[0-9]_decision")) 

select(contains("grp_decision")) %>% 
names(a2)



# A2 ---------------------------------------------------------------
a <- a2 %>% 
  select(contains("grp_decision")) %>% 
  map(parse_col) %>%
  map(as_data_frame) %>%
  bind_rows()

# Fix [] in names
names(a) <- sub("\\[", ".", names(a))
names(a) <- sub("\\]", "", names(a))

# Repeat all other columns
# ExitTest_[0-9]_TEXT.[0-9].
# [0-9]_stimulus_[0-9]
b <- a2 %>% select(-contains("ExitTest_1_TEXT"))
b <- do.call(rbind, replicate(nrow(a) / nrow(a2), b, simplify = FALSE))

# Return binded columns
a2 <- cbind(b, a)

names(a2)

a2$uid

a2 %>% filter(str_detect(uid, "18080915_1")) %>% 
  select(uid, itemnum, ind_response, grp_response) %>% 
  gather(var, val, -uid, -itemnum) %>% 
  mutate(var = factor(var, levels = c("ind_response", "grp_response"))) %>% 
  spread(uid, val)  

a2 %>% #filter(str_detect(uid, "18080915_1")) %>% 
  select(uid, itemnum, grp_response) %>% 
  mutate(grp_response = as.numeric(grp_response)) %>% 
  filter(!is.na(grp_response)) %>% 
  separate(uid, c("date", "team", "id")) %>% 
  unite(group, c("date", "team")) %>% 
  group_by(group, itemnum) %>% 
  summarise(same = grp_response[1] == grp_response[2]) %>% 
  group_by(group) %>% 
  summarise(n = sum(same, na.rm = TRUE)) %>% 
  arrange(n)




# B2 ---------------------------------------------------------------
col <- c("[0-9]_stimulus", "_grp_decision", "_item", "_decision")

b2 %>% 
  select(matches("[0-9]_stimulus|[0-9]_grp_decision|[0-9]_item|[0-9]_decision"))
  map(parse_col) %>%
  map(as_data_frame) %>%
  bind_rows()

# Fix [] in names
names(a) <- sub("\\[", ".", names(a))
names(a) <- sub("\\]", "", names(a))

# Repeat all other columns
# ExitTest_[0-9]_TEXT.[0-9].
# [0-9]_stimulus_[0-9]
b <- b2 %>% select(-contains("ExitTest_1_TEXT"))
b <- do.call(rbind, replicate(nrow(a) / nrow(b2), b, simplify = FALSE))

# Return binded columns
b2 <- cbind(b, a)


b2 %>% #filter(str_detect(uid, "18080915_1")) %>% 
  select(uid, itemnum, grp_response) %>% 
  mutate(grp_response = as.numeric(grp_response)) %>% 
  filter(!is.na(grp_response)) %>% 
  separate(uid, c("date", "team", "id")) %>% 
  unite(group, c("date", "team")) %>% 
  group_by(group, itemnum) %>% 
  summarise(same = grp_response[1] == grp_response[2]) %>% 
  group_by(group) %>% 
  summarise(n = sum(same, na.rm = TRUE)) %>% 
  arrange(n)











table(surveys_qrt$`/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI_data/qrt/Cognitive_Reflection_Test_A1` %>%
  select(uid, CRTItemNum, Grp_Stimulus.RESP) %>% 
  mutate(Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP)) %>% 
  filter(!is.na(Grp_Stimulus.RESP)) %>% 
  separate(uid, c("date", "team", "id")) %>% 
  unite(group, c("date", "team")) %>% 
  group_by(group, CRTItemNum) %>% 
  summarise(same = Grp_Stimulus.RESP[1] == Grp_Stimulus.RESP[2]) %>% 
  group_by(group) %>% 
  summarise(n = sum(same, na.rm = TRUE)) %>% 
  arrange(n) %>% select(n))

table(surveys_qrt$`/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI_data/qrt/Cognitive_Reflection_Test_B1` %>%
        select(uid, CRTItemNum, Grp_Stimulus.RESP) %>% 
        mutate(Grp_Stimulus.RESP = as.numeric(Grp_Stimulus.RESP)) %>% 
        filter(!is.na(Grp_Stimulus.RESP)) %>% 
        separate(uid, c("date", "team", "id")) %>% 
        unite(group, c("date", "team")) %>% 
        group_by(group, CRTItemNum) %>% 
        summarise(same = Grp_Stimulus.RESP[1] == Grp_Stimulus.RESP[2]) %>% 
        group_by(group) %>% 
        summarise(n = sum(same, na.rm = TRUE)) %>% 
        arrange(n) %>% select(n))



table(surveys_java$`/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI_data/java/Cognitive_Reflection_Test_A2` %>%
        select(uid, itemnum, grp_response) %>% 
        mutate(grp_response = as.numeric(grp_response)) %>% 
        filter(!is.na(grp_response)) %>% 
        separate(uid, c("date", "team", "id")) %>% 
        unite(group, c("date", "team")) %>% 
        group_by(group, itemnum) %>% 
        summarise(same = grp_response[1] == grp_response[2]) %>% 
        group_by(group) %>% 
        summarise(n = sum(same, na.rm = TRUE)) %>% 
        arrange(n) %>% select(n))

table(surveys_java$`/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI_data/java/Cognitive_Reflection_Test_B2` %>%
  select(uid, itemnum, grp_response) %>% 
  mutate(grp_response = as.numeric(grp_response)) %>% 
  filter(!is.na(grp_response)) %>% 
  separate(uid, c("date", "team", "id")) %>% 
  unite(group, c("date", "team")) %>% 
  group_by(group, itemnum) %>% 
  summarise(same = grp_response[1] == grp_response[2]) %>% 
  group_by(group) %>% 
  summarise(n = sum(same, na.rm = TRUE)) %>% 
  arrange(n) %>% select(n))


