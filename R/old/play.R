
surveys_qrt$RAPM_individual_A1$ApmItemNum

names(surveys_qrt)

# To do
# Check issues with CRT
# Check accuracy for ADR
# Calculate team RT for each task (and check times are legit)
# Read in data for additional 8 participants that completed multi-JSON protocol







mean(x$Grp_Stimulus.RT, na.rm = TRUE)

test <- x %>% group_by(uid) %>%
  mutate(Grp_Stimulus.OnsetTime = Grp_Stimulus.OnsetTime/1000,
         Grp_Stimulus.OffsetTime = Grp_Stimulus.OffsetTime/1000,
         Grp_Stimulus.OnsetTime = as.POSIXct(Grp_Stimulus.OnsetTime, tz = "Etc/GMT-10", origin="1970-01-01"),
         Grp_Stimulus.OffsetTime = as.POSIXct(Grp_Stimulus.OffsetTime, tz = "Etc/GMT-10", origin="1970-01-01")) %>%
  filter(group == "18041210_2") %>%
  select(group, uid, member, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime, Grp_Stimulus.RT)

 x %>% filter(group == "18041210_2") %>% select(member, Grp_Stimulus.RESP, Grp_Stimulus.OnsetTime, Grp_Stimulus.OffsetTime)
 

# Risk_Aversion_Scale -----------------------------------------------------

