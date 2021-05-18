# calculate reliability for accuracy

# load cleaned data
source("R/import_data.R")

scripts <- list.files("R/survey_prep/", pattern = ".R")[c(-2, -5, -6, -9, -22)]

for (i in scripts) {
  source(paste0("R/survey_prep/", i))
}

# rename itemnum variables so they are consistent across tests
adr <- adr %>% rename(itemnum = ADRitemNum)
crt <- crt %>% rename(itemnum = CRTItemNum)
gk <- gk %>% rename(itemnum = Itemnum)
mdmt_ind <- mdmt_ind %>% rename(itemnum = MdmtItemNum)
mdmt_team <- mdmt_team %>% rename(itemnum = MdmtItemNum)
rapm_ind <- rapm_ind %>% rename(itemnum = ApmItemNum)
rapm_team <- rapm_team %>% rename(itemnum = ApmItemNum)


# individual reliabilities
ind <- list(adr, crt, gk, mdmt_ind, rapm_ind)
tests <- c("adr", "crt", "gk", "mdmt", "rapm")

# accuracy

map(1:5, function(i) {
  x <- ind[[i]] %>% 
    ungroup() %>% 
    select(uid, itemnum, Ind_Stimulus.ACC) %>% 
    mutate(Ind_Stimulus.ACC = ifelse(Ind_Stimulus.ACC == TRUE, 1,
                                     ifelse(Ind_Stimulus.ACC == FALSE, 0, NA))) %>% 
    spread(itemnum, Ind_Stimulus.ACC) %>% 
    select(-uid)
  
  print(paste0(tests[[i]], " - Reliability for individual accuracy = ", round(psych::alpha(x)$total$raw_alpha, 2)))
})

# confidence
map(1:5, function(i) {
  x <- ind[[i]] %>% 
    ungroup() %>% 
    select(uid, itemnum, Ind_Confidence.RESP) %>% 
    spread(itemnum, Ind_Confidence.RESP) %>% 
    select(-uid)
  
  print(paste0(tests[[i]], " - Reliability for individual confidence = ", round(psych::alpha(x)$total$raw_alpha, 2)))
})


# team reliability
team <- list(adr, crt, gk, mdmt_team, rapm_team)
tests <- c("adr", "crt", "gk", "mdmt", "rapm")

# accuracy
map(1:5, function(i) {
  x <- team[[i]] %>% 
    ungroup() %>% 
    select(group, itemnum, Grp_Stimulus.ACC) %>% 
    mutate(Grp_Stimulus.ACC = ifelse(Grp_Stimulus.ACC == TRUE, 1,
                                     ifelse(Grp_Stimulus.ACC == FALSE, 0, NA))) %>% 
    group_by(group, itemnum) %>% 
    summarise(Grp_Stimulus.ACC = Grp_Stimulus.ACC[1]) %>% 
    spread(itemnum, Grp_Stimulus.ACC) %>% 
    ungroup() %>% 
    select(-group)
  
  print(paste0(tests[[i]], " - Reliability for group accuracy = ", round(psych::alpha(x)$total$raw_alpha, 2)))
})

# confidence
map(1:5, function(i) {
  x <- team[[i]] %>% 
    ungroup() %>% 
    select(group, itemnum, Grp_Confidence.RESP) %>% 
    group_by(group, itemnum) %>% 
    summarise(Grp_Confidence.RESP = Grp_Confidence.RESP[1]) %>% 
    spread(itemnum, Grp_Confidence.RESP) %>% 
    ungroup() %>% 
    select(-group)
  
  print(paste0(tests[[i]], " - Reliability for group confidence = ", round(psych::alpha(x)$total$raw_alpha, 2)))
})


