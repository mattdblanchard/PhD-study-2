# 
# # load cleaned data
# source("R/import_data.R")
# 
# scripts <- list.files("R/survey_prep/", pattern = ".R")[c(-2, -5, -6, -9, -22)]
# 
# for (i in scripts) {
#   source(paste0("R/survey_prep/", i))
# }


# ID reliability
# team reliability
cet <- cet %>% rename(itemnum = CetItemNum)
run <- run %>% rename(itemnum = RUNNINGtest.TrialNr)
risk <- risk %>% mutate(Stimulus.RESP = ifelse(Stimulus.RESP == "a", 0, 1))
trust <- trust %>% mutate(Stimulus.RESP = ifelse(key == 1, Stimulus.RESP, 6 - Stimulus.RESP))  # Reverse score negative items

id_test <- list(cet, run)
id_self <- list(adapt, trust, risk)

tests <- c("cet", "run")
self <- c("adapt","risk", "trust")

# accuracy measures
map(1:2, function(i) {
  x <- id_test[[i]] %>%
    select(uid, itemnum, Stimulus.ACC) %>% 
    pivot_wider(names_from = itemnum, values_from = Stimulus.ACC) %>% 
    ungroup() %>% 
    select(-uid)
  
  print(paste0(tests[[i]], " - Reliability for accuracy = ", round(psych::alpha(x)$total$raw_alpha, 2)))
})

# self-report measures
map(1:3, function(i) {
  x <- id_self[[i]] %>%
    select(uid, itemnum, Stimulus.RESP) %>% 
    pivot_wider(names_from = itemnum, values_from = Stimulus.RESP) %>% 
    ungroup() %>% 
    select(-uid)
  
  print(paste0(self[[i]], " - Reliability = ", round(psych::alpha(x)$total$raw_alpha, 2)))
})
