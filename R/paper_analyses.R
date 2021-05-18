# load packages
packages <- c("tidyverse")

if (any(!packages %in% installed.packages())) { # install any packages not currently installed
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
} 

lapply(packages, library, character.only = TRUE)

# load data
d.uid <- read_csv("data/190619_uid_data.csv")
d.grp <- read_csv("data/210226_dyad_data.csv")

# 18080915_1 = ADR
# 18073114_1 = CRT
# 
# adr.uid %>% filter(group == "18080915_1")

# descriptives for individual and group accuracy and confidence
means <- d.grp %>% 
  select(matches(c(".acc", ".conf"))) %>% 
  select(-matches(c(".accR", ".accUR", ".confR", ".confUR", 
                    "trait", "diff", "wm.acc", "emotion.acc", "factor"))) %>%
  pivot_longer(everything(), names_to = "var", values_to = "val") %>% 
  separate(var, into = c("test", "g", "var")) %>%
  group_by(test, g, var) %>% 
  summarise(m = round(mean(val, na.rm = T),2),
            sd = round(sd(val, na.rm = T),2)) %>% 
  pivot_longer(m:sd, names_to = "par", values_to = "val") %>% 
  unite(par, g, par) %>%
  pivot_wider(names_from = par, values_from = val) %>% 
  mutate(var = str_replace(var, "O", "")) %>% 
  arrange(var) 

test <- d.grp %>% 
  select(group, matches(c(".acc", ".conf"))) %>% 
  select(-matches(c(".accR", ".accUR", ".confR", ".confUR", 
                    "trait", "diff", "wm.acc", "emotion.acc", "factor"))) %>% 
  pivot_longer(-group, names_to = "var", values_to = "val") %>% 
  separate(var, into = c("test", "g", "var")) %>% 
  mutate(var = str_replace(var, "O", "")) %>% 
  pivot_wider(names_from = g, values_from = val) %>% 
  nest(data = c("group", "ind", "grp"))

test <- test %>% 
  mutate(fit = map(data, ~t.test(.x$ind, .x$grp, paired = T, var.equal = T)),
         tidy_fit = map(fit, broom::tidy)) %>% 
         unnest(tidy_fit) %>% 
           select(-data, -fit, -method, -alternative) %>% 
           rename(t_value = statistic, df = parameter, mean_diff = estimate) %>% 
           mutate(p.value = round(p.value, 4),
                  t_value = ifelse(p.value < .001, paste0(round(t_value,2), "***")))
  

means %>% left_join(test, by = c("test", "var")) %>% 
  select(test, var, mean_diff, ind_m, ind_sd, grp_m, grp_sd, t_value, 
         p.value, df, conf.low, conf.high) %>% 
  write_csv("output/paper_acc_conf_descriptives.csv")

# reliability estimates
# source("R/reliability_acc_conf.R")


# descriptives for ID measures
# add comms measures
d.grp %>% 
  select(wm.acc, adapt.global:neuroticism) %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>% 
  mutate(var = factor(var, levels = c("wm.acc", "emotion.acc", "risk.aversion", "trust", 
                                      "agreeableness", "conscientiousness", "extraversion", 
                                      "intellect", "neuroticism", "adapt.global", "speak_volume", "speak_ratio"))) %>% 
  group_by(var) %>% 
  summarise(m = round(mean(val, na.rm = T),2),
            sd = round(sd(val, na.rm = T),2)) %>% 
  write_csv("output/paper_id_descriptives.csv")


# reliability
# source("R/reliability_id.R")

# communication measures
d.grp %>% 
  select(group, matches("speak_num_turn_sum|speak_num_turn_ratio|speak_total_dur_sum|speak_total_dur_ratio"),
         -matches("gk_")) %>% 
  pivot_longer(-group, names_to = "var", values_to = "val") %>% 
  separate(var, into = c("test", "speak", "total", "dur", "var")) %>% 
  unite(var, dur, var) %>% 
  mutate(var = factor(var, levels = c("turn_sum", "turn_ratio", "dur_sum", "dur_ratio"))) %>% 
  group_by(test, var) %>% 
  summarise(m = mean(val, na.rm = T), 
            sd = sd(val, na.rm = T)) %>% 
  pivot_longer(m:sd, names_to = "var2", values_to = "val") %>% 
  unite(var2, test, var2) %>% 
  pivot_wider(names_from = var2, values_from = val) %>% 
  write_csv("output/paper_comms_descriptives.csv")

  # correlations bw comms measures
source("R/corr_matrix_sig.R")
source("R/corstarsfunction.R")

corstarssigcheck(d.grp %>%
              select(adr_speak_num_turn_sum, adr_speak_total_dur_sum, adr_speak_num_turn_ratio, adr_speak_total_dur_ratio,
                     crt_speak_num_turn_sum, crt_speak_total_dur_sum, crt_speak_num_turn_ratio, crt_speak_total_dur_ratio,
                     rapm_speak_num_turn_sum, rapm_speak_total_dur_sum, rapm_speak_num_turn_ratio, rapm_speak_total_dur_ratio)) %>% 
  write_csv("output/paper_corrs_comms_vars.csv")

data.frame(x) %>% 
  mutate(vars = names(x))

data.frame(d.grp %>%
  select(adr_speak_num_turn_sum, adr_speak_total_dur_sum, adr_speak_num_turn_ratio, adr_speak_total_dur_ratio,
         crt_speak_num_turn_sum, crt_speak_total_dur_sum, crt_speak_num_turn_ratio, crt_speak_total_dur_ratio,
         rapm_speak_num_turn_sum, rapm_speak_total_dur_sum, rapm_speak_num_turn_ratio, rapm_speak_total_dur_ratio) %>% 
  cor(use = "pairwise.complete.obs")) %>% 
  write_csv("output/paper_corrs_comms_vars.csv")
  

# comms vars -------------------------------------------------------
# descriptives
d.grp %>% 
  select(n_speak_turn_sum:speak_total_dur_ratio) %>% 
  ungroup() %>% 
  summarise(across(n_speak_turn_sum:speak_total_dur_ratio, mean_sd, .names = "{.fn}_{.col}")) %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>% 
  separate(var, into = c("par", "a", "b", "c", "d")) %>% 
  unite(var, a,b,c,d) %>% 
  pivot_wider(names_from = par, values_from = val) %>% 
  write_csv("output/paper_comms_desc.csv")