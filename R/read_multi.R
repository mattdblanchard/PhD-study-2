if (file.exists("data/rds/surveys_multi_raw.rds")) {
  surveys_multi <- readRDS("data/rds/surveys_multi_raw.rds")
  
  } else {

library(jsonlite)
library(tidyverse)
source("R/parse_qrtengine.R")

# create an empty list to store each parsed survey
surveys_multi <- list()

# create a list of the column names to select for parsing JSON data via the for loop
list_of_cols <- c("ind_stimulus", "ind_confidence", "ind_decision", "wait", "grp_stimulus", "grp_confidence", "grp_decision")

# Applying decision rules -------------------------------------------------
# create an empty list to save each parsed column as separate dfs
list_of_dfs <- list()

# read in the data
# if using office computer
# d <- read_csv("/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/ADR_B2.csv")[c(-1, -2), ]
# if using laptop
d <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/ADR_B2.csv")[c(-1, -2), ]

# parse each column of JSON data
for (i in list_of_cols) {
  df <- d %>% 
    select(matches(i)) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  list_of_dfs[[i]] <- df
}

x <- cbind(list_of_dfs$ind_stimulus, list_of_dfs$ind_confidence, list_of_dfs$ind_decision, list_of_dfs$wait, 
           list_of_dfs$grp_stimulus, list_of_dfs$grp_confidence, list_of_dfs$grp_decision)

b <- d %>% select(-matches("_ind_stimulus|_ind_confidence|_ind_decision|_wait|_grp_stimulus|_grp_confidence|_grp_decision"))
b <- do.call(rbind, replicate(nrow(x) / nrow(d), b, simplify = FALSE))

# Return binded columns
surveys_multi$adr_multi <- cbind(b, x)


# CRT -------------------------------------------------
# create an empty list to save each parsed column as separate dfs
list_of_dfs <- list()

# read in the data
# if using office computer
# d <- read_csv("/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/CRT_B2.csv")[c(-1, -2), ]

# if using laptop
d <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/CRT_B2.csv")[c(-1, -2), ]

# parse each column of JSON data
for (i in list_of_cols) {
  df <- d %>% 
    select(matches(i)) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  list_of_dfs[[i]] <- df
}

x <- cbind(list_of_dfs$ind_stimulus, list_of_dfs$ind_confidence, list_of_dfs$ind_decision, list_of_dfs$wait, 
           list_of_dfs$grp_stimulus, list_of_dfs$grp_confidence, list_of_dfs$grp_decision)

b <- d %>% select(-matches("_ind_stimulus|_ind_confidence|_ind_decision|_wait|_grp_stimulus|_grp_confidence|_grp_decision"))
b <- do.call(rbind, replicate(nrow(x) / nrow(d), b, simplify = FALSE))

# Return binded columns
surveys_multi$crt_multi <- cbind(b, x)


# GK -------------------------------------------------
# create an empty list to save each parsed column as separate dfs
list_of_dfs <- list()

# read in the data
# if using office computer
# d <- read_csv("/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/GK_B2.csv")[c(-1, -2), ]

# if using laptop
d <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/GK_B2.csv")[c(-1, -2), ]

# parse each column of JSON data
for (i in list_of_cols) {
  df <- d %>% 
    select(matches(i)) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  list_of_dfs[[i]] <- df
}

x <- cbind(list_of_dfs$ind_stimulus, list_of_dfs$ind_confidence, list_of_dfs$ind_decision, list_of_dfs$wait, 
           list_of_dfs$grp_stimulus, list_of_dfs$grp_confidence, list_of_dfs$grp_decision)

b <- d %>% select(-matches("_ind_stimulus|_ind_confidence|_ind_decision|_wait|_grp_stimulus|_grp_confidence|_grp_decision"))
b <- do.call(rbind, replicate(nrow(x) / nrow(d), b, simplify = FALSE))

# Return binded columns
surveys_multi$gk_multi <- cbind(b, x)


# R2F negative -------------------------------------------------
# create a list of the column names to select for parsing JSON data via the for loop
list_of_cols <- c("ind_stimulus","wait", "grp_stimulus")

# create an empty list to save each parsed column as separate dfs
list_of_dfs <- list()

# read in the data
# if using office computer
# d <- read_csv("/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/R2F_neg_B2.csv")[c(-1, -2), ]

# if using laptop
d <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/R2F_neg_B2.csv")[c(-1, -2), ]

# parse each column of JSON data
for (i in list_of_cols) {
  df <- d %>% 
    select(matches(i)) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  list_of_dfs[[i]] <- df
}

x <- cbind(list_of_dfs$ind_stimulus, list_of_dfs$wait, list_of_dfs$grp_stimulus)

b <- d %>% select(-matches("_ind_stimulus|_wait|_grp_stimulus"))
b <- do.call(rbind, replicate(nrow(x) / nrow(d), b, simplify = FALSE))

# Return binded columns
surveys_multi$r2f_neg_multi <- cbind(b, x)


# R2F positive -------------------------------------------------
# create an empty list to save each parsed column as separate dfs
list_of_dfs <- list()

# read in the data
# if using office computer
# d <- read_csv("/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/R2F_pos_B2.csv")[c(-1, -2), ]

# if using laptop
d <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/R2F_pos_B2.csv")[c(-1, -2), ]

# parse each column of JSON data
for (i in list_of_cols) {
  df <- d %>% 
    select(matches(i)) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  list_of_dfs[[i]] <- df
}

x <- cbind(list_of_dfs$ind_stimulus, list_of_dfs$wait, list_of_dfs$grp_stimulus)

b <- d %>% select(-matches("_ind_stimulus|_wait|_grp_stimulus"))
b <- do.call(rbind, replicate(nrow(x) / nrow(d), b, simplify = FALSE))

# Return binded columns
surveys_multi$r2f_pos_multi <- cbind(b, x)


# Composite Emotions -------------------------------------------------
# create a list of the column names to select for parsing JSON data via the for loop
list_of_cols <- c("stimulus","confidence", "decision")

# create an empty list to save each parsed column as separate dfs
list_of_dfs <- list()

# read in the data
# if using office computer
# d <- read_csv("/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/CET_B2.csv")[c(-1, -2), ]

# if using laptop
d <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/CET_B2.csv")[c(-1, -2), ]

# parse each column of JSON data
for (i in list_of_cols) {
  df <- d %>% 
    select(matches(i)) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  list_of_dfs[[i]] <- df
}

x <- cbind(list_of_dfs$stimulus, list_of_dfs$confidence, list_of_dfs$decision)

b <- d %>% select(-matches("_stimulus|_confidence|_decision"))
b <- do.call(rbind, replicate(nrow(x) / nrow(d), b, simplify = FALSE))

# Return binded columns
surveys_multi$cet_multi <- cbind(b, x)


# MDMT individual -------------------------------------------------
# create a list of the column names to select for parsing JSON data via the for loop
list_of_cols <- c("stimulus","confidence", "decision")

# create an empty list to save each parsed column as separate dfs
list_of_dfs <- list()

# read in the data
# if using office computer
# d <- read_csv("/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/MDMT_ind_B2.csv")[c(-1, -2), ]

# if using laptop
d <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/MDMT_ind_B2.csv")[c(-1, -2), ]

# parse each column of JSON data
for (i in list_of_cols) {
  df <- d %>% 
    select(matches(i)) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  list_of_dfs[[i]] <- df
}

x <- cbind(list_of_dfs$stimulus, list_of_dfs$confidence, list_of_dfs$decision)

b <- d %>% select(-matches("_stimulus|_confidence|_decision"))
b <- do.call(rbind, replicate(nrow(x) / nrow(d), b, simplify = FALSE))

# Return binded columns
surveys_multi$mdmt_ind_multi <- cbind(b, x)


# MDMT group -------------------------------------------------
# create an empty list to save each parsed column as separate dfs
list_of_dfs <- list()

# read in the data
# if using office computer
# d <- read_csv("/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/MDMT_team_B2.csv")[c(-1, -2), ]

# if using laptop
d <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/MDMT_team_B2.csv")[c(-1, -2), ]

# parse each column of JSON data
for (i in list_of_cols) {
  df <- d %>% 
    select(matches(i)) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  list_of_dfs[[i]] <- df
}

x <- cbind(list_of_dfs$stimulus, list_of_dfs$confidence, list_of_dfs$decision)

b <- d %>% select(-matches("_stimulus|_confidence|_decision"))
b <- do.call(rbind, replicate(nrow(x) / nrow(d), b, simplify = FALSE))

# Return binded columns
surveys_multi$mdmt_grp_multi <- cbind(b, x)



# RAPM individual -------------------------------------------------
# create an empty list to save each parsed column as separate dfs
list_of_dfs <- list()

# read in the data
# if using office computer
# d <- read_csv("/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/RAPM_ind_B2.csv")[c(-1, -2), ]

# if using laptop
d <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/RAPM_ind_B2.csv")[c(-1, -2), ]

# parse each column of JSON data
for (i in list_of_cols) {
  df <- d %>% 
    select(matches(i)) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  list_of_dfs[[i]] <- df
}

x <- cbind(list_of_dfs$stimulus, list_of_dfs$confidence, list_of_dfs$decision)

b <- d %>% select(-matches("_stimulus|_confidence|_decision"))
b <- do.call(rbind, replicate(nrow(x) / nrow(d), b, simplify = FALSE))

# Return binded columns
surveys_multi$rapm_ind_multi <- cbind(b, x)


# RAPM group -------------------------------------------------
# create an empty list to save each parsed column as separate dfs
list_of_dfs <- list()

# read in the data
# if using office computer
# d <- read_csv("/Users/Matty/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/RAPM_team_B2.csv")[c(-1, -2), ]

# if using laptop
d <- read_csv("/Users/blattymanchard/Dropbox (Sydney Uni)/R_projects/S2_CI/data/java_multi/RAPM_team_B2.csv")[c(-1, -2), ]

# parse each column of JSON data
for (i in list_of_cols) {
  df <- d %>% 
    select(matches(i)) %>%
    map(parse_col) %>%
    map(as_data_frame) %>%
    bind_rows()
  
  list_of_dfs[[i]] <- df
}

x <- cbind(list_of_dfs$stimulus, list_of_dfs$confidence, list_of_dfs$decision)

b <- d %>% select(-matches("_stimulus|_confidence|_decision"))
b <- do.call(rbind, replicate(nrow(x) / nrow(d), b, simplify = FALSE))

# Return binded columns
surveys_multi$rapm_grp_multi <- cbind(b, x)

# clear redundant objects from the environment
rm(list = setdiff(ls(), c("surveys_multi")))

saveRDS(surveys_multi, "data/rds/surveys_multi_raw.rds")

}
