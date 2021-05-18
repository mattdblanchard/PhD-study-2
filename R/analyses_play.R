library(tidyverse)

d <- read_csv("data/200525_dyad_data.csv")



# function to identify sig. relationships between DVs and IVs
sig_corrs <- function(data, dv) {
  # store correlations as df and convert to long format
  r <- data.frame(data$r) %>% 
    mutate(rowname = rownames(data$r)) %>% 
    gather(var , r, -rowname) %>% 
    mutate(r = round(r, 2))
  
  # store p-values as df and convert to long format
  p <- data.frame(data$p) %>% 
    mutate(rowname = rownames(data$p)) %>% 
    gather(var , p, -rowname)
  
  # combine r and p and print sig. correlations for each comms factor
  map(dv, function(i) {
    tmp <- r %>% 
      left_join(p, by = c("rowname", "var")) %>% 
      filter(var %in% i) %>% 
      # filter(!rowname %in% dv) %>% 
      filter(p < .05 & r != 1) %>% 
      select(-p)
    
    if (length(tmp) > 1) {
      if (i == "speak_volume") {
        tmp <- tmp %>%
          filter(!str_detect(rowname, "speak_ratio")) %>% 
          filter(!str_detect(rowname, "grp_acc")) %>%
          filter(!str_detect(rowname, "ind_")) %>%
          filter(!str_detect(rowname, "diff_"))
      } else if (i == "speak_ratio") {
        tmp <- tmp %>%
          filter(!str_detect(rowname, "speak_volume")) %>% 
          filter(!str_detect(rowname, "grp_acc")) %>%
          filter(!str_detect(rowname, "ind_")) %>%
          filter(!str_detect(rowname, "diff_"))
      } else {
        tmp <- tmp %>% 
          filter(!str_detect(rowname, "grp_acc")) %>% 
          filter(!str_detect(rowname, "grp_conf")) %>% 
          filter(!str_detect(rowname, "grp_bias")) %>% 
          filter(!str_detect(rowname, "diff"))
      }
    }
    
    tmp %>% spread(var, r)
    
  })
}

# function to select variables for the regression models
select_vars <- function(dv) {
    bind_rows(x) %>% 
      select(rowname, dv) %>% 
      na.omit() %>% select(rowname)
}

# function to fit regression model, and run diagnostics for overall simulation-derived metrics
regress <- function(dv) {
  # print dv name
  print(paste0("DV = ", dv))

    n <- var$rowname
    
    # create regression formula
    fm <- as.formula(paste0("d$", dv, " ~ ", paste0("scale(", n, ")", collapse = " + ")))
    
    # fit model
    f <- lm(fm, data = d)

  # results
  print(summary(f))
  
  # print zero-order, semi-part and partial correlations
  data <- d %>% select(dv, var$rowname) %>% na.omit()
  library(ppcor) # package requires MASS which removes the select function from dplyr
  r <- cor(data)
  part <- pcor(data)
  spart <- spcor(data)
  cor <- data.frame(zero_order = round(r[1,c(2:length(data))],2),
                    partial = round(part[[1]][1,c(2:length(data))],2),
                    part = round(spart[[1]][1,c(2:length(data))],2))
  
  print(cor)
  
  # detach packages and reload dplyr
  detach("package:ppcor", unload=TRUE)
  detach("package:MASS", unload=TRUE)
  library(dplyr)
  
  # scatterplots of relationship between each IV and the DV
  p <- d %>% select(dv, var$rowname) %>% 
    gather(var, val, var$rowname) %>% 
    ggplot(aes(x = val, y = get(dv))) +
    geom_point() +
    facet_wrap(~var, scales = "free_x") +
    geom_smooth(method='lm', formula = y~x, se=FALSE) +
    theme_minimal() +
    labs(x = "", y = str_replace(dv, "_", " "))
  
  print(p)
  
  # check assumptions and multicollinearity
  plot(f)
  
  # plot correlations between IVs
  p <- d %>% select(var$rowname) %>% 
    GGally::ggcorr(method = "pairwise.complete.obs", 
           label = TRUE, label_size = 3, label_round = 2, label_alpha = TRUE, # add text correlations value to plot
           hjust = 0.9, size = 4, color = "grey50", layout.exp = 3) # move variable labels
  # more detailed plot: distribution, scatterplot, correlations
  # vars %>% select(x, issue) %>% 
  #   ggpairs(., lower = list(continuous = wrap("smooth", alpha = 0.3, size=0.1)))
  
  # print correlation plot
  print(p)
  
  # check for multicollinearity
  if (length(var$rowname) > 1) {
    print(mctest::omcdiag(d %>% select(var$rowname), d %>% select(dv)))
    
    print(mctest::imcdiag(d %>% select(var$rowname), d %>% select(dv)))
  }
}




# correlations between difference scores and the other variables
name <- d %>% select(speak_volume, speak_ratio) %>% names()

x <- d %>% 
  select(wm.acc:neuroticism, age:aus.born, eng.fl, team.familiarity,
         ind_conf_factor:diff_bias, speak_volume, speak_ratio) %>% 
  select_if(is.numeric) %>% 
  psych::corr.test(adjust = "none") 

x <- sig_corrs(x, name)


# comms -------------------------------------------------------------------
# set the dv
dv <- "speak_volume"

# select IVs
var <- select_vars(dv)

# fit regression model
regress(dv)


# set the dv
dv <- "speak_ratio"

# select IVs
var <- select_vars(dv)

# fit regression model
regress(dv)



# test outcomes -----------------------------------------------------------
# correlations between difference scores and the other variables
name <- d %>% select(grp_acc_factor, grp_conf_factor, grp_bias_factor,
                     diff_acc, diff_conf, diff_bias) %>% names()

x <- d %>% 
  select(wm.acc, trust:neuroticism, age:aus.born, eng.fl, team.familiarity,
         ind_conf_factor:diff_bias, speak_volume, speak_ratio) %>% 
  select_if(is.numeric) %>% 
  corr.test(adjust = "none") 

x <- sig_corrs(x, name)


# set the dv
dv <- "grp_acc_factor"

# select IVs
var <- select_vars(dv)

# fit regression model
regress(dv)


# set the dv
dv <- "grp_conf_factor"

# select IVs
var <- select_vars(dv)

# fit regression model
regress(dv)


# set the dv
dv <- "grp_bias_factor"

# select IVs
var <- select_vars(dv)

# fit regression model
regress(dv)







source("R/corr_matrix_sig.R")

d %>% 
  select(speak_ratio, speak_volume, prop.female, emotion.acc) %>% 
  star_matrix()
