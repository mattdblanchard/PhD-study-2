# "GGally", "naniar", "psych", "corrr", "ez"
packages <- c("tidyverse", "lavaan", "lavaanPlot", "broom")

lapply(packages, library, character.only = TRUE)

d.uid <- read_csv("data/190619_uid_data.csv")
d.grp <- read_csv("data/190619_dyad_data.csv")


d.uid %>% 
  select(accuracy, adr.ind.acc, crt.ind.acc, rapm.ind.acc, 
         confidence, adr.ind.conf, crt.ind.conf, rapm.ind.conf)


cfa_one <- paste0('
                ci  =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc +
                  adr.ind.conf + crt.ind.conf + rapm.ind.conf')

cfa_two <- paste0('
                accuracy  =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc 
                
                confidence =~ adr.ind.conf + crt.ind.conf + rapm.ind.conf
                     ')



fit_one <- cfa(cfa_one, data = d.uid, missing = "ML")
fit_two <- cfa(cfa_two, data = d.uid, missing = "ML")

# display summary output
summary(fit_one, fit.measures=TRUE)
summary(fit_two, fit.measures=TRUE)

fitMeasures(fit_one, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

fitMeasures(fit_two, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                     "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper"))

# look at the correlation between the residuals (ideally everything should be <= .1)
residuals(fit_one, type = "cor")
residuals(fit_two, type = "cor")

# test the difference between the models
anova(fit_one, fit_two)     

# predict factor scores for each participant
d.uid <- bind_cols(d.uid, data.frame(predict(fit_two))) %>% 
  rename(ind.accuracy = accuracy,
         ind.confidence = confidence)

cor.test(d.uid$ind.accuracy, d.uid$ind.confidence)







d.uid %>% 
  select(uid, adr.ind.acc, crt.ind.acc, rapm.ind.acc, 
         adr.ind.conf, crt.ind.conf, rapm.ind.conf) %>%
  group_by(uid) %>% 
  # pivot_longer(-uid, names_to = "var", values_to = "val") %>% 
  count(is.na(.))






  






