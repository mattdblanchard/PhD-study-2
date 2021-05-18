# detailed texts on implementing CFA using lavaan can be found here:
# https://methodenlehre.github.io/intro-to-rstats/cfa-and-sem-with-lavaan.html
# and here:
# https://shiny.rit.albany.edu/stat/cfa1test/lavaan.html


# load packages
packages <- c("tidyverse", "psych", "knitr", "kableExtra", "lavaan", "lavaanPlot", "semPlot")

if (any(!packages %in% installed.packages())) { # install any packages not currently installed
  install.packages(packages[!packages %in% installed.packages()[,"Package"]])
} 

lapply(packages, library, character.only = TRUE)

# load functions for PCA
source("R/pca_functions.R")

# load datasets
d.uid <- read_csv("data/190619_uid_data.csv")
d.grp <- read_csv("data/210226_dyad_data.csv")

# utilities
fit_cfa <- function(model) {
  fit <- cfa(model, data = d.grp, std.lv=TRUE, missing="FIML")
}

clean_table <- function(data, title) {
  data %>% 
    kable(booktabs = T, caption = title) %>%
    kable_styling(font_size = 12, latex_options = "HOLD_position")
}

# print goodness of fit indices
gfi <- function(fit_data) {
  x <- data.frame(fitMeasures(fit_data, fit.measures= c("chisq", "df", "pvalue", "gfi", "tli", 
                                                        "cfi", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "aic"))) %>%
    rownames_to_column()
  
  names(x) <- c("index", "value")  
  
  x %>% 
    mutate(value = round(value, 3))
  # clean_table("Goodness of fit indices")
}


# check assumptions -------------------------------------------------------
# normality
d.grp %>% 
  select(adr.ind.acc, crt.ind.acc, rapm.ind.acc, 
         adr.ind.conf, crt.ind.conf, rapm.ind.conf) %>% 
  pivot_longer(everything(), names_to = "var", values_to = "val") %>% 
  ggplot(aes(x = val, fill = var)) +
  geom_histogram(aes(y=..density..)) +
  geom_density(colour = "blue", lwd = .7, alpha = 0) +
  facet_wrap(~var) +
  theme_classic() +
  theme(legend.position = "none")


# individuals -------------------------------------------------------------
# Fit one factor model (uncorrelated error)
cfa_one <- paste0('
                # measurement model
                ci  =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc +
                        adr.ind.conf + crt.ind.conf + rapm.ind.conf
                ')

fit1 <- fit_cfa(cfa_one)

# display summary output
# summary(fit, fit.measures=TRUE, standardized=TRUE)
gfi(fit1)

# look at the correlation between the residuals (ideally everything should be <= .1)
# these residual correlations drive RMSEA so if you have too many corrs > .1 this index will be poor
residuals(fit1, type = "cor")$cov

# inspect the residual matrix that results from the subtraction of the variance-covariance matrix implied by 
# the model from the actual observed (empirical) variance-covariance matrix
# the smaller the residual values the better the model fit
# can extract the implied and observed separately
# lavInspect(fit, what = "sampstat")$cov - 
#   lavInspect(fit, what = "implied")$cov

lavInspect(fit1, what = "resid")

# this produces the residual matrix in standardised units
# residuals greater than 1.96 will be sig at .05
resid(fit1, type = "standardized")

# Fit one factor model (uncorrelated error)
cfa_one_corr <- paste0('
                # measurement model
                ci  =~ adr.ind.acc + crt.ind.acc + rapm.ind.acc +
                        adr.ind.conf + crt.ind.conf + rapm.ind.conf
                        
                # residual correlations        
                adr.ind.acc ~~ adr.ind.conf
                crt.ind.acc ~~ crt.ind.conf
                rapm.ind.acc ~~ rapm.ind.conf
                ')

fit2 <- fit_cfa(cfa_one_corr)

# display summary output
# summary(fit, fit.measures=TRUE, standardized=TRUE)
gfi(fit2)

# look at the correlation between the residuals (ideally everything should be <= .1)
# these residual correlations drive RMSEA so if you have too many corrs > .1 this index will be poor
residuals(fit2, type = "cor")$cov

# inspect the residual matrix that results from the subtraction of the variance-covariance matrix implied by 
# the model from the actual observed (empirical) variance-covariance matrix
# the smaller the residual values the better the model fit
lavInspect(fit2, what = "resid")

# this produces the residual matrix in standardised units
# residuals greater than 1.96 will be sig at .05
resid(fit2, type = "standardized")

# compare uncorrelated and correlated models
kable(anova(fit1,fit2), booktabs=TRUE, format="markdown")


# Fit two factor model (uncorrelated)
cfa_two <- paste0('
                # measurement model
                accuracy  =~  adr.ind.acc + crt.ind.acc + rapm.ind.acc
                confidence =~ adr.ind.conf + crt.ind.conf + rapm.ind.conf
                     ')

fit1 <- fit_cfa(cfa_two)

# display summary output
# summary(fit_two_corr, fit.measures=TRUE, standardized=TRUE)
gfi(fit1)

# look at the correlation between the residuals (ideally everything should be <= .1)
# these residual correlations drive RMSEA so if you have too many corrs > .1 this index will be poor
residuals(fit1, type = "cor")$cov

# inspect the residual matrix that results from the subtraction of the variance-covariance matrix implied by 
# the model from the actual observed (empirical) variance-covariance matrix
# the smaller the residual values the better the model fit
lavInspect(fit1, what = "resid")

# this produces the residual matrix in standardised units
# residuals greater than 1.96 will be sig at .05
resid(fit1, type = "standardized")


# Fit two factor model (correlated)
cfa_two_corr <- paste0('
                # measurement model
                accuracy  =~  adr.ind.acc + crt.ind.acc + rapm.ind.acc
                confidence =~ adr.ind.conf + crt.ind.conf + rapm.ind.conf

                # residual correlations
                adr.ind.acc ~~ adr.ind.conf
                crt.ind.acc ~~ crt.ind.conf
                rapm.ind.acc ~~ rapm.ind.conf
                     ')

fit2 <- fit_cfa(cfa_two_corr)

# display summary output
# summary(fit_two_corr, fit.measures=TRUE, standardized=TRUE)
gfi(fit2)

# look at the correlation between the residuals (ideally everything should be <= .1)
# these residual correlations drive RMSEA so if you have too many corrs > .1 this index will be poor
residuals(fit2, type = "cor")$cov

# inspect the residual matrix that results from the subtraction of the variance-covariance matrix implied by 
# the model from the actual observed (empirical) variance-covariance matrix
# the smaller the residual values the better the model fit
lavInspect(fit2, what = "resid")

# this produces the residual matrix in standardised units
# residuals greater than 1.96 will be sig at .05
resid(fit2, type = "standardized")

# compare uncorrelated and correlated models
kable(anova(fit1,fit2), booktabs=TRUE, format="markdown")

# print regression coefficients
# The lavaan package provides a set of extractor functions to pull specific portions of the output 
# to further process or analyze. The parameterEstimates(), standardizedSolution(), and fitMeasures() 
# functions can be used to return only the unstandardized estimates, standardized estimates, and model 
# fit statistics, respectively.

# parameterEstimates(fit, standardized=TRUE) %>% 
#   filter(op == "=~") %>% 
#   select('Latent Factor'=lhs, Indicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
#   knitr::kable(digits = 3, booktabs=TRUE, format="markdown", caption="Factor Loadings")

parameterEstimates(fit2, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('latent_factor'=lhs, indicator=rhs, beta=std.all) %>% 
  pivot_wider(names_from = latent_factor, values_from = beta)

# plot model
# semPaths(fit, residuals=F, nCharNodes = 0, style = "lisrel", rotation = 1)
semPaths(fit2, residuals=F, sizeMan=7, "std", style = "lisrel", 
         rotation = 1, nCharNodes = 0, 
         posCol=c("blue", "red"),
         edge.color="blue",
         edge.label.cex=1.2)

# correlate factor scores
x <- corr.test(predict(fit2))

data.frame(r = round(x$r[1,2],2),
           p = x$p[1,2]) %>% 
  mutate(p = ifelse(p <.001, "<.001", p))

# save factor scores
x <- data.frame(ind_accuracy_cfa = predict(fit2)[,1],
           ind_confidence_cfa = predict(fit2)[,2])

d.grp <- bind_cols(d.grp, x)

# groups ------------------------------------------------------------------
# Fit one factor model (uncorrelated error)
cfa_one <- paste0('
                # measurement model
                ci  =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc +
                        adr.grp.conf + crt.grp.conf + rapm.grp.conf
                ')

fit1 <- fit_cfa(cfa_one)

# display summary output
# summary(fit, fit.measures=TRUE, standardized=TRUE)
gfi(fit1)

# look at the correlation between the residuals (ideally everything should be <= .1)
# these residual correlations drive RMSEA so if you have too many corrs > .1 this grpex will be poor
residuals(fit1, type = "cor")$cov

# inspect the residual matrix that results from the subtraction of the variance-covariance matrix implied by 
# the model from the actual observed (empirical) variance-covariance matrix
# the smaller the residual values the better the model fit
lavInspect(fit1, what = "resid")

# this produces the residual matrix in standardised units
# residuals greater than 1.96 will be sig at .05
resid(fit1, type = "standardized")


# Fit one factor model (uncorrelated error)
cfa_one_corr <- paste0('
                # measurement model
                ci  =~ adr.grp.acc + crt.grp.acc + rapm.grp.acc +
                        adr.grp.conf + crt.grp.conf + rapm.grp.conf
                        
                # residual correlations
                adr.grp.acc ~~ adr.grp.conf
                crt.grp.acc ~~ crt.grp.conf
                rapm.grp.acc ~~ rapm.grp.conf
                ')

fit2 <- fit_cfa(cfa_one_corr)

# display summary output
# summary(fit, fit.measures=TRUE, standardized=TRUE)
gfi(fit2)

# look at the correlation between the residuals (ideally everything should be <= .1)
# these residual correlations drive RMSEA so if you have too many corrs > .1 this grpex will be poor
residuals(fit2, type = "cor")$cov

# inspect the residual matrix that results from the subtraction of the variance-covariance matrix implied by 
# the model from the actual observed (empirical) variance-covariance matrix
# the smaller the residual values the better the model fit
lavInspect(fit2, what = "resid")

# this produces the residual matrix in standardised units
# residuals greater than 1.96 will be sig at .05
resid(fit2, type = "standardized")

# compare uncorrelated and correlated models
kable(anova(fit1,fit2), booktabs=TRUE, format="markdown")


# Fit two factor model (uncorrelated)
cfa_two <- paste0('
                # measurement model
                accuracy  =~  adr.grp.acc + crt.grp.acc + rapm.grp.acc
                confidence =~ adr.grp.conf + crt.grp.conf + rapm.grp.conf
                     ')

fit1 <- fit_cfa(cfa_two)

# display summary output
# summary(fit_two_corr, fit.measures=TRUE, standardized=TRUE)
gfi(fit1)

# look at the correlation between the residuals (ideally everything should be <= .1)
# these residual correlations drive RMSEA so if you have too many corrs > .1 this grpex will be poor
residuals(fit1, type = "cor")$cov

# inspect the residual matrix that results from the subtraction of the variance-covariance matrix implied by 
# the model from the actual observed (empirical) variance-covariance matrix
# the smaller the residual values the better the model fit
lavInspect(fit1, what = "resid")

# this produces the residual matrix in standardised units
# residuals greater than 1.96 will be sig at .05
resid(fit1, type = "standardized")


# Fit two factor model (correlated)
cfa_two_corr <- paste0('
                # measurement model
                accuracy  =~  adr.grp.acc + crt.grp.acc + rapm.grp.acc
                confidence =~ adr.grp.conf + crt.grp.conf + rapm.grp.conf

                # residual correlations
                adr.grp.acc ~~ adr.grp.conf
                crt.grp.acc ~~ crt.grp.conf
                rapm.grp.acc ~~ rapm.grp.conf
                     ')

fit2 <- fit_cfa(cfa_two_corr)

# display summary output
# summary(fit_two_corr, fit.measures=TRUE, standardized=TRUE)
gfi(fit2)

# look at the correlation between the residuals (ideally everything should be <= .1)
# these residual correlations drive RMSEA so if you have too many corrs > .1 this grpex will be poor
residuals(fit2, type = "cor")$cov

# inspect the residual matrix that results from the subtraction of the variance-covariance matrix implied by 
# the model from the actual observed (empirical) variance-covariance matrix
# the smaller the residual values the better the model fit
lavInspect(fit2, what = "resid")

# this produces the residual matrix in standardised units
# residuals greater than 1.96 will be sig at .05
resid(fit2, type = "standardized")

# compare uncorrelated and correlated models
kable(anova(fit1,fit2), booktabs=TRUE, format="markdown")

# print regression coefficients
# The lavaan package provides a set of extractor functions to pull specific portions of the output 
# to further process or analyze. The parameterEstimates(), standardizedSolution(), and fitMeasures() 
# functions can be used to return only the unstandardized estimates, standardized estimates, and model 
# fit statistics, respectively.

# parameterEstimates(fit, standardized=TRUE) %>% 
#   filter(op == "=~") %>% 
#   select('Latent Factor'=lhs, grpicator=rhs, B=est, SE=se, Z=z, 'p-value'=pvalue, Beta=std.all) %>% 
#   knitr::kable(digits = 3, booktabs=TRUE, format="markdown", caption="Factor Loadings")

parameterEstimates(fit2, standardized=TRUE) %>% 
  filter(op == "=~") %>% 
  select('latent_factor'=lhs, grpicator=rhs, beta=std.all) %>% 
  pivot_wider(names_from = latent_factor, values_from = beta)
  
# plot model
# semPaths(fit, residuals=F, nCharNodes = 0, style = "lisrel", rotation = 1)
semPaths(fit2, residuals=F, sizeMan=7, "std", style = "lisrel", 
         rotation = 1, nCharNodes = 0, 
         posCol=c("blue", "red"),
         edge.color="blue",
         edge.label.cex=1.2)

# correlate factor scores
x <- corr.test(predict(fit2))

data.frame(r = round(x$r[1,2],2),
           p = x$p[1,2]) %>% 
  mutate(p = ifelse(p <.001, "<.001", p))

# save factor scores
x <- data.frame(grp_accuracy_cfa = predict(fit2)[,1],
                grp_confidence_cfa = predict(fit2)[,2])

d.grp <- bind_cols(d.grp, x)

# d.grp %>% write_csv("data/210312_dyad_data.csv")
