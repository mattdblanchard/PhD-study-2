
# PCA  --------------------------------------------------------------------
# load packages
packages <- c("tidyverse", "psych")

purrr::map(packages, library, character.only = TRUE)

# load data
d <- read_csv("data/200525_dyad_data.csv")

# CONDUCT PCA -------------------------------------------------------------
# select news sources variables
x <- d %>% select(adr.ind.acc, crt.ind.acc, gk.ind.accO, mdmt.ind.acc, rapm.ind.acc)

# print correlations
corstarsl(x)

# c <- cor(x, use = "pairwise.complete.obs")
c <- corr.test(x)

as.character(round(c$r,2))
data.frame(round(c$r,2))


# make diagonal NA
diag(c$r) <- NA
diag(c$p) <- NA

# identify which cors receive 1 star
paste0(round(c$r,2)[which(c$p <= .05 & c$p >= .01)], "*")
c$r[which(c$p <= .05 & c$p >= .01)]

# identify which cors receive 2 stars
paste0(round(c$r,2)[which(c$p < .01 & c$p >= .001)], "**")
c$r[which(c$p < .01 & c$p >= .001)]

# identify which cors receive 3 stars
c$p[which(c$p < .001)]






# reliability for news source variables
psych::alpha(x)$total$raw_alpha %>% round(2)

# Kaiser-Meyer-Olkin Measure of Sampling Adequacy (KMO)
print(KMO(cor(x, use="complete.obs")))

# Bartlett's test of spherecity
print("Bartletts test of spherecity")
print(data.frame(cortest.bartlett(cor(x, use="complete.obs"), n = 1361)))

# scree plot
scree(x)

# 2-component PCA
n_comp <- 2
rotate_method <- "promax"
score_method <- "Bartlett"

fit <- principal(x, rotate = rotate_method, nfactors = n_comp, 
                 method = score_method, scores = TRUE)

# variance explained
var_table()

# pattern matrix
pattern_matrix()
fit$loadings
# save component scores as dataframe
if (data == "reduced") {
  pca_scores <- data.frame(fit$scores) %>%
    rename(official_sources = RC1, casual_sources = RC2)
  
} else if (any(data == c("full", "marvin"))) {
  pca_scores <- data.frame(fit$scores) %>%
    rename(casual_sources = RC1, official_sources = RC2)
}

# Component correlations matrix
corstarsl(pca_scores)

# add component scores to d
d <- d %>% bind_cols(pca_scores)

