# calculate control thresholds 
# Logisitic regression: use confidence to predict the point at which a bet decision switches from no to yes
# Great tutorial for conductin logistic regression: https://www.datacamp.com/community/tutorials/logistic-regression-R
post_vars <- adr %>% 
  ungroup() %>% 
  select(uid, Ind_Confidence.RESP, Grp_Confidence.RESP, Ind_Decision.RESP, Grp_Decision.RESP) %>% 
  mutate(
    # Ind_Decision.RESP = ifelse(Ind_Decision.RESP == "y", 1,
    #                             ifelse(Ind_Decision.RESP == "n", 0, Ind_Decision.RESP)),
    # Grp_Decision.RESP = ifelse(Grp_Decision.RESP == "y", 1,
    #                             ifelse(Grp_Decision.RESP == "n", 0, Grp_Decision.RESP)),
    Ind_Decision.RESP = as.factor(Ind_Decision.RESP),
    Grp_Decision.RESP = as.factor(Grp_Decision.RESP)) %>%
  na.omit()

# visualise the magnitude of the relationship between confidence and betting
library(corrplot)
correlations <- cor(na.omit(post_vars[,c(2:5)]))
corrplot(correlations, method="circle")

library(caret)
x <- post_vars %>% select(Ind_Confidence.RESP)
y <- post_vars %>% select(Ind_Decision.RESP)
scales <- list(x=list(relation="free"), y=list(relation="free"))

featurePlot(x=x, y=y, plot="density", scales=scales)

fit <- glm(Ind_Decision.RESP ~ Ind_Confidence.RESP, data = post_vars, family = binomial)

summary(fit)

glm.probs <- predict(fit,type = "response")
glm.probs[1:5]

glm.pred <- ifelse(glm.probs > 0.5, "y", "n")

# inspect correct predictions
attach(post_vars)
table(glm.pred,Ind_Decision.RESP)

# lets calculate the mean % of correct predictions = 86%
mean(glm.pred == Ind_Decision.RESP)
library(broom)

post_data <- adr %>%
  ungroup() %>% 
  select(uid, Ind_Confidence.RESP, Ind_Decision.RESP) %>%
  mutate(Ind_Decision.RESP = as.factor(Ind_Decision.RESP)) %>% 
  group_by(uid) %>% 
  mutate(dec_sd = sd(Ind_Decision.RESP)) %>% 
  filter(dec_sd != 0) %>% 
  select(-dec_sd) %>% 
  group_by(uid) %>% 
  nest() %>% 
  mutate(fit = map(data, ~ glm(Ind_Decision.RESP ~ Ind_Confidence.RESP, data = ., family = binomial)),
         coef = map(fit, ~ tidy(.)$estimate),
         coef = map(coef, ~ data.frame(a = .[[1]], b = .[2]))) %>% 
  unnest(coef)

post <- post_data %>% 
  group_by(uid) %>% 
  summarise(post = (-1*a)/b) %>% 
  filter(post >= 0 & post <= 100)


log_data %>% filter(uid == "17040610_1_d2") %>% summarise(post = -a/b)

adr %>% filter(uid == "19013110_1_g1") %>% select(Grp_Confidence.RESP, Ind_Decision.RESP)