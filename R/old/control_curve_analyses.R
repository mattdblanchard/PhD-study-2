# Install packages
install.packages("lme4",
                 repos=c("http://lme4.r-forge.r-project.org/repos",
                         getOption("repos")[["CRAN"]]))
install.packages("lazyeval", lib = "~/R/R-3.2.2/library")
install.packages("lmerTest")
install.packages("plotrix")

#Load packages

library(lme4)
library(lmerTest)
library(grid)
library(lattice)
library(plotrix)

#Read data
wdir<-getwd()
# change \ to / in the path
setwd("R:/FML - From memorizing to learning/FML4 - with-without TP for PSRuTP/Results/For JOL_ST with monika/R Analyses")
ExpRak.r<-read.table("FML4_raw_results for Monika.csv",header=T,sep=",")


#Compute variables
ExpRak.r<-cbind(ExpRak.r,CondD1=ifelse(ExpRak.r$condition=="AT",1,0),CondD2=ifelse(ExpRak.r$condition=="TP",1,0))
ExpRak.r<-cbind(ExpRak.r,JOLp=ExpRak.r$JOL/100)
ExpRak.r<-cbind(ExpRak.r,JOLpc=scale(ExpRak.r$JOLp,center=TRUE,scale=FALSE))
ExpRak.r<-cbind(ExpRak.r,JOLp2c=ExpRak.r$JOLpc^2)
ExpRak.r<-cbind(ExpRak.r,logST=log(ExpRak.r$ST))

#Regression for standard condition
temp<- subset(ExpRak.r, condition == "FT")
#model0: Random intercepts for subjects and items 
model0.FT<-lmer(logST ~ 1 + JOLpc + JOLp2c + (1 | subject_id) + (1 | item), REML = F, data = temp)

#model1: Random intercepts for subjects and items PLUS variance in slope and curvilinearity across subjects
model1.FT<-lmer(logST ~ 1 + JOLpc + JOLp2c + (1 | subject_id) + (1 | JOLpc) + (1 | JOLp2c) + (1 | item), REML = F, data = temp)

#tests whether model1 fits better than model0 => Chi-square test
anova(model0.FT,model1.FT)

#prints out results from model0
summary(model0.FT)
coef(model0.FT)$subject_id
ranef(model0.FT)$subject_id

summary(model1.FT)
coef(model1.FT)$subject_id
ranef(model1.FT)$subject_id


#Regression for time pressure condition

temp<- subset(ExpRak.r, condition == "TP")

model0.TP<-lmer(logST ~ 1 + JOLpc + JOLp2c + (1 | subject_id) + (1 | item), REML = F, data = temp)
model1.TP<-lmer(logST ~ 1 + JOLpc + JOLp2c + (1 | subject_id) + (1 | JOLpc) + (1 | JOLp2c) + (1 | item), REML = F, data = temp)

anova(model0.TP,model1.TP)
# ??? ask Monika: The difference is significant, indicating that model1 is better - so why don't we use Model1 for the summary?

summary(model0.TP)



#Both conditions in one regression analyis

temp<-subset(ExpRak.r, condition != "AT")

model0<-lmer(logST ~ 1 + JOLpc + JOLp2c + CondD2 + JOLpc*CondD2 + JOLp2c*CondD2 + (1 | subject_id) + (1 | item), REML = F, data = temp)
model1<-lmer(logST ~ 1 + JOLpc + JOLp2c + CondD2 + JOLpc*CondD2 + JOLp2c*CondD2+ (1 | subject_id) + (1 | JOLpc) + (1 | JOLp2c) + (1 | item), REML = F, data = temp)

anova(model0,model1)

summary(model0)