library(lme4)
library(emmeans)
library(glmmTMB)
library(ggplot2)
#------#
library(readr)
RLBr_20 <- read_csv("RSTUDIO/RLBr_20.csv")
View(RLBr_20)
#
overdisp_fun <- function(model){ 
rdf <- df.residual(model)
rp <- residuals(model,type="pearson")
prat<-Pearson.chi/rfd
pval<-pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
c(chisq=Pearson.chisq,ration=prat,rdf=rdf,p=pval)}
#
wtnb<-read.csv("RSTUDIO/RLBr_20.csv", header=TRUE)
#
shapiro.test(RLBr_20$wtnb)
wtnb1<-cbind(RLBr_20, log(RLBr_20$wtnb))
hist(wtnb1$"log(RLBr_20$wtnb)")
#
kruskal.test(wtnb ~ week, data=RLBr_20)
fligner.test(wtnb ~ week, data=RLBr_20)
#
data1<-glm(wtnb ~ week, data=RLBr_20)
summary(data1)
data2<-glm(wtb ~ week, data=RLBr_20)
summary(RLBr_20)  
data2<
#
data3<-
#
boxplot.stats(wtnb~ week)
TypeIII <-drop1(model,~., test="F")# to get type III errors
##Load packages Car and Multcomp viewer and lsmeans##
model <- lm(sperm ~ trt, data=con)
lsm <- lsmeans(model, "trt", adjust="tukey")
cld(lsm, alpha=0.05, Letters=letters)
