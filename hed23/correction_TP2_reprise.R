library(survival)
library(survminer)
library(muhaz)
library(survRM2)
library(tidyr)
library(dplyr)
library(jtools)

options(scipen=999)
options(show.signif.stars=FALSE)

rep <- read.csv("D:/D/Marc/SMS/FORMATIONS/2023/hed durées/reprise.csv")
rep$d   = ifelse(rep$ageact==0,0,1)
rep$fin = ifelse(rep$d==1, rep$ageact, rep$age_enq)
rep$dur = rep$fin - rep$ageinact + 1

table(rep$d)

km = survfit(Surv(dur,d) ~ 1, data=rep)
km
summary(km)
plot(km)

h = muhaz(rep$dur, rep$d, bw.method="g", bw.grid=5)
plot(h)

km = survfit(Surv(dur,d) ~ typinact, data=rep)
km
ggsurvplot(km, data=rep)

km = survfit(Surv(dur,d) ~ bacplus, data=rep)
km
ggsurvplot(km, data=rep)


survdiff(Surv(dur,d) ~ bacplus, data=rep, rho=0)
survdiff(Surv(dur,d) ~ bacplus, data=rep, rho=1)


rep$arm = ifelse(rep$typinact=="Autres",0,1)
rmst2(rep$dur,rep$d,rep$arm)

rep$arm = ifelse(rep$typinact=="Autres",0,1)
rmst2(rep$dur,rep$d,rep$arm, tau=10)

rep$typinact=as.factor(rep$typinact)

coxfit = coxph(Surv(dur,d) ~  gene4550 +  bacplus +  typinact + ageinact, data=rep)
summary(coxfit)

source("https://raw.githubusercontent.com/mthevenin/analyse_duree/main/cox.zphold/cox.zphold.R")

cox.zph(coxfit,    transform="identity", terms=FALSE)
cox.zphold(coxfit, transform="identity")

### Empêcher le report en format scientifique de la pv du test GT (sans utiliser l'option scipen)
### avec 4 décimales
test = cox.zphold(coxfit)$table
as.data.frame(apply(test,2, formatC, format="f", digits=4))


# durée discrete

rep$dur2= rep$dur
dt = uncount(rep, dur2)
dt = dt%>% group_by(ident) %>% mutate(t = cumsum(1))
dt$x= 1
dt$t = ave(dt$x,dt$ident, FUN=cumsum)
dt = select(dt, -x)

dt$d2 = ifelse(dt$t<dt$dur,0,dt$d)

fit = glm(d2 ~ t + gene4550 +  bacplus +  typinact + ageinact, data=dt, family="binomial")
summary(fit)


## output console sans star et 4 digits aux pv

# avec package gtools
fit = glm(d2 ~ t + gene4550 +  bacplus +  typinact + ageinact, data=dt, family="binomial")
summ(fit)
summ(fit, digits=3)
summ(fit, digits=3, confint=TRUE, exp=TRUE)








