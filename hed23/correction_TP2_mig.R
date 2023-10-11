library(survival)
library(survminer)
library(survRM2)
library(tidyr)
library(dplyr)
library(jtools)

mig <- read.csv("D:/D/Marc/SMS/FORMATIONS/2023/hed durées/mig.csv")

# variables
is.factor(mig$educ)
is.numeric(mig$q601f)

mig = subset(mig, n_mig==1)
mig$e = ifelse(mig$q601f>0,1,0)

table(mig$e)
table(mig$educ)

# non param
mig$fin = ifelse(mig$e==1,mig$q601f,mig$survey)
mig$dur = mig$fin - mig$q601d + 1

table(mig$dur)

km = survfit(Surv(dur,e)~1, data=mig)
km
plot(km)
ggsurvplot(km, data=mig)

km = survfit(Surv(dur,e)~femme, data=mig)
km
plot(km)
ggsurvplot(km, data=mig)

km = survfit(Surv(dur,e)~senegal, data=mig)
km
plot(km)
ggsurvplot(km, data=mig)


mig$arm=mig$femme
a=rmst2(mig$dur, mig$e, mig$arm)
print(a)
plot(a)


# cox 
mig$educ = as.factor(mig$educ)

fit = coxph(Surv(dur,e) ~  femme + educ + senegal, data=mig)
summary(fit)

cox.zph(fit)
cox.zphold(fit, transform="identity")

# durée discrete

mig$dur2= mig$dur
dt = uncount(mig, dur2)
dt = dt%>% group_by(ident) %>% mutate(t = cumsum(1))
dt$x= 1
dt$t = ave(dt$x,dt$ident, FUN=cumsum)
dt = select(dt, -x)

dt$e2 = ifelse(dt$t<dt$dur,0,dt$e)

fit = glm(e2 ~ t + femme + educ + senegal, data=dt, family="binomial")
summary(fit)


## output console sans star et 4 digits aux pv
# avec package gtools
fit = glm(e2 ~ t + femme + educ + senegal, data=dt, family="binomial")
summ(fit)
summ(fit, digits=3)
summ(fit, digits=3, confint=TRUE, exp=TRUE)



