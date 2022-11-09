

library(survival)

library(readr)
trans <- read.csv("https://raw.githubusercontent.com/mthevenin/analyse_duree/master/bases/transplantation.csv")

coxfit1 = coxph(formula = Surv(stime, died) ~ year + age + surgery, data = trans)
coxfit2 = coxph(formula = Surv(mois, died) ~ year + age + surgery, data = trans)

cox.zph(coxfit1, transform="identity")
cox.zphold(coxfit1, transform="identity")
cox.zph(coxfit2)
cox.zph(coxfit3)

trans$x100 = trans$stime/100
trans$x100 = trans$x100+1
trans$x100 = round(trans$x100)

table(trans$x100)

coxfit3 = coxph(formula = Surv(x100, died) ~ year + age + surgery, data = trans)

cox.zph(coxfit3)

library(coxed)
library(dplyr)

mv.data <- dplyr::select(martinvanberg, postel, rgovm, pgovno)
simdata <- sim.survdata(T=100, X=mv.data, num.data.frames = 1, censor=.2)
df = simdata$data
model <- coxph(Surv(y, failed) ~ postel + pgovno + rgovm, data=df)
#summary(model)
cox.zph(model)
df$t100 = df$y/100 + 1
df$t100 = round(df$t100)
model <- coxph(Surv(t100, failed) ~ postel + pgovno + rgovm, data=df)
cox.zph(model)


source("D:/D/Marc/SMS/FORMATIONS/2022/Durée2/a distribuer/cox.zphold.R")

simdata <- sim.survdata(N=1000, T=1000, censor=.2, num.data.frames=1)
df   = simdata$data
df$failed = as.numeric(df$failed)
df$failed = ifelse(df$y>700,df$failed==0,df$failed) 

model <- coxph(Surv(y, failed) ~ X1 + X2 + X3, data=df)
cox.zph(model)
cox.zphold(model)
km= survfit(Surv(y, failed) ~ 1, data=df)
plot(km)

df$t100 = df$y/30 + 1
df$t100 = round(df$t100)
model <- coxph(Surv(t100, failed) ~ X1 + X2 + X3, data=df)
cox.zph(model)
cox.zphold(model)
km= survfit(Surv(t100, failed) ~ 1, data=df)
plot(km)


#########################################
mv.data <- dplyr::select(trans, year, age, surgery)
simdata <- sim.survdata(T=1000, X=mv.data, num.data.frames = 1)
df = simdata$data
model <- coxph(Surv(y, failed) ~ year + age + surgery, data=df)
cox.zph(model)
cox.zphold(model)

df$t100 = df$y/100 + 1
df$t100 = round(df$t100)
model <- coxph(Surv(t100, failed) ~ year + age + surgery, data=df)
cox.zph(model)
cox.zphold(model)








