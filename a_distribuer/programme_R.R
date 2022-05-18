#install.packages("survival")
#install.packages("survminer")
#install.packages("flexsurv")
#install.packages("survRM2")
#install.packages("tidyr")
#install.packages("gtools")
#install.packages("jtools")
#install.packages("miceadds")
#install.packages("RecordLinkage")
#install.packages("cmprsk")
#install.packages("tibble")
#install.packages("stringr")
#install.packages("gtsummary")
#install.packages("nnet")
library(survival)
library(survminer)
library(flexsurv)
library(survRM2)
library(gtools)
library(tidyr)
library(jtools)
library(RecordLinkage)
library(cmprsk)
library(tibble)
library(stringr)
library(gtsummary)
library(nnet)

# installation survival v2.44-1 (30 mars 2019) => problème test Grambsch-Therneau
#require(remotes)
#install_version("survival", version = "2.44-1", repos = "http://cran.us.r-project.org")

# chargement base
library(readr)
trans <- read_csv("https://raw.githubusercontent.com/mthevenin/analyse_duree/master/bases/transplantation.csv")

# non paramétrique

## km
fit <- survfit(Surv(time, status) ~ x, data = base)

fit <- survfit(Surv(stime, died) ~ 1, data = trans)
fit
summary(fit)
plot(fit)

ggsurvplot(fit, conf.int = TRUE)
ggsurvplot(fit, conf.int = TRUE, risk.table = TRUE)

### comparaison
fit <- survfit(Surv(stime, died) ~ surgery, data = trans)
fit
ggsurvplot(fit, conf.int = TRUE, risk.table = TRUE)

#### test log rank
survdiff(Surv(stime, died) ~ surgery, rho=1, data = trans)

pairwise_survdiff(Surv(stime, died) ~ surgery, rho=1, data = trans) # pas interessant ici car un dll seulement

# rmst
trans$arm=trans$surgery
a=rmst2(trans$stime, trans$died, trans$arm, tau=NULL)
print(a)
plot(a)

# risques proportionnels

# Cox

coxfit = coxph(formula = Surv(stime, died) ~ year + age + surgery, data = trans)
summary(coxfit)
ggforest(coxfit)

## test ph attention diff v2 versus v3

### test GB (résultat différent cours si v3)

cox.zph(coxfit)
cox.zph(coxfit, transform="identity")

### méthode ols: corrélation residus/t

resid= resid(coxfit, type="scaledsch")
resid = data.frame(resid)

resid= rownames_to_column(resid, var = "Z")

resid$t = str_sub(resid$Z, 2, )
resid$t=as.numeric(resid$t)
resid$t=round(resid$t, digits=0)

year    = summary(lm(X1~t, data=resid))
age     = summary(lm(X2~t, data=resid))
surgery = summary(lm(X3~t, data=resid))

year$coefficients[,4] 
age$coefficients[,4] 
surgery$coefficients[,4] 

## intéraction

coxfit2 = coxph(formula = Surv(stime, died) ~ year + age + surgery + tt(surgery), data = trans, tt = function(x, t, ...) x*t)
summary(coxfit2)

## Variable dynamique

cut= unique(trans$stime[trans$died == 1])
tvc = survSplit(data = trans, cut = cut, end = "stime", start = "stime0", event = "died")
tvc$tvc=ifelse(tvc$transplant==1 & tvc$wait<=tvc$stime,1,0)

tvcfit = coxph(formula = Surv(stime0, stime, died) ~ year + age + surgery + tvc, data = tvc)
summary(tvcfit)

ggforest(tvcfit, data=trans)

# logistique temps discret

## durée continue
library("tidyr")

###
dt = uncount(trans,mois)
dt = dt[order(dt$id),]
kable(dt[1:11, ],)

dt$x=1
dt$t = ave(dt$x,dt$id, FUN=cumsum)
dt$T = ave(dt$x,dt$id, FUN=sum)
kable(dt[1:11, ],)

dt$died[dt$t<dt$T]=0
kable(dt[1:11, ],)

dt$t2=dt$t^2
dtfit = glm(died ~ t + t2 + year + age + surgery, data=dt, family="binomial")
summ(dtfit)

## durée groupée/discrète (le début vise pour l'exemple à regrouper les durées)

### groupement des durées pour l'exemple
dt$ct4 <- quantcut(dt$t)
table(dt$ct4) 
dt$n = ave(dt$x,dt$id, dt$ct4, FUN=cumsum)
dt$N = ave(dt$x,dt$id, dt$ct4, FUN=sum)
dt2 = subset(dt, n==N)

fit = glm(died ~ ct4 + year + age + surgery, data=dt2, family=binomial)
summ(fit)

# Compléments

## modèles paramétriques standard (seulement weibull)

### aft
weibull = survreg(formula = Surv(stime, died) ~ year + age + surgery, data = trans, dist="weibull")
summary(weibull)

### ph (avec flexsurv)

library(flexsurv)
flexsurvreg(formula = Surv(stime, died) ~ year + age + surgery, data = trans, dist="weibullPH")

## risques concurrents



table(trans$compet) 
table(trans$died)

## ic
ic = cuminc(trans$stime, trans$compet)
ic 
plot(ic)
ggcompetingrisks(fit = ic)

ic = cuminc(trans$stime, trans$compet, group=trans$surgery, rho=1)
ic 
plot(ic)

ggcompetingrisks(fit = ic)
ggcompetingrisks(fit = ic, multiple_panels = F)

## logit multinomial
## fonction multinom de nnet: l'output de l'angoisse (à quand une maj?????)
## utiliser gtsummary

### mise en forme
trans$T = trans$mois
td = uncount(trans, mois)
td$x=1
td$t = ave(td$x, td$id, FUN=cumsum)
td$t2 = td$t*td$t
td$e = ifelse(td$t<td$T,0, td$compet)

### modèle
competfit = multinom(formula = e ~ t + t2 + year + age + surgery, data = td)
summary(competfit)
z = summary(competfit)$coefficients/summary(competfit)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

tbl_regression(competfit)











