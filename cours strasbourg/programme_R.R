# maj juin 2022
# version antérieure de cox.zph (=>cox.zphold) préférable pour durées discrètes/groupées

#install.packages("survival")
#install.packages("survminer")
#install.packages("flexsurv")
#install.packages("survRM2")
#install.packages("tidyr")
#install.packages("gtools")
#install.packages("jtools")
#install.packages("RecordLinkage")
#install.packages("cmprsk")
#install.packages("gtsummary")
#install.packages("nnet")
#install.packages("muhaz")
library(survival)
library(survminer)
library(flexsurv)
library(survRM2)
library(gtools)
library(tidyr)
library(jtools)
library(RecordLinkage)
library(cmprsk)
library(gtsummary)
library(nnet)
library(muhaz)

# installation survival v2.44-1 (30 mars 2019) => problème test Grambsch-Therneau
#require(remotes)
#install_version("survival", version = "2.44-1", repos = "http://cran.us.r-project.org")


options(scipen=999)
options(show.signif.stars=FALSE)


# Chargement base
library(readr)
trans <- read.csv("https://raw.githubusercontent.com/mthevenin/analyse_duree/master/bases/transplantation.csv")

# Non parametrique

## km
# syntaxe: fit <- survfit(Surv(time, status) ~ x, data = df)

fit <- survfit(Surv(stime, died) ~ 1, data = trans)
fit
summary(fit)
plot(fit)

ggsurvplot(fit, conf.int = TRUE)
ggsurvplot(fit, conf.int = TRUE, risk.table = TRUE)

# estimation et visualisation de la fonction de risque
# technique de lissage avec de nombreux paramètres. Ici paramètres par défaut.

haz = muhaz(trans$stime,trans$died)
plot(haz)

### comparaison
fit <- survfit(Surv(stime, died) ~ surgery, data = trans)
fit
ggsurvplot(fit, conf.int = TRUE, risk.table = TRUE)

#### test log rank
survdiff(Surv(stime, died) ~ surgery, rho=1, data = trans)

pairwise_survdiff(Surv(stime, died) ~ surgery, rho=1, data = trans) # pas intéressant ici car un dll seulement

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
tbl_regression(coxfit,exponentiate=TRUE)


## test ph attention diff v2 versus v3
### test GB (resultat different cours si v3)

### avec Récupération de l'ancienne version du test: fonction cox.zphold
## lien fichier: https://github.com/mthevenin/analyse_duree/tree/main/cox.zphold
# source("D:/D/Marc/SMS/FORMATIONS/2022/Durée2/a distribuer/cox.zphold.R")
source("https://raw.githubusercontent.com/mthevenin/analyse_duree/master/cox.zphold/cox.zphold.R")
cox.zphold(coxfit)
cox.zphold(coxfit, transform="identity")

### methode ols: correlation residus/t
### plutôt conseillé d'estimer avec une gls (generalized least square => correlation des erreurs de la regression dans le temps)

resid= resid(coxfit, type="scaledsch")
varnames <- names(coxfit$coefficients)
coln = c(varnames)
colnames(resid) = c(coln)

times    = as.numeric(dimnames(resid)[[1]])

resid = data.frame(resid)
resid = cbind(resid, t=times)

year    = summary(lm(year~t, data=resid))
age     = summary(lm(age~t, data=resid))
surgery = summary(lm(surgery~t, data=resid))

###################
#p-values de l'OLS#
###################

paste("p-value pour year:", year$coefficients[2,4])
paste("p-value pour age:",  age$coefficients[2,4])
paste("p-value pour surgery:", surgery$coefficients[2,4])


## interaction

coxfit2 = coxph(formula = Surv(stime, died) ~ year + age + surgery + tt(surgery), data = trans, tt = function(x, t, ...) x*t)
summary(coxfit2)
tbl_regression(coxfit2,exponentiate=TRUE, estimate_fun = purrr::partial(style_ratio, digits = 3))

## Variable dynamique

coxfit = coxph(formula = Surv(stime, died) ~ year + age + surgery + transplant + wait, data = trans)
tbl_regression(coxfit,exponentiate=TRUE,  estimate_fun = purrr::partial(style_ratio, digits = 3))

cox.zphold(coxfit)

cut= unique(trans$stime[trans$died == 1])
tvc = survSplit(data = trans, cut = cut, end = "stime", start = "stime0", event = "died")
tvc$tvc=ifelse(tvc$transplant==1 & tvc$wait<=tvc$stime,1,0)

tvcfit = coxph(formula = Surv(stime0, stime, died) ~ year + age + surgery + tvc, data = tvc)
summary(tvcfit)
tbl_regression(tvcfit,exponentiate=TRUE,  estimate_fun = purrr::partial(style_ratio, digits = 3))

ggforest(tvcfit, data=trans)


# logistique temps discret

## duree continue
library("tidyr")

###
dt = uncount(trans,mois)
dt = dt[order(dt$id),]


dt$x=1
dt$t = ave(dt$x,dt$id, FUN=cumsum)
dt$T = ave(dt$x,dt$id, FUN=sum)


dt$died[dt$t<dt$T]=0


dt$t2=dt$t^2
dtfit = glm(died ~ t + t2 + year + age + surgery, data=dt, family="binomial")
summ(dtfit)

## duree groupée/discrète (le debut vise pour l'exemple à regrouper les durees)

### groupement des durees pour l'exemple
dt$ct4 <- quantcut(dt$t)
table(dt$ct4) 
dt$n = ave(dt$x,dt$id, dt$ct4, FUN=cumsum)
dt$N = ave(dt$x,dt$id, dt$ct4, FUN=sum)
dt2 = subset(dt, n==N)

fit = glm(died ~ ct4 + year + age + surgery, data=dt2, family=binomial)
summ(fit)

# Complements

## modeles parametriques standard (seulement weibull)

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
## fonction multinom de nnet: l'output de l'angoisse (a quand une maj?????)
## utiliser gtsummary

### mise en forme
trans$T = trans$mois
td = uncount(trans, mois)
td$x=1
td$t = ave(td$x, td$id, FUN=cumsum)
td$t2 = td$t*td$t
td$e = ifelse(td$t<td$T,0, td$compet)

### modele
competfit = multinom(formula = e ~ t + t2 + year + age + surgery, data = td)
summary(competfit)
z = summary(competfit)$coefficients/summary(competfit)$standard.errors
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

tbl_regression(competfit)











