# maj novembre 2023
# version antérieure de cox.zph (=>cox.zphold) préférable pour durées discrètes/groupées

#install.packages("survival")
#install.packages("ggsurvfit")
#install.packages("flexsurv")
#install.packages("survRM2")
#install.packages("tidyr")
#install.packages("gtools")
#install.packages("jtools")
#install.packages("RecordLinkage")
#install.packages("cmprsk")
#install.packages("tibble")
#install.packages("stringr")
#install.packages("gtsummary")
#install.packages("nnet")
#install.packages("muhaz")
#install.packages("survminer")
library(survival)
library(survminer)
library(ggsurvfit)
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
library(muhaz)


options(show.signif.stars=FALSE) # suppression etoile significativité
options(scipen=999) # suppression format scientifique pour proba

# installation survival v2.44-1 (30 mars 2019) => problème test Grambsch-Therneau
#require(remotes)
#install_version("survival", version = "2.44-1", repos = "http://cran.us.r-project.org")

# Chargement base
library(readr)
trans <- read_csv("https://raw.githubusercontent.com/mthevenin/analyse_duree/master/bases/transplantation.csv")

# Non parametrique

## km
# syntaxe: fit <- survfit(Surv(time, status) ~ x, data = df)

fit <- survfit(Surv(stime, died) ~ 1, data = trans)
fit
summary(fit)
plot(fit)

ggsurvfit(fit) 

ggsurvfit(fit) +
  add_confidence_interval() +  
  add_risktable()


# estimation et visualisation de la fonction de risque
# technique de lissage avec de nombreux paramètres. Ici paramètres par défaut.

haz = muhaz(trans$stime,trans$died)
plot(haz)

### comparaison
fit <- survfit(Surv(stime, died) ~ surgery, data = trans)
fit

ggsurvfit(fit) +
  add_confidence_interval() +  
  add_risktable()

#### test log rank
survdiff(Surv(stime, died) ~ surgery, rho=1, data = trans)

pairwise_survdiff(Surv(stime, died) ~ surgery, rho=1, data = trans) # pas intéressant ici car un dll seulement

# rmst
trans$arm=trans$surgery
a=rmst2(trans$stime, trans$died, trans$arm, tau=NULL)
print(a)
plot(a)

# Risques proportionnels

# Cox
coxfit = coxph(formula = Surv(stime, died) ~ year + age + surgery, data = trans)
summary(coxfit)
ggforest(coxfit)
tbl_regression(coxfit,exponentiate=TRUE)

### Test ph attention diff v2 versus v3

# Test GB (Résultat different si v3 car test "exact" => gls avec variances calculées à chaque t sur les résidus)

cox.zph(coxfit)
cox.zph(coxfit, transform="identity")

### avec Récupération de l'ancienne version du test: fonction cox.zphold
## lien fichier: https://github.com/mthevenin/analyse_duree/tree/main/cox.zphold
## je conseille de rester sur cette solution car reproductible avec autres applis Stats + test GLS pas adapté avec durées groupées + pas de test miracle (voir 
## le bouquin de Grambsch-Therneau)
source("D:/D/Marc/SMS/FORMATIONS/2022/Durée2/a distribuer/cox.zphold.R")
cox.zphold(coxfit)
cox.zphold(coxfit, transform="identity")

### pour info estimation directe ols: correlation residus/t =>  cox.zphold avec transform="identity"

resid= resid(coxfit, type="scaledsch")
varnames <- names(coxfit$coefficients)
coln = c(varnames)
colnames(resid) = c(coln)

times = as.numeric(dimnames(resid)[[1]])

resid = data.frame(resid)
resid = cbind(resid, t=times)

year    = summary(lm(year~t, data=resid))
age     = summary(lm(age~t, data=resid))
surgery = summary(lm(surgery~t, data=resid))

##########
#p-values#
##########

year$coefficients[,4] 
age$coefficients[,4] 
surgery$coefficients[,4] 

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

##  fonction durée traitée comme variable quanti
library("tidyr")

## mise en forme base
dt = uncount(trans,mois)
dt = dt[order(dt$id),]

## variables durées
dt$x=1
dt$t = ave(dt$x,dt$id, FUN=cumsum)
dt$T = ave(dt$x,dt$id, FUN=sum)

## centrage des x quanti pour interprétation de la constante, sinon x=0

dt$myear = mean(dt$year)
dt$mage = mean(dt$age)

dt$yearb = dt$year - mean(dt$year)
dt$ageb = dt$age - mean(dt$age)

## variable censure
dt$died[dt$t<dt$T]=0


dt$t2=dt$t^2
dt$t3=dt$t^3
dtfit = glm(died ~ t + t2 + t3 + yearb + ageb + surgery, data=dt, family="binomial")
summ(dtfit, digits=3, exp=TRUE)


### si on compare avec un modèle de cox avec durée = variables mois

coxfit = coxph(formula = Surv(mois, died) ~ year + age + surgery, data = trans)
summary(coxfit)


## duree groupée/discrète (le debut vise pour l'exemple à regrouper les durees)

### groupement des durees pour l'exemple (quartiles)
dt$ct4 <- quantcut(dt$t)
table(dt$ct4) 
dt$n = ave(dt$x,dt$id, dt$ct4, FUN=cumsum)
dt$N = ave(dt$x,dt$id, dt$ct4, FUN=sum)
dt2 = subset(dt, n==N)

fit = glm(died ~ ct4 + yearb + ageb + surgery, data=dt2, family=binomial)
summ(fit, exp=TRUE, digits=3)

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











