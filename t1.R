
library(survival)
library(survminer)
library(survRM2)
library(tidyr)
library(gtools)
library(jtools)
library(RecordLinkage)
library(cmprsk)
library(nnet)
library(gtsummary)
library(nnet)
library(muhaz)


options(scipen=999) 
options(show.signif.stars=FALSE)

# création variables d'analyse

act <- read.csv("D:/D/Marc/SMS/FORMATIONS/2023/strasbourg durées/correction TP/activite.csv")

act$d = ifelse(act$ageinact>0, 1, 0)

table(act$d)

act$fin = ifelse(act$d==1, act$ageinact, ifelse(act$ageret>0, act$ageret, act$age_enq))
act$dur = act$fin - act$ageact + 1 

table(act$d)
summary(act$dur)

## KM

km = survfit(Surv(dur,d)~1, data=act)
summary(km) 
km
plot(km)
ggsurvplot(km, risk.table=TRUE, ggtheme=theme_light(),)

km = survfit(Surv(dur,d)~diplome, data=act)

summary(km) 
km
plot(km)
ggsurvplot(km, risk.table=TRUE, ggtheme=theme_light(),)

# fonction risque
haz = muhaz(act$dur,act$d)
plot(haz)


km = survfit(Surv(dur,d)~csp, data=act)
# summary(km) 
km
plot(km)
ggsurvplot(km, risk.table=TRUE, ggtheme=theme_light(),)

km = survfit(Surv(dur,d)~gene, data=act)

km = survfit(Surv(dur,d)~diplome, data=act)
ggsurvplot(km, risk.table=F, ggtheme=theme_light(),)

# summary(km) 
km
plot(km)
ggsurvplot(km, risk.table=TRUE, ggtheme=theme_light(),)


## Test du log-rank (niveau de diplome)

survdiff(Surv(dur,d)~diplome,data=act, rho=0)
survdiff(Surv(dur,d)~diplome,data=act, rho=1)

pairwise_survdiff(Surv(dur,d)~diplome,data=act, rho=1, p.adjust.method = "none")
pairwise_survdiff(Surv(dur,d)~diplome,data=act, rho=1)


## Comparaison des RMST (niveau de diplome)


# dipl1 versus dipl2
rmst12=act[act$diplome!=3,]
rmst12$arm=ifelse(rmst12$diplome==2,1,0)
a=rmst2(rmst12$dur, rmst12$d, rmst12$arm)
print(a)
plot(a)

# dipl1 versus dipl3
rmst13=act[act$diplome!=2,]
rmst13$arm=ifelse(rmst13$diplome==3,1,0)
a=rmst2(rmst13$dur, rmst13$d, rmst13$arm)
print(a)
plot(a)


# dipl2 versus dipl3
rmst23=act[act$diplome!=1,]
rmst23$arm=ifelse(rmst23$diplome==3,1,0)
a=rmst2(rmst23$dur, rmst23$d, rmst23$arm)
print(a)
plot(a)

# Analyse semi-paramétrique: modèle de cox

act$gene    = as.factor(act$gene)
act$gene    = relevel(act$gene, ref = "2")
act$csp     = as.factor(act$csp)
act$csp     = relevel(act$csp,  ref = "Cadres")
act$diplome = as.factor(act$diplome)
act$diplome = relevel(act$diplome,  ref = "2")

coxfit = coxph(Surv(dur,d) ~ gene + csp + diplome, data=act)
summary(coxfit)

ggforest(coxfit)

## Test de l'hypothese de risques proportionnels

# Attention nouveau test et nouveau resultats depuis la v3 de survival: 
# pas comparable avec les autres logiciels 
# test v3 est estimé avec une GLS, pas adapté aux durées groupées. Donc récupérer et effectuer le test avec la fonction précédente 

###Juin 2022:  avec recuperation de l'ancienne version du test: fonction cox.zphold
## lien fichier: https://github.com/mthevenin/analyse_duree/tree/main/cox.zphold

# Charger directement la fonction
source("https://raw.githubusercontent.com/mthevenin/analyse_duree/master/cox.zphold/cox.zphold.R")

# chargement en local
#source("D:/D/Marc/SMS/FORMATIONS/2022/Durée2/a distribuer/cox.zphold.R")
# par defaut g(t) = 1-km. Ici je prend la fonction linéaire pour me caler sur l'intéraction

cox.zphold(coxfit)
cox.zphold(coxfit, transform="identity")

# Avec  survival v3 (attention test différent
# Nouvelle option terms pour specifier si on veut un test pour pour chaque de degre de liberte 
# (un test par indicatrice par exemple) ou sous test multiple pour chaque variable categorielle: 
# diplome => un test par niveau de diplome
# ou un test a 3 ddl pour la variable diplome. Preferer la premiere solution.

# v3 survival (je ne conseille pas en durées discretes/groupées)
cox.zph(coxfit, terms=FALSE )
cox.zph(coxfit, terms=FALSE, transform="identity")

# Interaction
# On passe la variable diplome en indicatrice (ne pas oublier qu'avec les courbes Kaplan-Meier, les courbes
# se croisent rapidement)
act$dipl1 = ifelse(act$diplome==1, 1,0)
# act$dipl2 = ifelse(act$diplome==2, 1,0). Pas necessaire car c'est la reference choisie
act$dipl3 = ifelse(act$diplome==3, 1,0)

coxfit2 = coxph(Surv(dur, d) ~ gene + csp + dipl1 + dipl3 + tt(dipl3), data = act, tt = function(x, t, ...) x*t)
summary(coxfit2)


## Introduction d'une variable dynamique 

# cut permet de recuperer les valeurs de t avec au moins un evenement (ici tous t compris entre 2 et 44)
cut= unique(act$dur[act$d == 1])

# survsplit va modifier le format de la base avec pour chaque individu une ligne par age d'observation
# gros avantage avec Sas et Stata, la variable évenement est bien construite: valeur toujours égale a 
# avec l'age a l'evenement.

tvc = survSplit(data = act, cut = cut, end = "dur", start = "dur0", event = "d")

# On vérifie qu'on obtient les mêmess résultats que le modèle d'origine
coxfit = coxph(Surv(dur0, dur, d) ~ gene + csp + diplome, data = tvc)
summary(coxfit)

### Construction de la TVC

# on doit recuperer l'age de la personne pour chaque annee d'observation pour le comparer a l'age
# a la naissance
# une fois la variable creer toujours penser a checker si c'est ok 

tvc$age= tvc$ageact + tvc$dur0
tvc$tvc = tvc$enf
tvc$tvc = ifelse(tvc$tvc==1 & tvc$age>=tvc$aanenf,1,0)
head(tvc, n=12)

# estimation du modele
coxfit3 = coxph(Surv(dur0, dur, d) ~ gene + csp + diplome + tvc, data = tvc)
summary(coxfit3)

# Modele (logistique) à temps discret  
# HORS COURS POUR INFO (tp de la formation INED et HED)

## Transformation de la base
## Remarque: on pourrait utiliser survsplit => cela engendrera des ecarts tres legers avec une
## specification en continue. Si la duree est utilisee de maniere discrete, cela evite de faire 
## des regroupement lorsqu'ils n'y a pas d'evenement (ici en t=1 et t>44)

# Au lieu d'utiliser average, il y a egalement des solutions en dplyr (je suis toujours un peu old school là dessus) 

act$dur2 = act$dur
act$x=1
td = uncount(data=act,dur)
head(td, n=13)
td$t = ave(td$x,td$ident, FUN=cumsum)
head(td, n=13)
td$d[td$t<td$dur]=0
head(td, n=13)

## durée continue/quantitative
# polynome ordre 3: cela ajuste bien en apparence mais présence outliers
# en fin d'observation, la probabilité conditionnelle prédite est surestimée (pour info)
# méthode par les splines (plus compliqué) serait préférable

td$t2 = td$t^2
td$t3 = td$t^3

fit1 = glm(d ~ t, data=td, family="binomial")
summ(fit1)
fit2 = glm(d ~ t + t2, data=td, family="binomial")
summ(fit2)
fit3 = glm(d ~ t + t2 + t3, data=td, family="binomial")
summ(fit3)

fit = glm(d ~ t + t2 + t3 + gene + csp + diplome, data=td, family="binomial")
summ(fit, digits=4, exp=TRUE)


### Risques concurrents (si traité)

# IC et test de gray

library(survminer)

act$typinact = as.factor(act$typinact)
ic           = cuminc(act$dur, act$typinact)
ic
plot(ic)
ggcompetingrisks(fit = ic)


ic = cuminc(act$dur, act$typinact, group=act$diplome, rho=1)
ic
plot(ic)
ggcompetingrisks(fit = ic)
ggcompetingrisks(fit = ic, multiple_panels = F)


# Modele multinomial

td = act

td$T = td$dur
td = uncount(td,T)  

td$gene = as.factor(td$gene)
td$gene =  relevel(td$gene, ref = "2")
td$csp  = as.factor(td$csp)
td$csp  =  relevel(td$csp,  ref = "Cadres")
td$diplome  = as.factor(td$diplome)
td$diplome =  relevel(td$diplome,  ref = 2)
#td$typinact = as.factor(td$typinact)
#td$typinact =  relevel(td$typinact, ref = "0")

td$x=1
td$t = ave(td$x, td$ident, FUN=cumsum)
td$t2 = td$t*td$t

# visiblement faire attention ici que typinact ne soit pas en facteur mais en caractere, 
# sinon probleme avec ifelse, on se retrouve avec 3 causes et non deux (pas vraiment compris pourquoi)


td$e = td$typinact
td$e = ifelse(td$t<td$dur,0, td$e)

competfit = multinom(formula = e ~ t  + gene + csp + diplome, data = td)
competfit

tbl_regression(competfit, exponentiate = TRUE)


# Mise en forme de la base

mef <- read.csv("D:/D/Marc/SMS/FORMATIONS/2022/Durée2/a distribuer/mef.csv")
head(mef, n=29)

mef$act2 = ifelse(mef$act == 1, 1 , 0)
mef$tact = ave(mef$act2, mef$ident, FUN=cumsum)
mef = subset(mef, mef$tact>0)

mef$tact = NULL

mef$inact = ifelse(mef$act != 1, 1 , 0)
mef$tinact = ave(mef$inact,  mef$ident, FUN=cumsum)
mef$ttinact = ave(mef$tinact, mef$ident, FUN=cumsum)
head(mef, n=29)

mef$ageret =   ifelse(mef$ttinact==1 & mef$act==7, mef$ageact,0) 
mef$ageinact = ifelse(mef$ttinact==1 & mef$act!=7, mef$ageact,0) 
head(mef, n=29)

mef$ageret   = ave(mef$ageret,   mef$ident, FUN=sum)
mef$ageinact = ave(mef$ageinact, mef$ident, FUN=sum)
head(mef, n=29)

mef$x=1
mef$n = ave(mef$x, mef$ident, FUN=cumsum)
mef = subset(mef, mef$n==1)
mef = mef[c("ident","age_enq","ageact","act","ageret","ageinact")]
head(mef)





