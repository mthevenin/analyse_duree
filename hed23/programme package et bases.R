
# normalement survival est maintenant installé avec Rsurv (à vérifier)
#install.packages("survival")


install.packages("survminer")
install.packages("survRM2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("jtools")
install.packages("gtools")
install.packages("muhaz")
install.package("gtsummary")

# library(survival)
library(survminer)

library(survRM2)
library(tidyr)
library(dplyr)
library(jtools)
library(gtools)
library(cmprsk)
library(muhaz)
library(gtsummary)

# Pour récupérer le test ols de Grambsch-Therneau, malheureusement supprimé avec la v3 de survival
# Le fichier de la fonction est également dans l'archive zip distribuée.
source("https://raw.githubusercontent.com/mthevenin/analyse_duree/main/cox.zphold/cox.zphold.R")


# Chargement des bases via github
library(readr)
## Cours
trans <- read.csv("https://raw.githubusercontent.com/mthevenin/analyse_duree/master/bases/transplantation.csv")

## TP (à partir de mercredi)
activite <- read.csv("https://raw.githubusercontent.com/mthevenin/analyse_duree/master/hed23/activite.csv")
reprise  <- read.csv("https://raw.githubusercontent.com/mthevenin/analyse_duree/master/hed23/reprise.csv")
mig <- read.csv("https://raw.githubusercontent.com/mthevenin/analyse_duree/master/hed23/mig.csv")










