Programme du test de la V1-V2 de survival. 
Visiblement le test "exact" est (très) sensible aux évènements simultanés/groupés, classiques en sciences sociales. 
Il me semble préférable d'exécuter ce test "simplifié" qui est par ailleurs la solution adoptée par les autres logiciels: Sas - Stata - Python ("lifelines" ou "statmodels).  

Ici je n'ai rien fait  de plus que renommer la fonction de la version antérieure   

***Marche à suivre***:  

* Télécharger le programme **cox.zphold.R**
* Charger la fonction **`cox.zphold()`** avec: **`source("//...path.../cox.zphold.R")`**
* Exécuter un modèle de Cox avec la fonction **`fit=coxph(...)`**
* Exécuter le test avec **`cox.zphold(fit)`**


* Package **`survival`**: https://github.com/therneau/survival
  * Lien dépôt: https://github.com/therneau/survival
  * Description et licence: https://github.com/therneau/survival/blob/master/DESCRIPTION







