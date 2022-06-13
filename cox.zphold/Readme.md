Programme du test de la V1-V2 de survival. 
Visiblement le test "exact" est (très) sensible aux évènements simultanés/groupés, classiques en sciences sociale. Il me semble préférable d'exécuter ce test "simplifié"
qui est par ailleurs la solution adoptée par les autres logiciels: Sas - Stata - Python ("lifelines" et "statmodels).    

***Marche à suivre***:  

* Télécharger le programme **cox.zphold.R**
* Charger la fonction **`cox.zphold()`** avec: **`source("//...path.../cox.zphold.R")`**
* Exécuter un modèle de Cox avec la fonction **`fit=coxph(...)`**
* Exécuté le test avec **`cox.zphold(fit)`**


