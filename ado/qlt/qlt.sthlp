.-
Aide pour ^qlt^: Calcul des durées pour plusieurs quantiles de la fonction de survie
estimée à partir de la ^méthode actuarielle^

.-

^Syntaxe^
---------
^qlt^ [if/in] [, sas]


^Description^
------------
- Calcul des durées pour les quantiles de la fonction de survie (90% - 75% - 50% - 25% - 10%)
estimée avec la méthode actuarielle.
- La commande ltable ne permettant pas de récupérer directement les estimations de la fonction
  de survie, elles doivent etres enregistrées dans une base avec l'option ^saving()^
- La definition des bornes étant par défaut differente entre Stata et Sas, les valeurs des quantiles pour chaque definition
  est possible. On peut afficher la valeur des estimations de la fonctions de séjours pour cette définition des bornes avec la commande ^list^
  Stata: t(i) <= t <  t(i+1)
  Sas  : t(i) <  t <= t(i+1)

^Option^
-------

^sas^: définition des bornes alternatives. Si on selectionne une strate avec if, on doit réouvrir la base enregistrée avec ltable
     pour 
 

^Exemples^
---------

Ensemble des observations
-------------------------
^webuse rat^
^quiet ltable t died, saving(lt)^
^use lt^
^qlt^
Durée pour différents quantiles de la fonction de survie
S(t)=0.90: t=  163.000
S(t)=0.75: t=  198.000
S(t)=0.50: t=  231.456
S(t)=0.25: t=  254.567
S(t)=0.10: t=  293.028


Pour chaque valeur de la variable group
---------------------------------------
^webuse rat^
^ltable t died, saving(lt,replace) by(group)^
^use lt, clear^
^qlt if group==1^
Durée pour differents quantiles de la fonction de survie
Définition des bornes Stata-ltable
S(t)=0.90: t=  161.900
S(t)=0.75: t=  189.500
S(t)=0.50: t=  214.425
S(t)=0.25: t=  233.012
S(t)=0.10: t=  259.792

^qlt if group==2^
Durée pour differents quantiles de la fonction de survie
Définition des bornes Stata-ltable
S(t)=0.90: t=  156.700
S(t)=0.75: t=  207.382
S(t)=0.50: t=  232.779
S(t)=0.25: t=  271.059
S(t)=0.10: t=  296.635

Définition alternative des bornes (Sas)
---------------------------------------

^qlt if group==1, sas^
Durée pour différents quantiles de la fonction de survie
Définition des bornes Sas-lifetest
S(t)=0.90: t=  185.600
S(t)=0.75: t=  191.500
S(t)=0.50: t=  217.900
S(t)=0.25: t=  241.529
S(t)=0.10: t=  293.309

^list^

     +---------------------------------------------------------------+
     | group    t0    t1   survival   sesurv~l   lsurvi~l   usurvi~l |
     |---------------------------------------------------------------|
  1. |     1   143   144          1          .          .          . |
  2. |     1   164   165   .9473684   1.000122   .6811868   .9924147 |
  3. |     1   188   189   .8947368   .7074713   .6407943   .9725854 |
  4. |     1   190   191   .7894737    .501165   .5319127   .9152861 |
  5. |     1   192   193   .7368421   .4489534   .4789329   .8810194 |
     |---------------------------------------------------------------|
  6. |     1   206   207   .6842105   .4107024   .4279406   .8439419 |
  7. |     1   209   210   .6315789   .3812989   .3789929   .8044088 |
  8. |     1   213   214   .5789474   .3579703   .3320811   .7626399 |
  9. |     1   216   217   .5263158   .3390848   .2872013   .7187638 |
 10. |     1   220   221   .4709141   .3245911   .2410465   .6712523 |
     |---------------------------------------------------------------|
 11. |     1   227   228   .4120499   .3142131   .1937253   .6194443 |
 12. |     1   230   231   .3531856   .3059908   .1501859   .5647705 |
 13. |     1   234   235   .2943213   .3001319   .1105193   .5070339 |
 14. |     1   244   245   .2354571   .2972067   .0750561   .4458795 |
 15. |     1   246   247   .2354571   .2972067   .0750561   .4458795 |
     |---------------------------------------------------------------|
 16. |     1   265   266   .1569714   .3201429   .0311804   .3720617 |
 17. |     1   304   305   .0784857   .3625863   .0056304   .2864089 |
 18. |     1   305     .          0   .3625863          .          . |
     +---------------------------------------------------------------+

^use lt, clear^

^qlt if group==2, sas^
Durée pour différents quantiles de la fonction de survie
Définition des bornes Sas-lifetest
S(t)=0.90: t=  166.500
S(t)=0.75: t=  232.088
S(t)=0.50: t=  237.676
S(t)=0.25: t=  288.471
S(t)=0.10: t=  323.494
r; t=0.05 16:09:31

^list^

     +---------------------------------------------------------------+
     | group    t0    t1   survival   sesurv~l   lsurvi~l   usurvi~l |
     |---------------------------------------------------------------|
  1. |     2   142   143          1          .          .          . |
  2. |     2   156   157    .952381   1.000099   .7072068   .9931521 |
  3. |     2   163   164   .9047619   .7074019   .6700459   .9752942 |
  4. |     2   198   199   .8571429    .577922    .619718   .9515517 |
  5. |     2   204   205   .8095238   .5009308   .5689051   .9238889 |
     |---------------------------------------------------------------|
  6. |     2   205   206   .8095238   .5009308   .5689051   .9238889 |
  7. |     2   232   233   .7589286   .4494526   .5139409    .891979 |
  8. |     2   233   234   .6577381   .3821141    .412316   .8202803 |
  9. |     2   239   240   .4553571   .3108943   .2353067   .6519933 |
 10. |     2   240   241   .4047619   .3001645   .1961499   .6051905 |
     |---------------------------------------------------------------|
 11. |     2   261   262   .3541667   .2915174   .1591443    .556433 |
 12. |     2   280   281   .3035714   .2849194   .1244597   .5055901 |
 13. |     2   296   297    .202381   .2790197   .0632669    .396679 |
 14. |     2   323   324   .1011905   .2924168   .0171915   .2748764 |
 15. |     2   344   345   .0505952   .3264236   .0034907   .2072695 |
     |---------------------------------------------------------------|
 16. |     2   345     .   .0505952   .3264236   .0034907   .2072695 |
     +---------------------------------------------------------------+


.-
Auteur: ^Marc Thevenin^ - ^Ined-Sms^ [^marc.thevenin@ined.fr^]
.-

