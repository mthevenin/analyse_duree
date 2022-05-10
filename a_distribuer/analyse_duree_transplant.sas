
libname x "/home/users/thevenin_m";

data trans; set x.transplantation_m;
run;

****** non param;

* méthode actuarielle;
proc lifetest data=trans method=lifetable width=30;
time stime*died(0);run;

* kaplan meier;
ods exclude Lifetest.Stratum1.ProductLimitEstimates;
proc lifetest data=trans;
time stime*died(0); run;

ods exclude Lifetest.Stratum1.ProductLimitEstimates Lifetest.Stratum2.ProductLimitEstimates ;
proc lifetest data=trans;
time stime*died(0);
strata surgery / test=all;
run;


* RMST: en attente de mise à jour de SAS Studio (5 ans de retard sur la derniere version du module Stat)
ods exclude Lifetest.Stratum1.ProductLimitEstimates;
proc lifetest data=trans rmst plots=(rmst);
time stime*died(0);
strata surgery; run;


****** Cox;
proc phreg data=trans;
model stime*died(0) = year age surgery ;
run;

* Test Grambsh-Therneau;
ods select PHReg.zphTest;
proc phreg data=trans zph(global noplot);
model stime*died(0) = year age surgery ;
run;

ods select PHReg.zphTest;
proc phreg data=trans zph(global noplot transform=km);
model stime*died(0) = year age surgery ;
run;

* Interaction duree;
* Vérifier/penser à ce que la variable soit bien en indicatrices;

ods select PHReg.ParameterEstimates;
proc phreg data=trans ;
model stime*died(0) = year age surgery surgeryt ;
surgeryt = surgery*stime;
run;

* Variables dynamique;
ods select PHReg.ParameterEstimates;
proc phreg data=trans;
model stime*died(0) = year age surgery tvc ;
tvc = transplant=1 and stime>=wait;
run;

*** logistique temps discret;

* t continu;
data td; set trans; 
do t=1 to mois; 
     output; 
     end; run;
     
data td; set td;
if t<mois then died=0;
t2=t*t;
t3=t2*t; run; 

proc logistic data=td;
model died(ref="0") = t t2 t3 year age surgery  ; run;

* t discret;

proc rank data=td out=td2 groups=4;
var t;
ranks tq4;
run;

data td2; set td2;
id2=put(id, 3.);
tq42=put(tq4, 1.);
g=id2 || tq42; run;

proc sort data=td2; by id tq4; run;

data td2; set td2;
by g;
if LAST.g; run;

proc logistic data=td2;
class tq4 / param=ref;
model died(ref="0") = tq4 year age surgery; run;

** AFT Weibull;

proc lifereg data=trans;
model stime*died(0) = year age surgery /D=WEIBULL;
run;

** Risques concurrents;

proc lifetest data=trans plots=CIF;
time stime*compet(0) / eventcode=1; run;

proc lifetest data=trans plots=CIF;
time stime*compet(0) / eventcode=1;
strata surgery; run;

data td; set trans; 
do t=1 to mois; 
     output; 
     end; 
run;

data td; set td;
if t<mois then compet=0;
t2=t*t;
run;

proc logistic data=td;
model compet(ref="0") = t t2 year age surgery / link=glogit;
run;




