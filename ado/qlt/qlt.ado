capture program drop qlt
program define qlt /*, byable(recall)*/

syntax [if], [sas]

local q 0.90 0.75 0.50 0.25 0.10

if "`sas'"=="" {
marksample touse
qui capture if `touse'
di "Duree pour differents quantiles de la fonction de survie"
di "Definition des bornes " as result "Stata-ltable"
foreach q2 of local q {
preserve
qui capture keep if `touse'
qui gen mdif=abs(survival-`q2')
qui gen mdif2=mdif-mdif[_n-1]
qui drop if mdif2==0
qui gen s2=survival[_n+1]
qui gen t02=t0[_n+1]
qui replace s2=0 if s2==.
qui gen x=1 if survival>=`q2' & s2<=`q2'
qui keep if x==1

qui capture gen tq=t0 + ((t02-t0)*(survival-`q2')/(survival-s2)) 
qui capture replace tq=. if tq<=0
qui capture sum t02
qui capture replace tq=. if tq>`r(max)'
qui capture replace tq=tq
qui capture sum tq 
di as text "S(t)=`q2': t="as result %9.3f r(max)
restore
}
}

if "`sas'"!="" {
di "Duree pour differents quantiles de la fonction de survie"
di "Definition des bornes " as result "Sas-lifetest"

marksample touse
qui capture keep if `touse'

qui drop start lost death lost  failure-uhazard
unab a: _all
local b t0 t1 group
local var : list a - b

qui gen last = _n == _N
qui replace last = last + 1 if _n==_N
qui expand last
qui drop last

foreach v of local var {
qui gen `v'2=`v'[_n-1]
qui drop `v'
qui rename `v' `v'
}
qui replace survival=1 if _n==1
qui replace t0=t1[_n-1] if _n==_N
qui replace t1=.        if _n==_N

foreach q2 of local q {
preserve
qui gen mdif=abs(survival-`q2')
qui gen mdif2=mdif-mdif[_n-1]
qui drop if mdif2==0
qui gen s2=survival[_n+1]
qui gen t02=t0[_n+1]
qui replace s2=0 if s2==.
qui gen x=1 if survival>=`q2' & s2<=`q2'
qui keep if x==1
qui capt gen tq=t0 + ((t02-t0)*(survival-`q2')/(survival-s2)) 
qui capt replace tq=. if tq<=0
qui capt sum t02
qui capt replace tq=. if tq>`r(max)'
qui capt replace tq=tq
qui capt sum tq 
di as text "S(t)=`q2': t="as result %9.3f r(max)
restore
}
} 
end
