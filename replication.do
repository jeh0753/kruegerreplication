capture log close
log using appendix2_1.log, replace
clear all


/* The first part of this file is Krueger's original code. At line 320 I begin my robustness checks etc.

Quoted from Krueger original code: 

This program creates a NxN matrix of countries, containing the no. of attacks each country i
has launched against country j.

The list of N countries with population > 1 million is contained in Count.xls.

Significant Events from Patterns of Global Terrorism 1997-2003 

Assigns affected=affected2 if origin = affected1  

Drops Israel /WB & Gaza

Drops ij countries.  

November 22, 2004

Reassign Kashmir as Pakistan for origin 


*/

set mem 300m
set matsize 800

insheet using count.raw, names
reshape long o, i(affected) j(count)
replace count=.
rename o origin
drop if affected=="West Bank and Gaza"
drop if origin=="West Bank and Gaza"
sort affected origin
save count, replace

use event4, clear
rename affected1 affected

replace origin = "Pakistan" if kashmir==1 

replace affected = affected2 if affected==origin 
/* country names */
replace origin="Israel" if origin=="West Bank and Gaza"
replace affected="Israel" if affected=="West Bank and Gaza"


bys affected origin: gen count=_N
keep affected origin count
bys affected origin: keep if _n==1
sort affected origin
merge affected origin using count
tab _merge
drop _merge
sort affected
save count,replace

use krueglait, clear
keep if pop>1000000
gene double gdp = exp(lgdp) 
replace gdp = 295 if country=="Myanmar"       // Cambodia 
replace gdp = 368 if country=="Afghanistan"   // Tajikistan        
replace gdp = 914 if country=="Korea, North"  // Albania      
replace gdp = 5000 if country=="Libya"        //  http://www.nationmaster.com/country/ly/Economy 
replace gdp = 113.5 if country=="Somalia"     // Ethiopia        
replace gdp = 942  if country=="Cuba"         // Guyana 
replace gdp = 1482 if country=="Yugoslavia"   // Bulgaria       
replace gdp = 1619 if country=="Iraq"         // Iran 
replace gdp = 12375 if country=="Korea, Rep." // fix up 
replace lgdp = log(gdp)   
drop if country=="West Bank and Gaza"
qui for var _all: rename X t_X
rename t_country affected
sort affected
merge affected using count
tab _merge
drop _merge
sort origin
save count, replace


use krueglait, clear
keep if pop>1000000
gene double gdp = exp(lgdp) 
replace gdp = 295 if country=="Myanmar"       // Cambodia 
replace gdp = 368 if country=="Afghanistan"   // Tajikistan        
replace gdp = 914 if country=="Korea, North"  // Albania      
replace gdp = 5000 if country=="Libya"        //  http://www.nationmaster.com/country/ly/Economy 
replace gdp = 113.5 if country=="Somalia"     // Ethiopia       
replace gdp = 942  if country=="Cuba"         // Guyana 
replace gdp = 1482 if country=="Yugoslavia"   // Bulgaria       
replace gdp = 1619 if country=="Iraq"         // Iran 
replace gdp = 12375 if country=="Korea, Rep." // fix up 
replace lgdp = log(gdp) 
drop if country=="West Bank and Gaza"
qui for var _all: rename X o_X
rename o_country origin
sort origin
merge origin using count
tab _merge
drop _merge

gene o_gr9096 = (o_gdp96 / o_gdp90)^(1/6) - 1 
 

replace count=0 if count==.
drop if affected=="" | origin==""


** use Haversine distance formula 
replace t_latitude = t_latitude*-1 if t_lat_d=="S"
replace o_latitude = o_latitude*-1 if o_lat_d=="S"

replace t_longitude = t_longitude*-1 if t_long_d=="E"
replace o_longitude = o_longitude*-1 if o_long_d=="E"


 * convert longitude and latitude to radians 

gene double lon1 = o_longitude* 0.017453293 
gene double lat1 = o_latitude * 0.017453293 
gene double lon2 = t_longitude* 0.017453293 
gene double lat2 = t_latitude* 0.017453293 

gene double dlon = lon2 - lon1 
gene double dlat = lat2 - lat1 
gene double a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2 
replace a = 1 if a > 1 & a~=.
gene double c = 2 * asin(a^.5) 
gene double d = 3956 * c 

replace d=. if dlon==. | dlat==. 
replace d = d/1000 

label var d "distance betw capitals (1000s mi)" 


* same continent 
gene same  = (t_continent==o_continent) 

gen self=(affected==origin)


sum 
sum d if self==1 

gene ot_lgdp = o_lgdp * t_lgdp 



sort origin affected 

save temp 

clear 

use trade 

rename importer origin 
rename exporter affected 

sort origin affected  

rename trade imports 

merge origin affected using temp 

sort origin affected 


label var imports "imports to origin country" 

tab _merge 

drop _merge 

save temp, replace 

clear 


use trade 
rename importer affected 
rename exporter origin 

sort origin affected  

merge origin affected using temp 

rename trade exports 

label var exports "Exports from origin country"  

tab _merge 
drop _merge

gene trade = (imports+exports)/((o_pop*t_pop)^.5) 

replace exports = (exports)/((o_pop*t_pop)^.5) 
replace imports = (imports)/((o_pop*t_pop)^.5) 

erase temp.dta  



gene occupier = 0 
replace occupier =1 if affected=="U.S.A." | affected=="Israel" | affected=="U.K." | affected == "Indonesia" | affected =="Armenia" | affected =="Turkey" | affected =="Israel" | affected=="Syria" | affected=="Morocco" | affected == "Uganda" | affected=="Rwanda" | affected=="Burundi" | affected=="Angola" | affected=="Namibia" | affected=="Ethiopia" | affected=="Nigeria" 
   
   
gene occupied=0 
replace occupied=1 if origin=="Iraq" | origin=="Afghanistan" | origin=="East Timor" | origin=="Cyprus" | origin=="Lebanon" | origin=="Syria" | origin=="Drc" | origin=="Eritrea" | origin=="Sierra Leone"  



#delimit cr 

egen double o_max=rmax( o_muslim o_christian o_hindu o_budhist o_other)
egen double t_max=rmax(t_muslim t_christian t_hindu t_budhist t_other) 

gen str20 o_mrelig="" 
gen str20 t_mrelig="" 

foreach i in o_ t_{
foreach j in christian hindu budhist other muslim {
replace `i'mrelig="`j'" if `i'max==`i'`j'
}
} 

gen samerel =(o_mrelig==t_mrelig) 
tab samerel 


gen diffrel = 1-samerel 
replace diffrel = . if o_muslim==. | t_muslim==.  
drop samerel  


keep if self==0 




gen o_gdpc = . 
 replace o_gdpc = 1 if o_lgdp < 6.008 & o_lgdp~=.
 replace o_gdpc = 2 if 6.008<= o_lgdp & o_lgdp < 7.301 
 replace o_gdpc = 3 if 7.301 <= o_lgdp & o_lgdp < 8.517 
 replace o_gdpc = 4 if o_lgdp >= 8.517 & o_lgdp~=. 

gene t_gdpc = . 
 replace t_gdpc = 1 if t_lgdp < 6.008 & t_lgdp~= .
 replace t_gdpc = 2 if 6.008<= t_lgdp & t_lgdp < 7.301 
 replace t_gdpc = 3 if 7.301 <= t_lgdp & t_lgdp < 8.517 
 replace t_gdpc = 4 if t_lgdp >= 8.517 & t_lgdp~= . 



gene g11 = (o_gdpc==1 & t_gdpc==1) if o_lgdp~=. & t_lgdp~=. 
gene g12 = (o_gdpc==1 & t_gdpc==2) if o_lgdp~=. & t_lgdp~=.
gene g13 = (o_gdpc==1 & t_gdpc==3) if o_lgdp~=. & t_lgdp~=.
gene g14 = (o_gdpc==1 & t_gdpc==4) if o_lgdp~=. & t_lgdp~=.

gene g21 = (o_gdpc==2 & t_gdpc==1) if o_lgdp~=. & t_lgdp~=.
gene g22 = (o_gdpc==2 & t_gdpc==2) if o_lgdp~=. & t_lgdp~=.
gene g23 = (o_gdpc==2 & t_gdpc==3) if o_lgdp~=. & t_lgdp~=.
gene g24 = (o_gdpc==2 & t_gdpc==4) if o_lgdp~=. & t_lgdp~=.

gene g31 = (o_gdpc==3 & t_gdpc==1) if o_lgdp~=. & t_lgdp~=.
gene g32 = (o_gdpc==3 & t_gdpc==2) if o_lgdp~=. & t_lgdp~=.
gene g33 = (o_gdpc==3 & t_gdpc==3) if o_lgdp~=. & t_lgdp~=.
gene g34 = (o_gdpc==3 & t_gdpc==4) if o_lgdp~=. & t_lgdp~=.

gene g41 = (o_gdpc==4 & t_gdpc==1) if o_lgdp~=. & t_lgdp~=.
gene g42 = (o_gdpc==4 & t_gdpc==2) if o_lgdp~=. & t_lgdp~=.
gene g43 = (o_gdpc==4 & t_gdpc==3) if o_lgdp~=. & t_lgdp~=.
gene g44 = (o_gdpc==4 & t_gdpc==4) if o_lgdp~=. & t_lgdp~=.




gene civ33 = (o_civr<=2.5 & t_civr<=2.5) if o_civr~=. & t_civr~=. 
gene civ32 = (o_civr<=2.5 & t_civr>2.5 & t_civr<4.5) if o_civr~=. & t_civr~=. 
gene civ31 = (o_civr<=2.5 & t_civr>=4.5) if o_civr~=. & t_civr~=.  


gene civ23 = (o_civr>2.5 & o_civr<4.5 & t_civr<=2.5) if o_civr~=. & t_civr~=. 
gene civ22 = (o_civr>2.5 & o_civr<4.5 & t_civr>2.5 & t_civr<4.5) if o_civr~=. & t_civr~=. 
gene civ21 = (o_civr>2.5 & o_civr<4.5 & t_civr>=4.5) if o_civr~=. & t_civr~=.  


gene civ13 = (o_civr>=4.5 & t_civr<=2.5) if o_civr~=. & t_civr~=. 
gene civ12 = (o_civr>=4.5 & t_civr>2.5 & t_civr<4.5) if o_civr~=. & t_civr~=. 
gene civ11 = (o_civr>=4.5 & t_civr>=4.5) if o_civr~=. & t_civr~=.  


drop if origin=="Israel" | affected=="Israel" 
*****ORIGINAL REPLICATION
* col 1 
nbreg count d o_lpop o_lgdp t_lpop t_lgdp, disp(constant) cluster(origin) 
eststo col1
nbreg count d o_lpop o_lgdp t_lpop t_lgdp 

* col 2
nbreg count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr, disp(constant) cluster(origin)
eststo col2
nbreg count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr


* col 3 
nbreg count d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier, disp(constant) cluster(origin)
eststo col3
test o_muslim o_budhist o_hindu o_other 


outreg2 [col1 col2 col3] using krueger2, label replace word excel 
esttab col1 col2 col3 using output2, label replace

*****ROBUSTNESS CHECKS
*merge war data
kountry origin, from(other)
replace origin=NAMES_STD
drop NAMES_STD
kountry affected, from(other)
replace affected=NAMES_STD
drop NAMES_STD
sort origin affected 
save temp 
clear 
use wars2clean3 
sort origin affected  
merge origin affected using temp 
sort origin affected 
tab _merge 
drop _merge 
save temp, replace 
clear 
use wars2clean3 
sort origin affected  
merge origin affected using temp 
tab _merge 
erase temp.dta 
replace wardummy=0 if wardummy==. 
replace wardummy=0 if wardummy==2 
drop if o_lpop==.

sort origin affected
by origin affected: gen copies= _N
tab copies
****Checks on Column 1
* log-dif
gen o_dlgdp=exp(o_lgdp)
gen t_dlgdp=exp(t_lgdp)
gen d_dlgdp=abs(t_dlgdp-o_dlgdp)
gen d_lgdp=log(d_dlgdp)
nbreg count d o_lpop t_lpop d_lgdp, disp(constant) cluster(origin) 
eststo colr1b
*  poisson with robust error
poisson count d o_lpop o_lgdp t_lpop t_lgdp, r
eststo colr4b
* linear probability model with two-way clustering (OLS)
gen countdummy=1 if count>0
replace countdummy=0 if count==0
ivreg2 countdummy d o_lpop o_lgdp t_lpop t_lgdp, r cluster(origin affected)
eststo colr5b
* inclusion of war dummy
nbreg count d o_lpop o_lgdp t_lpop t_lgdp wardummy, disp(constant) cluster(origin)
eststo colr2b
* zero inflation approach
zinb count d o_lpop o_lgdp t_lpop t_lgdp, inflate(wardummy)
eststo colr3b
outreg2 [col1 colr1b colr2b colr3b colr4b colr5b] using krueger1, label replace word excel 

****Checks on Column 2
* log-dif

nbreg count d o_lpop o_civr t_lpop t_civr d_lgdp, disp(constant) cluster(origin) 
eststo colr1
*  poisson with robust error
poisson count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr, r
eststo colr4
* linear probability model with two-way clustering (OLS)
ivreg2 countdummy d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr, r cluster(origin affected)
eststo colr5

* inclusion of war dummy
nbreg count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr wardummy, disp(constant) cluster(origin) 
eststo colr2
* zero inflation approach
zinb count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr, inflate(wardummy)
eststo colr3
outreg2 [col2 colr1 colr2 colr3 colr4 colr5] using krueger3, label replace word excel 

****Checks on Column 3
* log-dif
nbreg count d trade diffrel o_lpop o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_civr occupier d_lgdp, disp(constant) cluster(origin) 
eststo colr1a
*  poisson with robust error
poisson count d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier, r
eststo colr4a
* linear probability model with two-way clustering (OLS)
ivreg2 countdummy d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier, r cluster(origin affected)
eststo colr5a
* inclusion of war dummy
nbreg count d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier wardummy, disp(constant) cluster(origin)
eststo colr2a
* zero inflation approach
zinb count d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier, inflate(wardummy)
eststo colr3a
outreg2 [col3 colr1a colr2a colr3a colr4a colr5a] using krueger4, label replace word excel 

**Graph Original
nbreg count d o_lpop t_lpop o_lgdp t_lgdp, irr
prgen o_lgdp, gen(simple_o)
prgen t_lgdp, gen(simple_t)
nbreg count d o_lpop t_lpop d_lgdp, irr
prgen d_lgdp, gen(differenced)
twoway line simplmu  simple_tmu  simple_tx, scheme(s1mono) aspect(1) legend(lab(1 "Origin") lab(2 "Target") lab(3 "Differenced") )  yscale(r(0 .01)) ytitle("Predicted Count") lp(-) lc(black black) title("Predicted Counts of Terrorist Events" "against Origin/Target GDP")

**Graph OMVB
nbreg count d o_lpop t_lpop o_lgdp t_lgdp wardummy, irr
prgen o_lgdp, gen(simo2)
prgen t_lgdp, gen(simt2)
twoway line simo2mu  simt2mu  simt2x, scheme(s1mono) aspect(1) legend(lab(1 "Origin") lab(2 "Target") lab(3 "Differenced") )  yscale(r(0 .01)) ytitle("Predicted Count") lp(-) lc(black black) title("Predicted Counts of Terrorist Events" "against Origin/Target GDP")


**wardummy descriptive stats
gen ogdpq=1 if o_q1==1
replace ogdpq=2 if o_q2==1
replace ogdpq=3 if o_q3==1
tab wardummy ogdpq
gen tgdpq=1 if t_q1==1
replace tgdpq=2 if t_q2==1
replace tgdpq=3 if t_q3==1
tab tgdpq wardummy

**IRR Regressions
* col 1 

nbreg count d o_lpop o_lgdp t_lpop t_lgdp, disp(constant) cluster(origin) r irr 
nbreg count d o_lpop o_lgdp t_lpop t_lgdp
prchange
prchange if count>=1
* log-dif
nbreg count d o_lpop t_lpop d_lgdp, disp(constant) cluster(origin) r irr
nbreg count d o_lpop t_lpop d_lgdp
prchange
prchange if count>=1
*  poisson with robust error
poisson count d o_lpop o_lgdp t_lpop t_lgdp, cluster(origin) r irr
* linear probability model with two-way clustering (OLS)
ivreg2 countdummy d o_lpop o_lgdp t_lpop t_lgdp, r cluster(origin affected)
* inclusion of war dummy
nbreg count d o_lpop o_lgdp t_lpop t_lgdp wardummy, disp(constant) cluster(origin) r irr
nbreg count d o_lpop o_lgdp t_lpop t_lgdp wardummy
prchange
prchange if count>=1
* zero inflation approach
zinb count d o_lpop o_lgdp t_lpop t_lgdp, inflate(wardummy) cluster(origin) r irr
zinb count d o_lpop o_lgdp t_lpop t_lgdp, inflate(wardummy)
prchange
prchange if count>=1
* col 2
nbreg count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr, disp(constant) cluster(origin) r irr
nbreg count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr
prchange
prchange if count>=1
* log-dif
nbreg count d o_lpop o_civr t_lpop t_civr d_lgdp, disp(constant) cluster(origin) r irr 
nbreg count d o_lpop o_civr t_lpop t_civr d_lgdp
prchange
prchange if count>=1
*  poisson with robust error
poisson count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr, cluster(origin) r irr
poisson count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr
* linear probability model with two-way clustering (OLS)
ivreg2 countdummy d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr, r cluster(origin affected)
* inclusion of war dummy
nbreg count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr wardummy, disp(constant) cluster(origin) r irr 
nbreg count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr wardummy
prchange
prchange if count>=1
* zero inflation approach
zinb count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr, inflate(wardummy) cluster(origin) r irr
zinb count d o_lpop o_lgdp o_civr t_lpop t_lgdp t_civr, inflate(wardummy)
prchange
prchange if count>=1
* col 3 
nbreg count d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier, disp(constant) cluster(origin) r irr
nbreg count d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier
prchange
prchange if count>=1
* log-dif
nbreg count d trade diffrel o_lpop o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_civr occupier d_lgdp, disp(constant) cluster(origin) r irr
nbreg count d trade diffrel o_lpop o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_civr occupier d_lgdp
prchange
prchange if count>=1
*  poisson with robust error
poisson count d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier, cluster(origin) r irr
* linear probability model with two-way clustering (OLS)
ivreg2 countdummy d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier, r cluster(origin affected)
* inclusion of war dummy
nbreg count d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier wardummy, disp(constant) cluster(origin) r irr
nbreg count d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier wardummy
prchange
prchange if count>=1
* zero inflation approach
zinb count d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier, inflate(wardummy) cluster(origin) r irr
zinb count d trade diffrel o_lpop o_lgdp o_civr o_muslim o_budhist o_hindu o_other o_femil occupied  t_lpop t_lgdp t_civr occupier, inflate(wardummy)
prchange
prchange if count>=1
