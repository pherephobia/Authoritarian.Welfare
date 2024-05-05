tsset ccode year
gen lnpop = log(e_mipopula)
gen lngdp = log(e_migdppc)
gen ww1 = 0
gen ww2 = 0
gen coldwar = 0
replace ww1 = 1 if year > 1913 & year < 1919
replace ww2 = 1 if year > 1938 & year < 1946
replace coldwar = 1 if year > 1945 & year < 1992

sort ccode year 
by ccode: gen lag3pi = v2xps_party[_n-3]
by ccode: gen lag3class = class[_n-3]
by ccode: gen lag3resdep = resdep2[_n-3]
by ccode: gen lag3urban = urban[_n-3]
by ccode: gen lag3mil = cow_milsize[_n-3]
by ccode: gen lag3growth = e_migdpgro[_n-3]
by ccode: gen lag3gdp = lngdp[_n-3]
by ccode: gen lag3pop = lnpop[_n-3]
by ccode: gen lag3civil = e_civil_war[_n-3]
by ccode: gen lag3poly = v2x_polyarchy[_n-3]
by ccode: gen lag3pbranch = v2psprbrch[_n-3]
by ccode: gen lag3plink = v2psprlnks[_n-3]
by ccode: gen lag3porgs = v2psorgs[_n-3]
by ccode: gen lag3pplats = v2psplats[_n-3]
by ccode: gen lag3pcohesv = v2pscohesv[_n-3]
    
tab lag3class, gen(dum_)
rename dum_1 party
rename dum_2 military
rename dum_6 working
rename dum_7 urban_middle

label variable lag3pi "Party Institutionalization"
label variable lag3resdep "Resource Dep."
label variable lag3gdp "Ln.Pop."
label variable lag3pop "Ln.GDPpc"
label variable party "Party elites"
label variable military "The military"
label variable urban_middle "Urban middle"
label variable working "Working class"
label variable v2dlunivl "Universalism Index of V-Dem"
label variable totalunivers "Universalism Index of SPaW"

forvalues bot=1917(3)2017 {
	local top=`bot'+2
	gen years_`bot'_`top' = year >= `bot' & year <= `top' ///
		if !missing(year)
}

forvalues bot=1917(5)2017 {
	local top=`bot'+4
	gen years_`bot'_`top' = year >= `bot' & year <= `top' ///
		if !missing(year)
}

forvalues bot=1917(10)2017 {
	local top=`bot'+9
	gen years_`bot'_`top' = year >= `bot' & year <= `top' ///
		if !missing(year)
}


**Table1

***Panel Data Autocorrelation Breusch-Pagan-Godfrey Test
xtserial totalunivers party military working lag3gdp lag3pop /*
*/ if (dum_3==0  | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0) & year > 1916


*Wooldridge test for autocorrelation in panel data
*H0: no first-order autocorrelation
*    F(  1,      93) =   1356.878
*           Prob > F =      0.0000 = reject



**Panel Data HeteroskedasticTest
xtgls totalunivers party military working lag3gdp lag3pop /*
*/ if (dum_3==0  | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0) & year > 1916, /*
*/ igls panels(heteroskedastic)
 estimates store hetero
xtgls totalunivers party military working lag3gdp lag3pop /*
*/ if (dum_3==0  | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0) & year > 1916
local df = e(N_g) - 1
lrtest hetero . , df(`df')
***NULL: homoskedasticity / reject

*** 1-year
**** table1_model1
	qui xtpcse totalunivers i.ccode ww1 ww2 coldwar party military working /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t1m1
	
**** table1_model2
	xtpcse totalunivers i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t1m2
	
**** table1_model3
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t1m3
	
**** table1_model4
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working/*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t1m4

	
**** table1_model5
	qui xtpcse totalunivers i.ccode ww1 ww2 coldwar lag3pi /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t1m5
	
**** table1_model6
	qui xtpcse totalunivers i.ccode ww1 ww2 coldwar lag3pi /*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t1m6
	
**** table1_model7
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar lag3pi /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t1m7
	
**** table1_model8
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar lag3pi/*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t1m8

**** table1_model9
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working lag3pi/*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t1m9
	
**** table1_model10
	qui xtpcse totalunivers i.ccode ww1 ww2 coldwar party military working lag3pi/*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t1m10


esttab t1m2 t1m6 t1m10 t1m4 t1m8 t1m9 /*
*/ using "C:/Users/phere/Dropbox/Scholar/2_Graduates/2019_03_Fall/PER_R19_AutocraticWelfare/5. tex/tables/Hypothesis1_2.tex", noconst label mlabels(,depvar) /*
*/ title("Class Coalitions, Party Institutionalization and Welfare Universalism") /*
*/ b(3) se(3) r2(3) replace /*
*/ indicate("Country Dummies = *ccode*" "WWI Dummies = *ww1*" "WWII Dummies = *ww2*" "Cold War Dummies = *coldwar*" ) /*
  */ order(party military working lag3pi) ///
	 addnotes(OLS with panel corrected standard errors. /*
	 */Panel-Corrected Standard Errors reported in parentheses. /*
	 */Constant, period dummies and country dummies not displayed.)

	 
***regsave
	qui xtpcse totalunivers i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table1, tstat pval ci
	qui xtpcse totalunivers i.ccode ww1 ww2 coldwar lag3pi /*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table1, tstat pval ci append
	 	qui xtpcse totalunivers i.ccode ww1 ww2 coldwar party military working lag3pi/*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table1, tstat pval ci append
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working/*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table1, tstat pval ci append
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar lag3pi/*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table1, tstat pval ci append
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working lag3pi/*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table1, tstat pval ci append

**** table1_model4_pi deaggregate
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3pbranch /* 
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t2m1
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3plink /*  
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t2m2
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3porgs /*   
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t2m3
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3pplats /*   
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t2m4
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3pcohesv /* 
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t2m5
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t2m6

esttab t2m1 t2m2 t2m3 t2m4 t2m5 t2m6 /*
*/ using "C:/Users/phere/Dropbox/Scholar/2_Graduates/2019_03_Fall/PER_R19_AutocraticWelfare/5. tex/tables/Hypothesis1_pi.tex", noconst label mlabels(,depvar) /*
*/ title("Class Coalitions, Party Institutionalization and Welfare Universalism") /*
*/ b(3) se(3) r2(3) replace /*
*/ indicate("Country Dummies = *ccode*" "WWI Dummies = *ww1*" "WWII Dummies = *ww2*" "Cold War Dummies = *coldwar*" ) /*
  */ order(party military working lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv ) ///
	 addnotes(OLS with panel corrected standard errors. /*
	 */Panel-Corrected Standard Errors reported in parentheses. /*
	 */Constant, period dummies and country dummies not displayed.)

qui xtpcse totalunivers i.ccode ww1 ww2 coldwar working  urban_middle party /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t2pi1
	regsave using t2pi1, tstat pval ci

qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar working  urban_middle party /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	regsave using t2pi2, tstat pval ci

esttab t2pi1 t2pi2 /*
*/ using "C:/Users/phere/Dropbox/Scholar/2_Graduates/2019_03_Fall/PER_R19_AutocraticWelfare/5. tex/tables/Hypothesis1_pi2.tex", noconst label mlabels(,depvar) /*
*/ title("Class Coalitions, Disagreegated Party Institutionalization and Welfare Universalism") /*
*/ b(3) se(3) r2(3) replace /*
*/ indicate("Control Variables = *lag3gdp lag3pop lag3resdep*" "Country Dummies = *ccode*" "WWI Dummies = *ww1*" "WWII Dummies = *ww2*" "Cold War Dummies = *coldwar*" ) /*
  */ order(party military working lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv ) ///
	 addnotes(OLS with panel corrected standard errors. /*
	 */Panel-Corrected Standard Errors reported in parentheses. /*
	 */Constant, period dummies and country dummies not displayed.)
	
***regsave
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3pbranch /* 
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table2m1, tstat pval ci

	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3plink /*  
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table2m2, tstat pval ci

	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3porgs /*   
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table2m3, tstat pval ci

	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3pplats /*   
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table2m4, tstat pval ci

	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3pcohesv /* 
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table2m5, tstat pval ci

	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table2m6, tstat pval ci

xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3plink lag3porgs lag3pplats /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)

	
*** Interactions
xtpcse totalunivers i.ccode ww1 ww2 coldwar i.(party military working)##c.lag3pi /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using int1, tstat pval ci

qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar i.(party military working)##c.lag3pi /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using int2, tstat pval ci

	qui xtpcse univers_oldageprog i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m1
	
	qui xtpcse univers_mater_prog i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m2
	
	qui xtpcse univers_sick_prog i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m3
	
	qui xtpcse univers_working_prog i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m4
	
	qui xtpcse univers_unemp_prog i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m5
	
	qui xtpcse univers_familiy_prog i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m6
     
esttab t3m1 t3m2 t3m3 t3m4 t3m5 t3m6 /*
*/ using "C:/Users/phere/Dropbox/Scholar/2_Graduates/2019_03_Fall/PER_R19_AutocraticWelfare/5. tex/tables/Hypothesis2.tex", noconst label mlabels(,depvar) /*
*/ title("Class Coalitions, and Welfare Programs") /*
*/ b(3) se(3) r2(3) replace /*
*/ indicate("Country Dummies = *ccode*" "WWI Dummies = *ww1*" "WWII Dummies = *ww2*" "Cold War Dummies = *coldwar*" ) /*
  */ order(party military working) ///
	 addnotes(OLS with panel corrected standard errors. /*
	 */Panel-Corrected Standard Errors reported in parentheses. /*
	 */Constant, period dummies and country dummies not displayed.)
	 
	xtpcse univers_mater_prog i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3pi lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)

	 
	qui xtpcse univers_oldageprog i.ccode ww1 ww2 coldwar party urban_middle working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m1
	regsave using t3m1, tstat pval ci replace
	
	qui xtpcse univers_mater_prog i.ccode ww1 ww2 coldwar party urban_middle working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m2
	regsave using t3m2, tstat pval ci replace
	
	qui xtpcse univers_sick_prog i.ccode ww1 ww2 coldwar party urban_middle working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m3
	regsave using t3m3, tstat pval ci replace
	
	qui xtpcse univers_working_prog i.ccode ww1 ww2 coldwar party urban_middle working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m4
	regsave using t3m4, tstat pval ci replace
	
	qui xtpcse univers_unemp_prog i.ccode ww1 ww2 coldwar party urban_middle working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m5
	regsave using t3m5, tstat pval ci replace
	
	xtpcse univers_familiy_prog i.ccode ww1 ww2 coldwar party urban_middle working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t3m6
	regsave using t3m6, tstat pval ci replace
	
esttab t3m1 t3m2 t3m3 t3m4 t3m5 t3m6 /*
*/ using "C:/Users/phere/Dropbox/Scholar/2_Graduates/2019_03_Fall/PER_R19_AutocraticWelfare/5. tex/tables/Hypothesis2.tex", noconst label mlabels(,depvar) /*
*/ title("Class Coalitions, and Welfare Programs") /*
*/ b(3) se(3) r2(3) replace /*
*/ indicate("Country Dummies = *ccode*" "WWI Dummies = *ww1*" "WWII Dummies = *ww2*" "Cold War Dummies = *coldwar*" "P.I. Indicators = *lag3p*" ) /*
  */ order(working urban_middle party) ///
	 addnotes(OLS with panel corrected standard errors. /*
	 */Panel-Corrected Standard Errors reported in parentheses. /*
	 */Constant, period dummies and country dummies not displayed.)
	 
	 
	 
xtpcse univers_oldageprog i.ccode ww1 ww2 coldwar military working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0 | party==0)/*
	*/ & year > 1916 , p c (p)

xtpcse univers_mater_prog i.ccode ww1 ww2 coldwar military working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0 | party==0)/*
	*/ & year > 1916 , p c (p)

xtpcse univers_sick_prog i.ccode ww1 ww2 coldwar military working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0 | party==0)/*
	*/ & year > 1916 , p c (p)
	
xtpcse univers_working_prog i.ccode ww1 ww2 coldwar military working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0 | party==0)/*
	*/ & year > 1916 , p c (p)

	
xtpcse univers_unemp_prog i.ccode ww1 ww2 coldwar military working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0 | party==0)/*
	*/ & year > 1916 , p c (p)

	
xtpcse univers_familiy_prog i.ccode ww1 ww2 coldwar military working /*
	*/ lag3pbranch lag3plink lag3porgs lag3pplats lag3pcohesv lag3gdp lag3pop lag3resdep/*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0 | party==0)/*
	*/ & year > 1916 , p c (p)
