use "C:\Users\phere\Dropbox\Scholar\2_Graduates\2019_03_Fall\PER_R19_AutocraticWelfare\3. Datasets_Codebooks\mdata.dta" 

tsset ccode year
gen lnpop = log(e_mipopula)
gen lngdp = log(e_migdppc)
gen ww1 = 0
gen ww2 = 0
gen coldwar = 0
replace ww1 = 1 if year > 1913 & year < 1919
replace ww2 = 1 if year > 1938 & year < 1946
replace coldwar = 1 if year > 1945 & year < 1992

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

tab lag3class, gen(dum_)
rename dum_1 party
rename dum_2 military
rename dum_6 working
rename dum_7 urban_middle
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
	xtpcse v2dlunivl i.ccode ww1 ww2 coldwar lag3pi /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
	est store t1m7
	
**** table1_model8
	xtpcse v2dlunivl i.ccode ww1 ww2 coldwar lag3pi/*
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
*/ using "C:/Users/phere/Dropbox/Scholar/2_Graduates/2019_03_Fall/PER_R19_AutocraticWelfare/5. tex/tables/Hypothesis1.tex", noconst label mlabels(,depvar) /*
*/ title("Table 1: Class Coalitions, Party Institutionalization and Welfare Universalism") /*
*/ b(3) se(3) replace /*
*/ indicate("Country Dummies = *ccode*" "WWI Dummies = *ww1*" "WWII Dummies = *ww2*" "Cold War Dummies = *coldwar*" ) /*
  */ order(party military working lag3pi) ///
	 addnotes(OLS with panel corrected standard errors. Panel-Corrected Standard Errors reported in parentheses. /*
	 */Constant, period dummies and country dummies not displayed.)

	 
***regsave
	qui xtpcse totalunivers i.ccode ww1 ww2 coldwar party military working /*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table1, tstat pval ci replace
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
	xtpcse v2dlunivl i.ccode ww1 ww2 coldwar lag3pi/*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table1, tstat pval ci append
	qui xtpcse v2dlunivl i.ccode ww1 ww2 coldwar party military working lag3pi/*
	*/ lag3gdp lag3pop lag3resdep /*
	*/ if (dum_3==0  | dum_4==0 | dum_5==0 | dum_8==0 | dum_9==0 | dum_10==0)/*
	*/ & year > 1916 , p c (p)
regsave using table1, tstat pval ci append






xtpcse totalencomp lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65  i.Ccodecow i.year if regime !=., hetonly


**Table2
qui xtpcse univers_oldageprog lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model8

qui xtpcse univers_mater_prog lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model9


qui xtpcse univers_mater_prog lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model10


qui xtpcse univers_sick_prog lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model11

qui xtpcse univers_working_prog lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model12

qui xtpcse univers_unemp_prog lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model13

qui xtpcse univers_familiy_prog lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly


esttab model8 model9 model10 model11 model12 model13 /*
	*/ using "NEW_MAIN_TABLE2.tex", noconst  nodepvar b(a2) se replace  /*
	*/ title("Table 2: Party institutionalization and welfare Programs") /*
	  */ indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) /*
	  */ order(lag3v2psorgs lag3v2psprbrch lag3v2psplats lag3v2psprlnks lag3v2pscohesv) ///
		 addnotes(OLS with panel corrected standard errors.)
** Class variable

tab class, gen(dum_)

**Table3
qui xtpcse univers_oldageprog dum_1 dum_2 dum_3 dum_4 /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model14

qui xtpcse univers_mater_prog dum_1 dum_2 dum_3 dum_4 /*
*/ lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model15


qui xtpcse univers_sick_prog dum_1 dum_2 dum_3 dum_4 lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model17

qui xtpcse univers_working_prog dum_1 dum_2 dum_3 dum_4 lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model18

qui xtpcse univers_unemp_prog dum_1 dum_2 dum_3 dum_4 lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model19

qui xtpcse univers_familiy_prog dum_1 dum_2 dum_3 dum_4 lag3milsize /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year if regime !=., hetonly

est store model20

esttab model14 model15 model17 model18 model19 model20 /*
	*/ using "NEW_MAIN_TABLE3.tex", noconst  nodepvar b(a2) se replace  /*
	*/ title("Core Supporting Groups and Welfare Programs") /*
	  */ indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) /*
	  */ order(dum_2 dum_3 dum_4) ///
		 addnotes(OLS with panel corrected standard errors.)



** interaction

*** Argariam

qui xtpcse univers_oldageprog dum_1##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int1ag

qui xtpcse univers_mater_prog dum_1##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int2ag

qui xtpcse univers_sick_prog dum_1##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int3ag

qui xtpcse univers_working_prog dum_1##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int4ag

qui xtpcse univers_unemp_prog dum_1##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int5ag

qui xtpcse univers_familiy_prog dum_1##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int6ag

esttab int1ag int2ag int3ag int4ag int5ag int6ag /*
	*/ using "NEW_MAIN_TABLE3.tex", noconst  nodepvar b(a2) se replace  /*
	*/ title("Core Supporting Groups and Welfare Programs") /*
	  */ indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" )


qui xtpcse univers_oldageprog dum_2##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int1par

qui xtpcse univers_mater_prog dum_2##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int2par

qui xtpcse univers_sick_prog dum_2##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int3par

qui xtpcse univers_working_prog dum_2##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int4par

qui xtpcse univers_unemp_prog dum_2##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int5par

qui xtpcse univers_familiy_prog dum_2##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int6par

esttab int1par int2par int3par int4par int5par int6par /*
	*/ using "NEW_MAIN_TABLE4.tex", noconst  nodepvar b(a2) se replace  /*
	*/ title("Party elites $\times$ pi on Welfare Programs") /*
	  */ indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" )
	  
qui xtpcse univers_oldageprog dum_3##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int1mil

qui xtpcse univers_mater_prog dum_3##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int2mil

qui xtpcse univers_sick_prog dum_3##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int3mil

qui xtpcse univers_working_prog dum_3##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int4mil

qui xtpcse univers_unemp_prog dum_3##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int5mil

qui xtpcse univers_familiy_prog dum_3##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int6mil

esttab int1mil int2mil int3mil int4mil int5mil int6mil /*
	*/ using "NEW_MAIN_TABLE5.tex", noconst  nodepvar b(a2) se replace  /*
	*/ title("Military $\times$ pi on Welfare Programs") /*
	  */ indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" )

qui xtpcse univers_oldageprog dum_4##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int1work

qui xtpcse univers_mater_prog dum_4##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int2work

qui xtpcse univers_sick_prog dum_4##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int3work

qui xtpcse univers_working_prog dum_4##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int4work

qui xtpcse univers_unemp_prog dum_4##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int5work

qui xtpcse univers_familiy_prog dum_4##c.(lag3v2psorgs lag3v2psprbrch /*
*/lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3v2dlencmps) /*
*/ lag3gdppc lag3resdep lag3urban lag3pop14 /*
*/ lag3pop65 i.Ccodecow i.year, hetonly

est store int6work

esttab int1work int2work int3work int4work int5work int6work /*
	*/ using "NEW_MAIN_TABLE6.tex", noconst  nodepvar b(a2) se replace  /*
	*/ title("Working $\times$ pi on Welfare Programs") /*
	  */ indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" )   
