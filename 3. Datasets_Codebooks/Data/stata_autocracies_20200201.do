use "C:\Users\phere\Dropbox\Scholar\2_Graduates\2019_03_Fall\PER_R19_AutocraticWelfare\3. Datasets_Codebooks\mdata.dta" 
tsset Ccode year
**gen class dummies
tab lag3class, gen(dm_)


**Table1
**** Base model
qui xtpcse totalencomp dm_1 dm_2 dm_6 i.Ccodecow i.year /*
*/ if dm_3==0 & dm_4==0 & dm_5==0 & dm_8==0 &  dm_9==0 & dm_10==0 , hetonly
est store model1

qui xtpcse totalencomp dm_1 dm_2 dm_6 lag3milsize lag3gdppc lag3resdep/*
*/ lag3urban lag3pop14 lag3pop65 i.Ccodecow i.year /*
*/ if dm_3==0 & dm_4==0 & dm_5==0 & dm_8==0 &  dm_9==0 & dm_10==0, hetonly
est store model2

qui xtpcse totalencomp dm_1 dm_2 dm_6  /*
*/ lag3v2psorgs /*
*/ lag3milsize lag3gdppc lag3resdep/*
*/ lag3urban lag3pop14 lag3pop65 i.Ccodecow i.year /*
*/ if dm_3==0 & dm_4==0 & dm_5==0 & dm_8==0 &  dm_9==0 & dm_10==0, hetonly
est store model3

qui xtpcse totalencomp dm_1 dm_2 dm_6  /*
*/ lag3v2psprbrch /*
*/ lag3milsize lag3gdppc lag3resdep/*
*/ lag3urban lag3pop14 lag3pop65 i.Ccodecow i.year /*
*/ if dm_3==0 & dm_4==0 & dm_5==0 & dm_8==0 &  dm_9==0 & dm_10==0, hetonly
est store model4

qui xtpcse totalencomp dm_1 dm_2 dm_6  /*
*/ lag3v2psplats /*
*/ lag3milsize lag3gdppc lag3resdep/*
*/ lag3urban lag3pop14 lag3pop65 i.Ccodecow i.year /*
*/ if dm_3==0 & dm_4==0 & dm_5==0 & dm_8==0 &  dm_9==0 & dm_10==0, hetonly
est store model5

qui xtpcse totalencomp dm_1 dm_2 dm_6  /*
*/ lag3v2psprlnks /*
*/ lag3milsize lag3gdppc lag3resdep/*
*/ lag3urban lag3pop14 lag3pop65 i.Ccodecow i.year /*
*/ if dm_3==0 & dm_4==0 & dm_5==0 & dm_8==0 &  dm_9==0 & dm_10==0, hetonly
est store model6

qui xtpcse totalencomp dm_1 dm_2 dm_6  /*
*/ lag3v2pscohesv /*
*/ lag3milsize lag3gdppc lag3resdep/*
*/ lag3urban lag3pop14 lag3pop65 i.Ccodecow i.year /*
*/ if dm_3==0 & dm_4==0 & dm_5==0 & dm_8==0 &  dm_9==0 & dm_10==0, hetonly
est store model7

qui xtpcse totalencomp dm_1 dm_2 dm_6  /*
*/ lag3v2psorgs lag3v2psprbrch lag3v2psplats lag3v2psprlnks lag3v2pscohesv /*
*/ lag3milsize lag3gdppc lag3resdep/*
*/ lag3urban lag3pop14 lag3pop65 i.Ccodecow i.year /*
*/ if dm_3==0 & dm_4==0 & dm_5==0 & dm_8==0 &  dm_9==0 & dm_10==0, hetonly
est store model8

qui xtpcse totalencomp dm_1 dm_2 dm_6  /*
*/ lag3pi /*
*/ lag3milsize lag3gdppc lag3resdep/*
*/ lag3urban lag3pop14 lag3pop65 i.Ccodecow i.year /*
*/ if dm_3==0 & dm_4==0 & dm_5==0 & dm_8==0 &  dm_9==0 & dm_10==0, hetonly
est store model9


qui xtpcse totalencomp dm_1 dm_2 dm_6  /*
*/ lag3v2dlencmps /*
*/ lag3milsize lag3gdppc lag3resdep/*
*/ lag3urban lag3pop14 lag3pop65 i.Ccodecow i.year /*
*/ if dm_3==0 & dm_4==0 & dm_5==0 & dm_8==0 &  dm_9==0 & dm_10==0, hetonly
est store model10

qui xtpcse totalencomp dm_1 dm_2 dm_6  /*
*/ lag3v2dlencmps /*
*/ lag3milsize lag3gdppc lag3resdep/*
*/ lag3urban lag3pop14 lag3pop65 i.Ccodecow i.year /*
*/ if dm_3==0 & dm_4==0 & dm_5==0 & dm_8==0 &  dm_9==0 & dm_10==0, hetonly
est store model10


esttab model1 model2 model3 model4 model5 model6 model7 model8 model9 model10/*
*/ using "MAIN_TABLE1.tex", noconst label nodepvar /*
*/ title("Table 1: Party institutionalization and welfare state encompassingness") b(a2) se replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) /*
  */ order(dm_1 dm_2 dm_6 lag3v2psorgs lag3v2psprbrch lag3v2psplats lag3v2psprlnks lag3v2pscohesv lag3pi) ///
	 addnotes(OLS with panel corrected standard errors. Panel-Corrected Standard Errors reported in parentheses. /*
	 */Constant, year dummies and country dummies not displayed.)

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
	  
	  
	  
