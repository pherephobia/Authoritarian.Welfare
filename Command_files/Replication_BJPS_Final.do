****************************************************************************
*** Replication file for ***************************************************
*************** Party Institutionalization and Welfare State Development ***
****************************************************************************
*** Rasmussen and Knutsen (2020), BJPS *************************************
****************************************************************************


use "C:\Users\carlhk\Dropbox\the origin of industrial disputes\Party Strenght and the (national) Welfare state\BJPS\FINAL_Submission\replication_BJPS.dta", clear

keep name country_name country_id country_text_id year Ccodecow v2x_polyarchy boix_regime v2dlencmps e_h_polcon5 v2elloelsy uz4ind sz4ind wdi_megdp  exp_public_order_GDPGSRE gov_left1 sstran az4ind px2indst pturatpa scovratl ucovrate acovratl px2indst v2elparlel v2xps_party v2dlunivl_ord ht_region ss100 sc100 sickcov encompassing v2dlunivl universe_all lnpop e_migdppcln e_regionpol e_cow_exports e_cow_imports e_migdppc v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv e_miurbpop e_peginiwi farm union_density_wage left_executive e_miinteco  e_Civil_War v2x_corr v2clrspct v2x_genpp v2x_cspart 

 
sort Ccodecow year
xtset Ccodecow year

*********************************************************************
*** Variable creation and transformation, including interpolation ***
*********************************************************************

bysort Ccodecow: carryforward v2elparlel, gen(v2elparlel_full)
bysort Ccodecow: carryforward v2elloelsy, gen(v2elloelsy_full) 


by Ccodecow, sort : ipolate uz4ind year, generate(uz4ind_inter)
by Ccodecow, sort : ipolate sz4ind year, generate(sz4ind_inter)
by Ccodecow, sort : ipolate az4ind year, generate(az4ind_inter)
by Ccodecow, sort : ipolate px2indst year, generate(px2indst_inter)
by Ccodecow, sort : ipolate pturatpa year, generate(pturatpa_inter)
by Ccodecow, sort : ipolate scovratl year, generate(scovratl_inter)
by Ccodecow, sort : ipolate ucovrate year, generate(ucovrate_inter)

by Ccodecow, sort : ipolate acovratl year, generate(acovratl_inter)
bysort Ccodecow: gen cov_1930 = scovratl if year==1930
bysort Ccodecow: carryforward cov_1930, gen(cov_1930_full)


label var v2xps_party "Party Institutionalization"
	 label define v2elparlel_full 1 "PR"  2 "MIXED" 

	 
sort ht_region
sort Ccodecow
bysort Ccodecow: carryforward ht_region, gen(ht_regioncf)
sort Ccodecow year
browse Ccodecow year ht_region ht_regioncf
bysort Ccodecow: carryforward ht_regioncf, replace
sort Ccodecow year
browse country_name year ht_region ht_regioncf



****************************************
***Generate neighborhood instruments ***
****************************************

generate westeurplusoffshots =.
replace westeurplusoffshots =1 if ht_regioncf==5
replace westeurplusoffshots =0 if ht_regioncf!=5 & ht_regioncf!=.

generate easteursov =.
replace easteursov =1 if ht_regioncf==1
replace easteursov =0 if ht_regioncf!=1 & ht_regioncf!=.

generate africass =.
replace africass =1 if ht_regioncf==4
replace africass =0 if ht_regioncf!=4 & ht_regioncf!=.

generate asiapac =.
replace asiapac =1 if ht_regioncf==6 | ht_regioncf==7 | ht_regioncf==8  | ht_regioncf==9
replace asiapac =0 if ht_regioncf==1 | ht_regioncf==2 | ht_regioncf==3 | ht_regioncf==4 | ht_regioncf==5 | ht_regioncf==10

generate latam =.
replace latam =1 if ht_regioncf==2 | ht_regioncf==10
replace latam =0 if ht_regioncf==1 | ht_regioncf==3 | ht_regioncf==4 | ht_regioncf==5 | ht_regioncf==6 | ht_regioncf==7 | ht_regioncf==8  | ht_regioncf==9

generate mideanafr =.
replace mideanafr =1 if ht_regioncf==3
replace mideanafr =0 if ht_regioncf!=3 & ht_regioncf!=.

egen WER=sum(v2xps_party ) if  westeurplusoffshots==1, by (year)
egen WE = sum(westeurplusoffshots) if  westeurplusoffshots==1 & v2xps_party !=., by (year)
generate WERA = (WER-v2xps_party )/(WE-1)

egen EER=sum(v2xps_party ) if  easteursov==1, by (year)
egen EE = sum(easteursov) if  easteursov==1 & v2xps_party !=., by (year)
generate EERA = (EER-v2xps_party )/(EE-1)

egen AFR=sum(v2xps_party ) if  africass==1, by (year)
egen AF = sum(africass) if  africass==1 & v2xps_party !=., by (year)
generate AFRA = (AFR-v2xps_party )/(AF-1)

egen ASR=sum(v2xps_party ) if  asiapac==1, by (year)
egen AS = sum(asiapac) if  asiapac==1 & v2xps_party !=., by (year)
generate ASRA = (ASR-v2xps_party )/(AS-1)

egen LAR=sum(v2xps_party ) if  latam==1, by (year)
egen LA = sum(latam) if  latam==1 & v2xps_party !=., by (year)
generate LARA = (LAR-v2xps_party )/(LA-1)

egen MER=sum(v2xps_party ) if  mideanafr==1, by (year)
egen ME = sum(mideanafr) if  mideanafr==1 & v2xps_party !=., by (year)
generate MERA = (MER-v2xps_party )/(ME-1)

*gerenate regional averages, excepting country in question
generate regionv2xps_party_minus = WERA
replace regionv2xps_party_minus = EERA if regionv2xps_party_minus==.
replace regionv2xps_party_minus = AFRA if regionv2xps_party_minus==.
replace regionv2xps_party_minus = ASRA if regionv2xps_party_minus==.
replace regionv2xps_party_minus = MERA if regionv2xps_party_minus==.
replace regionv2xps_party_minus = LARA if regionv2xps_party_minus==.

**pure regional averages
egen WEurPI=mean(v2xps_party ) if  westeurplusoffshots==1, by (year)
egen EEurPI=mean(v2xps_party ) if  easteursov==1, by (year)
egen AfricaPI=mean(v2xps_party ) if  africass==1, by (year)
egen AsiaPI=mean(v2xps_party ) if  asiapac==1, by (year)
egen MENAPI=mean(v2xps_party ) if  mideanafr==1, by (year)
egen LatamPI=mean(v2xps_party ) if  latam==1, by (year)

generate regionPI = WEurPI
replace regionPI = EEurPI if regionPI==.
replace regionPI = AfricaPI if regionPI==.
replace regionPI = AsiaPI if regionPI==.
replace regionPI = MENAPI if regionPI==.
replace regionPI = LatamPI if regionPI==.
rename regionPI regionv2xps_party 

**generate global PI average
egen globalv2xps_party =mean(v2xps_party ), by (year)

egen totalglobalv2xps_party =sum(v2xps_party ), by (year)
**generate global PI average minus country in question

egen globcount = sum(westeurplusoffshots+ easteursov+ africass + asiapac+ latam+ mideanafr) if v2xps_party !=., by (year)
generate globv2xps_party_minus = (totalglobalv2xps_party -v2xps_party )/(globcount-1)

browse country_name year v2xps_party  globalv2xps_party   globv2xps_party_minus regionv2xps_party_minus regionv2xps_party  WEurPI EEurPI AfricaPI AsiaPI MENAPI LatamPI
correlate v2xps_party  globalv2xps_party   globv2xps_party_minus regionv2xps_party_minus regionv2xps_party 

***generate regional and global encomp averages, minus country in question
egen WERg=sum(encompassing) if  westeurplusoffshots==1, by (year)
egen WEg = sum(westeurplusoffshots) if  westeurplusoffshots==1 & encompassing!=., by (year)
generate WERAg = (WERg-encompassing)/(WEg-1)

egen EERg=sum(encompassing) if  easteursov==1, by (year)
egen EEg = sum(easteursov) if  easteursov==1 & encompassing!=., by (year)
generate EERAg = (EERg-encompassing)/(EEg-1)

egen AFRg=sum(encompassing) if  africass==1, by (year)
egen AFg = sum(africass) if  africass==1 & encompassing!=., by (year)
generate AFRAg = (AFRg-encompassing)/(AFg-1)

egen ASRg=sum(encompassing) if  asiapac==1, by (year)
egen ASg = sum(asiapac) if  asiapac==1 & encompassing!=., by (year)
generate ASRAg = (ASRg-encompassing)/(ASg-1)

egen LARg=sum(encompassing) if  latam==1, by (year)
egen LAg = sum(latam) if  latam==1 & encompassing!=., by (year)
generate LARAg = (LARg-encompassing)/(LAg-1)

egen MERg=sum(encompassing) if  mideanafr==1, by (year)
egen MEg = sum(mideanafr) if  mideanafr==1 & encompassing!=., by (year)
generate MERAg = (MERg-encompassing)/(MEg-1)

*gerenate regional averages, excepting country in question
generate regionencomp_minus = WERAg
replace regionencomp_minus = EERAg if regionencomp_minus==.
replace regionencomp_minus = AFRAg if regionencomp_minus==.
replace regionencomp_minus = ASRAg if regionencomp_minus==.
replace regionencomp_minus = MERAg if regionencomp_minus==.
replace regionencomp_minus = LARAg if regionencomp_minus==.

**generate global average encomp minus country in question
**generate global PI average
egen globcountg = sum(westeurplusoffshots+ easteursov+ africass + asiapac+ latam+ mideanafr) if encompassing!=., by (year)
egen globalencomp = mean(encompassing) if encompassing!=., by (year)
egen totalglobalencomp=sum(encompassing), by (year)
generate globencomp_minus = (totalglobalencomp-encompassing)/(globcountg-1)

browse Ccodecow country_name year v2xps_party  globalv2xps_party   globv2xps_party_minus regionv2xps_party_minus regionv2xps_party  encompassing globalencomp globencomp_minus


pwcorr v2xps_party v2dlunivl scovratl universe_all 
pwcorr v2psprlnks v2dlunivl scovratl universe_all 


gen diffv2xps_party =d.v2xps_party
gen diffv2dlunivl =d.v2dlunivl
gen diffscovratl =d.scovratl
gen diffuniverse_all =d.universe_all
pwcorr diffv2xps_party diffv2dlunivl  universe_all if scovratl!=.

bysort e_regionpol year: egen mean_region_PI = mean(v2xps_party)
sort Ccodecow year





****************************************
****************************************
***** Figures and Tables for paper *****
****************************************
****************************************



**************
** Figure 1 **
**************

twoway (tsline mean_region_PI) if e_regionpol<9, tscale(range(1900 2015)) tlabel(1900(25)2010, angle(stdarrow)) scheme(s2mono) by(, graphregion(fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))) by(e_regionpol, style(compact) rows(2))


**************
** Figure 2 **
**************

xtline v2xps_party if country_id==186 | country_id==46| country_id==68| country_id==152, overlay i(country_id) t(year) tscale(range(1900 2015)) tlabel(1900(25)2000) scheme(s2mono) graphregion(margin(vsmall) fcolor(white) lcolor(white) ifcolor(white) ilcolor(white))

	   
**************
** Figure 3 **
**************

xtset Ccodecow year 

xtpcse  encompassing v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store analysis1	 
summarize v2xps_party if _est_analysis1==1, detail	 

generate sample1_quantile =.
replace sample1_quantile =1 if _est_analysis1==1& v2xps_party > 0 & v2xps_party<0.3017344
replace sample1_quantile =2 if  _est_analysis1==1& v2xps_party < 0.6052523 & v2xps_party>0.3017343	 
replace sample1_quantile =3 if _est_analysis1==1&  v2xps_party > 0.6052522 & v2xps_party<0.8310981	 
replace sample1_quantile =4 if _est_analysis1==1 & v2xps_party <1 & v2xps_party>0.8310980	 
summarize encompassing if _est_analysis1==1, detail	

histogram encompassing, discrete width(1) percent scheme(s2mono) by(, graphregion(margin(tiny) fcolor(none) lcolor(none) ifcolor(none) ilcolor(none))) by(sample1_quantile, style(compact) imargin(tiny) rows(2))


**************
** Figure 4 **
**************

xtpcse  v2dlunivl v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store analysis5
summarize v2dlunivl if _est_analysis5==1, detail
summarize v2xps_party if _est_analysis5==1, detail

generate sample2_quantile =.
replace sample2_quantile =1 if _est_analysis5==1& v2xps_party > 0 & v2xps_party<0.1221504
replace sample2_quantile =2 if  _est_analysis5==1& v2xps_party < 0.4568251 & v2xps_party>0.1221504	 
replace sample2_quantile =3 if _est_analysis5==1&  v2xps_party > 0.4568251 & v2xps_party<0.7591842	 
replace sample2_quantile =4 if _est_analysis5==1 & v2xps_party <1 & v2xps_party>0.7591842	

histogram v2dlunivl, discrete width(0.15)  percent scheme(s2mono) by(, graphregion(margin(tiny) fcolor(none) lcolor(none) ifcolor(none) ilcolor(none))) by(sample2_quantile, style(compact) imargin(tiny) rows(2))


**************
** Figure 5 **
**************

twoway (scatter v2dlunivl v2xps_party if year==1950, xtitle(PI score in 1950) ytitle(Universalism score in 1950) sort mlabel(country_name) mlabsize(tiny) mlabcolor(black) mlabposition(6) mlabangle(stdarrow) mlabgap(vsmall)), yscale(range(-3 3)) ylabel(-3(1)3) graphregion(margin(tiny) fcolor(none) lcolor(none) ifcolor(none) ilcolor(none)) plotregion(fcolor(none) lcolor(none) ifcolor(none) ilcolor(none))
twoway (scatter v2dlunivl v2xps_party if year==2000, xtitle(PI score in 2000) ytitle(Universalism score in 2000) sort mlabel(country_name) mlabsize(tiny) mlabcolor(black) mlabposition(6) mlabangle(stdarrow) mlabgap(vsmall)), yscale(range(-3 3)) ylabel(-3(1)3)  graphregion(margin(tiny) fcolor(none) lcolor(none) ifcolor(none) ilcolor(none)) plotregion(fcolor(none) lcolor(none) ifcolor(none) ilcolor(none))


***************
*** Table 1 ***
***************

xtpcse  encompassing v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store analysis1
summarize encompassing if _est_analysis1==1

xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store analysis2

xtpcse  d.encompassing  l.encompassing v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  v2x_polyarchy i.Ccodecow i.year     ,hetonly 
estimate store analysis3
nlcom (_b[v2xps_party]*1)/(-_b[l.encompassing])

xtpcse  v2dlunivl v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store analysis4
summarize v2dlunivl if _est_analysis5==1, detail

xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store analysis5

xtpcse  d.v2dlunivl l.v2dlunivl  v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  v2x_polyarchy i.Ccodecow i.year     ,hetonly 
estimate store analysis6
nlcom (_b[v2xps_party]*.5)/(-_b[l.v2dlunivl])

xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store analysis7

 esttab analysis1 analysis2 analysis3 analysis4 analysis5 analysis6 analysis7 using "NEW_MAIN_TABLE1.rtf", noconst label nodepvar title("Table 1: Party institutionalization and welfare state encompassingness, universality, generosity and size: main models") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(OLS with panel corrected standard errors. T-values reported in parentheses. Constant, year dummies and country dummies not displayed. Concerning the dependent variable specifications, L=Level and C=Change (from t-1 to t).)


***************
*** Table 2 ***
***************

xtpcse  v2dlunivl v2psorgs lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators1
xtpcse  v2dlunivl v2psprbrch lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators2
xtpcse  v2dlunivl v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators3	
xtpcse  v2dlunivl v2psplats lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators4
xtpcse  v2dlunivl v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators5
xtpcse  v2dlunivl v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators6

 esttab indicators1 indicators2 indicators3 indicators4 indicators5 indicators6  using "MAIN_INDICATORS_MAIN.rtf", noconst label nodepvar title("Table 2 Disaggregating party institutionalization and estimating effects on welfare state universalism") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv v2psorgs)  ///
	 addnotes(OLS with panel corrected standard errors. T-values reported in parentheses.)
	  	 

***************
*** Table 3 ***
***************


gen opensum = (e_cow_exports+ e_cow_imports)/e_migdppc

*new party inst index
gen party_sys = v2psorgs+v2psprbrch+v2psprlnks+v2psplats+v2pscohesv

pca v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv, components(1)
predict party_factor if e(sample),
pwcorr party_factor  v2xps_party


xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  v2dlunivl l5.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  v2dlunivl l10.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop opensum e_peginiwi farm   i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage left_executive   i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miinteco  e_Civil_War v2x_corr v2clrspct v2x_genpp v2x_cspart   i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  v2dlunivl party_factor lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  v2dlunivl v2xps_party v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls8
xtreg  v2dlunivl v2xps_party  lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.year    ,cluster(Ccodecow) 
estimate store controls9
xtologit  v2dlunivl_ord v2xps_party  lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.year      , vce(cluster Ccodecow) 
estimate store controls10

 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls8 controls9 controls10 using "MAIN_ROBUSTTABLE.rtf", noconst label nodepvar title("Table 3: Robustness tests on Universal programs") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(OLS with panel corrected standard errors. T-values reported in parentheses.)


***************
*** Table 4 ***
***************

xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  i.Ccodecow i.year  if boix_regime==0    ,hetonly 
estimate store demos1
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  i.Ccodecow i.year if  boix_regime==1    ,hetonly 
estimate store demos2
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if e_regionpol==5    ,hetonly 
estimate store region1
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if e_regionpol!=5        ,hetonly 
estimate store region2
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if farm>33   ,hetonly 
estimate store farm1
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if farm<33        ,hetonly 
estimate store farm2

 esttab demos1 demos2 region1 region2 farm1 farm2  using "MAIN_SPLITTSAMPLE_MAIN.rtf", noconst label nodepvar title("Table 4 Split sample tests on party institutionalization and Universal programs") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full ) ///
	 addnotes(OLS with panel corrected standard errors. T-values reported in parentheses. Regime samples are determined by the Boix et al (2013) binary measure of democracy. For high and low inequality samples we use median sample-score (33) on percentage share of farm land that is family farms (from Vanhanen 2000) as cutoff.)


	 
**************
** Figure 6 **
**************	 

xtpcse  v2dlunivl c.v2xps_party##i.boix lnpop  e_migdppcln  i.v2elparlel_full i.Ccodecow i.year     ,hetonly 
estimate store regre23
 margins, dydx(v2xps_party) at( boix_regime =(0 1)) post
 marginsplot, yline(0)
 coefplot , title("") ytitle("", ) xlabel(1 "Autocratic Regimes" 2 "Democratic Regimes")  vertical

 
**************
** Figure 7 **
**************

 xtpcse v2dlunivl c.v2xps_party##c.e_h_polcon5 lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy  i.year i.country_id, hetonly
 margins, dydx(v2xps_party) at( e_h_polcon5 =(.008 (.10) .72)) post
 marginsplot, yline(0) xtitle(Political constraints index)
 coefplot , title("") ytitle("", )   vertical yline(0) xtitle(Political constraints index) label



 ******************************
 ***** ONLINE APPENDICES ******
 ******************************


	 
************************ 
** Appendixc Table A2 **
************************
estpost sum  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  v2elparlel_full   if _est_analysis2==1
esttab using "descmodel6A.rtf",cells("count mean sd  min max") noobs replace 

estpost sum  v2dlunivl  v2xps_party lnpop  e_migdppcln v2x_polyarchy  v2elparlel_full   if _est_analysis6==1
esttab using "descmodel6B.rtf",cells("count mean sd  min max") noobs replace 

estpost sum  universe_all  v2xps_party lnpop  e_migdppcln v2x_polyarchy  v2elparlel_full   if _est_analysis7==1
esttab using "descmodel6C.rtf",cells("count mean sd  min max") noobs replace 

estpost sum  sz4ind  v2xps_party lnpop  e_migdppcln v2x_polyarchy  v2elparlel_full   if _est_analysis8==1
esttab using "descmodel6D.rtf",cells("count mean sd  min max") noobs replace 

estpost sum  scovratl  v2xps_party lnpop  e_migdppcln v2x_polyarchy  v2elparlel_full   if _est_analysis9==1
esttab using "descmodel6E.rtf",cells("count mean sd  min max") noobs replace 

estpost sum  sstran  v2xps_party lnpop  e_migdppcln v2x_polyarchy  v2elparlel_full   if _est_analysis10==1
esttab using "descmodel6J.rtf",cells("count mean sd  min max") noobs replace 

************************ 
** Appendixc Table A3 **
************************

 edit name year _est_analysis1 Ccodecow if _est_analysis1==1	 
	 
	 
************************ 
** Appendixc Table A4 **
************************	 
	 
	 
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store analysis8


xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store analysis9


 esttab analysis9 analysis10 using "AppTab4.rtf", noconst label nodepvar title("Table A4: Party institutionalization and measures of welfare state universality and generosity, using data from SCIP on sickness benefits programs (5-year panels)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(OLS with panel corrected standard errors. T-values reported in parentheses. Constant, year dummies and country dummies not displayed.)

	 
************************ 
** Appendixc Table A5 **
************************	 
	 
xtpcse  ss100  v2xps_party   i.Ccodecow i.year      ,hetonly 
estimate store CEWD1	

xtpcse  ss100 v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full    i.Ccodecow i.year      ,hetonly 
estimate store CEWD2	

xtpcse  sc100  v2xps_party   i.Ccodecow i.year      ,hetonly 
estimate store CEWD3	

xtpcse  sc100 v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full    i.Ccodecow i.year      ,hetonly 
estimate store CEWD4	

xtpcse  sickcov  v2xps_party   i.Ccodecow i.year      ,hetonly 
estimate store CEWD5	

xtpcse  sickcov v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full    i.Ccodecow i.year      ,hetonly 
estimate store CEWD6	


 esttab CEWD1 CEWD2 CEWD3 CEWD4 CEWD5 CEWD6 using "CEWD.rtf", noconst label nodepvar title("Table A5. Party institutionalization and universalism (Coverage) and generosity (Replacement rates), using CEWD data on sickness benefits programs. Time series extend from 1970-2004") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)

	 
 
************************ 
** Appendixc Table A6 **
************************

xtpcse  d.universe_all  l.universe_all v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  v2x_polyarchy i.Ccodecow i.year     ,hetonly 
estimate store analysis7

xtpcse  universe_all v2xps_party  i.Ccodecow i.year    if _est_analysis7==1  ,hetonly 
estimate store analysis1

xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year if _est_analysis7==1     ,hetonly 
estimate store analysis2


 esttab analysis1 analysis2 analysis7  using "universalism_all.rtf", noconst label nodepvar title("Table A6. Replicating main models using the SPAW Universalism Index as DV") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)

	 
************************ 
** Appendixc Table A7 **
************************
	 
xtpcse  d.v2dlencmps  l.v2dlencmps v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  v2x_polyarchy i.Ccodecow i.year     ,hetonly 
estimate store analysis7

xtpcse  v2dlencmps v2xps_party  i.Ccodecow i.year    if _est_analysis7==1  ,hetonly 
estimate store analysis1

xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year if _est_analysis7==1     ,hetonly 
estimate store analysis2


 esttab analysis1 analysis2 analysis7  using "pubgood.rtf", noconst label nodepvar title("Table A7. Replicating main models using public vs particularistic goods measure(v2dlencmps from V-Dem)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
 
 
 
************************ 
** Appendixc Table A8 **
************************
 
 xtpcse  encompassing v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small1	
xtpcse  encompassing l.encompassing v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small2
xtpcse  v2dlunivl v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small3
xtpcse  v2dlunivl l.v2dlunivl v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small4
xtpcse  universe_all v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small5
xtpcse  universe_all l.universe_all v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small6
xtpcse  sz4ind v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small7
xtpcse  sz4ind l5.sz4ind v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small8
xtpcse  scovratl v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small9
xtpcse  scovratl l5.scovratl v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small10
xtpcse  sstran v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small11
xtpcse  sstran l.sstran v2xps_party  i.Ccodecow i.year      ,hetonly 
estimate store small12


 esttab small1 small2 small3 small4 small5 small6 small7 small8 small9 small10  small11 small12 using "SMALL_LVD.rtf", noconst label nodepvar title("Party institutionalization on various welfare state measures. Models without controls") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party ) ///
	 addnotes(T-values calculated with panel corrected standard errors.)
 
 
 
************************ 
** Appendixc Table A9 **
************************ 
 
 
xtpcse  v2dlunivl v2xps_party encompassing i.Ccodecow i.year     ,hetonly  
estimate store ec1
xtpcse  v2dlunivl v2xps_party encompassing lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full i.Ccodecow i.year     ,hetonly  
estimate store ec2
xtpcse  v2dlunivl L.v2dlunivl v2xps_party encompassing lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full i.Ccodecow i.year     ,hetonly   
estimate store ec3
xtpcse  universe_all v2xps_party encompassing i.Ccodecow i.year     ,hetonly  
estimate store ec4
xtpcse  universe_all v2xps_party encompassing lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full i.Ccodecow i.year     ,hetonly  
estimate store ec5
xtpcse  universe_all L.universe_all v2xps_party encompassing lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full i.Ccodecow i.year     ,hetonly   
estimate store ec6 
 esttab ec1 ec2 ec3 ec4 ec5 ec6 using "controllencomp.rtf", noconst label nodepvar title("Table A9. Party institutionalization on welfare state universalism measures holding welfare state size constant.") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party ) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors. Constant, fixed time, country effects and controls (GDP [Log], population [Log], electoral rules and Polyarchy) excluded.) 
 
 
************************* 
** Appendixc Table A10 **
************************* 
 
xtpcse  encompassing v2psorgs lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators1
xtpcse  encompassing v2psprbrch lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators2
xtpcse  encompassing v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators3	
xtpcse  encompassing v2psplats lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators4
xtpcse  encompassing v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators5
xtpcse  encompassing v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators6


 esttab indicators1 indicators2 indicators3 indicators4 indicators5 indicators6   using "MAIN_INDICATORS_MAINr1.rtf", noconst label nodepvar title("Party institutionalization indicators on Encompassingess (from SPAW)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv v2psorgs)  ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)

	 
************************* 
** Appendixc Table A11 **
************************* 
			 
xtpcse  universe_all v2psorgs lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators1
xtpcse  universe_all v2psprbrch lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators2
xtpcse  universe_all v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators3	
xtpcse  universe_all v2psplats lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators4
xtpcse  universe_all v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators5
xtpcse  universe_all v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators6


 esttab indicators1 indicators2 indicators3 indicators4 indicators5 indicators6   using "MAIN_INDICATORS_MAINr2.rtf", noconst label nodepvar title("Table A11. Party institutionalization indicators on SPAW Universalism Index") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv v2psorgs)  ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	  	 	 
	 
************************* 
** Appendixc Table A12 **
************************* 
	 
xtpcse  sz4ind v2psorgs lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators1
xtpcse  sz4ind v2psprbrch lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators2
xtpcse  sz4ind v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators3	
xtpcse  sz4ind v2psplats lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators4
xtpcse  sz4ind v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators5
xtpcse  sz4ind v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators6


 esttab indicators1 indicators2 indicators3 indicators4 indicators5 indicators6   using "MAIN_INDICATORS_MAINr3.rtf", noconst label nodepvar title("Table A12. Party institutionalization on replacement rates for sickness insurance (from SCIP)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv v2psorgs)  ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	  	 	 

************************* 
** Appendixc Table A13 **
************************* 			 
			 
xtpcse  scovratl v2psorgs lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators1
xtpcse  scovratl v2psprbrch lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators2
xtpcse  scovratl v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators3	
xtpcse  scovratl v2psplats lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators4
xtpcse  scovratl v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators5
xtpcse  scovratl v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators6


 esttab indicators1 indicators2 indicators3 indicators4 indicators5 indicators6   using "MAIN_INDICATORS_MAINr4.rtf", noconst label nodepvar title("Table A13. Party institutionalization indicators on coverage rates for sickness insurance (from SCIP)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv v2psorgs)  ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	
	
************************* 
** Appendixc Table A14 **
************************* 	
	
xtpcse  sstran v2psorgs lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators1
xtpcse  sstran v2psprbrch lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators2
xtpcse  sstran v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators3	
xtpcse  sstran v2psplats lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators4
xtpcse  sstran v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators5
xtpcse  sstran v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators6


 esttab indicators1 indicators2 indicators3 indicators4 indicators5 indicators6   using "MAIN_INDICATORS_MAINr5.rtf", noconst label nodepvar title("Table A14. Party institutionalization indicators on social security spending as share of GDP (from CPDS)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv v2psorgs)  ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)	 
	 

************************* 
** Appendixc Table A15 **
************************* 	 
	 
xtpcse  v2dlencmps v2psorgs lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators1
xtpcse  v2dlencmps v2psprbrch lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators2
xtpcse  v2dlencmps v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators3	
xtpcse  v2dlencmps v2psplats lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators4
xtpcse  v2dlencmps v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators5
xtpcse  v2dlencmps v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store indicators6


 esttab indicators1 indicators2 indicators3 indicators4 indicators5 indicators6   using "MAIN_INDICATORS_MAINr6.rtf", noconst label nodepvar title("Party institutionalization indicators on public goods measure (from V-Dem)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2psorgs v2psprbrch v2psprlnks v2psplats v2pscohesv v2psorgs)  ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)	 
	 
	 
************************* 
** Appendixc Table A16 **
************************* 

xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  encompassing l5.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  encompassing l10.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop opensum e_peginiwi farm   i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage left_executive   i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miinteco  e_Civil_War v2x_corr v2clrspct v2x_genpp v2x_cspart   i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  encompassing party_factor lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  encompassing v2xps_party v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls8
xtreg  encompassing v2xps_party  lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.year    ,cluster(Ccodecow) 
estimate store controls9


 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls8 controls9  using "MAIN_ROBUSTTABLEr7.rtf", noconst label nodepvar title("Party institutionalization on Encompassingness (from SPAW)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	 	 

************************* 
** Appendixc Table A17 **
************************* 		 
		 
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  universe_all l5.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  universe_all l10.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop opensum e_peginiwi farm   i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage left_executive   i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miinteco  e_Civil_War v2x_corr v2clrspct v2x_genpp v2x_cspart   i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  universe_all party_factor lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  universe_all v2xps_party v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls8
xtreg  universe_all v2xps_party  lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.year    ,cluster(Ccodecow) 
estimate store controls9


 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls8 controls9 using "MAIN_ROBUSTTABLEr8.rtf", noconst label nodepvar title("Party institutionalization on Universalism Index (from SPAW)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	 	 
************************* 
** Appendixc Table A18 **
************************* 
		 
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  sz4ind l5.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  sz4ind l10.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop opensum e_peginiwi farm   i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage left_executive   i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full   v2x_corr v2clrspct v2x_genpp v2x_cspart   i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  sz4ind party_factor lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  sz4ind v2xps_party v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls8
xtreg  sz4ind v2xps_party  lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.year    ,cluster(Ccodecow) 
estimate store controls9


 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls8 controls9 using "MAIN_ROBUSTTABLEr9.rtf", noconst label nodepvar title("Table A18. Party Institutionalization on replacement rates for sickness insurance (from SCIP)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	 	 		 
	
************************* 
** Appendixc Table A19 **
************************* 
	
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  scovratl l5.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  scovratl l10.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop opensum e_peginiwi farm   i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage left_executive   i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full   v2x_corr v2clrspct v2x_genpp v2x_cspart   i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  scovratl party_factor lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  scovratl v2xps_party v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls8
xtreg  scovratl v2xps_party  lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.year    ,cluster(Ccodecow) 
estimate store controls9


 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls8 controls9 using "MAIN_ROBUSTTABLEr10.rtf", noconst label nodepvar title("Party Institutionalization on coverage rates for sickness insurance (from SCIP)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)

	 
************************* 
** Appendixc Table A20 **
************************* 
		 		 
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  sstran l5.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  sstran l10.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop opensum e_peginiwi farm   i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage left_executive   i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full   v2x_corr v2clrspct v2x_genpp v2x_cspart   i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  sstran party_factor lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  sstran v2xps_party v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls8
xtreg  sstran v2xps_party  lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.year    ,cluster(Ccodecow) 
estimate store controls9


 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls8 controls9 using "MAIN_ROBUSTTABLEr11.rtf", noconst label nodepvar title("Table A20. Party institutionalization on social expenditure as share of GDP (from CPDS)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	 	 		 		 

************************* 
** Appendixc Table A21 **
************************* 				 
						 
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  v2dlencmps l5.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  v2dlencmps l10.v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop opensum e_peginiwi farm   i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage left_executive   i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miinteco  e_Civil_War v2x_corr v2clrspct v2x_genpp v2x_cspart   i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  v2dlencmps party_factor lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  v2dlencmps v2xps_party v2psprlnks lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls8
xtreg  v2dlencmps v2xps_party  lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.year    ,cluster(Ccodecow) 
estimate store controls9


 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls8 controls9  using "MAIN_ROBUSTTABLEr12.rtf", noconst label nodepvar title("Table A21. Party institutionalization on public goods (v2dlencmps from V-Dem)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)

************************* 
** Appendixc Table A22 **
************************* 
	
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop   i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full opensum    i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_peginiwi    i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full farm    i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage    i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full left_executive    i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miinteco    i.Ccodecow i.year      ,hetonly 
estimate store controls8
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_Civil_War    i.Ccodecow i.year      ,hetonly 
estimate store controls9
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_corr    i.Ccodecow i.year      ,hetonly 
estimate store controls10
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2clrspct    i.Ccodecow i.year      ,hetonly 
estimate store controls11
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_genpp    i.Ccodecow i.year      ,hetonly 
estimate store controls12
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_cspart    i.Ccodecow i.year      ,hetonly 
estimate store controls13




 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls8 controls9 controls10 controls11 controls12 controls13 using "MAIN_ROBUSTTABLEr14.rtf", noconst label nodepvar title("Table A22. Party institutionalization on coverage of major risks (Encompassingness, from SPAW) including one control at a time") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	 	 		
	 
************************* 
** Appendixc Table A23 **
************************* 	 

xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop   i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full opensum    i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_peginiwi    i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full farm    i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage    i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full left_executive    i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miinteco    i.Ccodecow i.year      ,hetonly 
estimate store controls8
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_Civil_War    i.Ccodecow i.year      ,hetonly 
estimate store controls9
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_corr    i.Ccodecow i.year      ,hetonly 
estimate store controls10
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2clrspct    i.Ccodecow i.year      ,hetonly 
estimate store controls11
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_genpp    i.Ccodecow i.year      ,hetonly 
estimate store controls12
xtpcse  v2dlunivl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_cspart    i.Ccodecow i.year      ,hetonly 
estimate store controls13




 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls8 controls9 controls10 controls11 controls12 controls13   using "MAIN_ROBUSTTABLEr13.rtf", noconst label nodepvar title("Table A23. Party institutionalization and universalism (v2dlunivl, from V-Dem) including one control at a time.") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	 	 	
	

************************* 
** Appendixc Table A24 **
************************* 
	
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop   i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full opensum    i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_peginiwi    i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full farm    i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage    i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full left_executive    i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miinteco    i.Ccodecow i.year      ,hetonly 
estimate store controls8
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_Civil_War    i.Ccodecow i.year      ,hetonly 
estimate store controls9
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_corr    i.Ccodecow i.year      ,hetonly 
estimate store controls10
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2clrspct    i.Ccodecow i.year      ,hetonly 
estimate store controls11
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_genpp    i.Ccodecow i.year      ,hetonly 
estimate store controls12
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_cspart    i.Ccodecow i.year      ,hetonly 
estimate store controls13




 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls8 controls9 controls10 controls11 controls12 controls13   using "MAIN_ROBUSTTABLEr15.rtf", noconst label nodepvar title("Table A24. Party institutionalization and universalism (Universalism Index; SPAW) including one control at a time") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	 	 	
************************* 
** Appendixc Table A25 **
************************* 
			
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop   i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full opensum    i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_peginiwi    i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full farm    i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage    i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full left_executive    i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_corr    i.Ccodecow i.year      ,hetonly 
estimate store controls10
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2clrspct    i.Ccodecow i.year      ,hetonly 
estimate store controls11
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_genpp    i.Ccodecow i.year      ,hetonly 
estimate store controls12
xtpcse  sz4ind v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_cspart    i.Ccodecow i.year      ,hetonly 
estimate store controls13


 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls10 controls11 controls12 controls13   using "MAIN_ROBUSTTABLEr16.rtf", noconst label nodepvar title("Table A25. Party institutionalization and replacement rates for sickness insurance (from SCIP) including one control at a time") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	 	 	

************************* 
** Appendixc Table A26 **
************************* 			
			
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop   i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full opensum    i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_peginiwi    i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full farm    i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage    i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full left_executive    i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_corr    i.Ccodecow i.year      ,hetonly 
estimate store controls10
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2clrspct    i.Ccodecow i.year      ,hetonly 
estimate store controls11
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_genpp    i.Ccodecow i.year      ,hetonly 
estimate store controls12
xtpcse  scovratl v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_cspart    i.Ccodecow i.year      ,hetonly 
estimate store controls13


 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls10 controls11 controls12 controls13   using "MAIN_ROBUSTTABLEr17.rtf", noconst label nodepvar title("Table A26. Party institutionalization and coverage rates for sickness insurance (from SCIP) including one control at a time") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	 	 		
	
************************* 
** Appendixc Table A27 **
************************* 
	
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop   i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full opensum    i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_peginiwi    i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full farm    i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage    i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full left_executive    i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_corr    i.Ccodecow i.year      ,hetonly 
estimate store controls10
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2clrspct    i.Ccodecow i.year      ,hetonly 
estimate store controls11
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_genpp    i.Ccodecow i.year      ,hetonly 
estimate store controls12
xtpcse  sstran v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_cspart    i.Ccodecow i.year      ,hetonly 
estimate store controls13


 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls10 controls11 controls12 controls13   using "MAIN_ROBUSTTABLEr18.rtf", noconst label nodepvar title("Party institutionalization and social expenditures as share of GDP (from CPDS) including one control at a time") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	 	 		

				
************************* 
** Appendixc Table A28 **
************************* 
				
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store controls1
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miurbpop   i.Ccodecow i.year      ,hetonly 
estimate store controls2
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full opensum    i.Ccodecow i.year      ,hetonly 
estimate store controls3
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_peginiwi    i.Ccodecow i.year      ,hetonly 
estimate store controls4
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full farm    i.Ccodecow i.year      ,hetonly 
estimate store controls5
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full union_density_wage    i.Ccodecow i.year      ,hetonly 
estimate store controls6
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full left_executive    i.Ccodecow i.year      ,hetonly 
estimate store controls7
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_miinteco    i.Ccodecow i.year      ,hetonly 
estimate store controls8
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full e_Civil_War    i.Ccodecow i.year      ,hetonly 
estimate store controls9
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_corr    i.Ccodecow i.year      ,hetonly 
estimate store controls10
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2clrspct    i.Ccodecow i.year      ,hetonly 
estimate store controls11
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_genpp    i.Ccodecow i.year      ,hetonly 
estimate store controls12
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full v2x_cspart    i.Ccodecow i.year      ,hetonly 
estimate store controls13




 esttab controls1 controls2 controls3 controls4 controls5 controls6 controls7 controls8 controls9 controls10 controls11 controls12 controls13   using "MAIN_ROBUSTTABLEr19.rtf", noconst label nodepvar title("Table A28. Party institutionalization and Public Goods (v2dlencmps from V-Dem) including one control at a time") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	 	 	
	
	
************************* 
** Appendixc Table A29 **
************************* 

		 
xtpcse  encompassing v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  i.Ccodecow i.year  if boix_regime==0    ,hetonly 
estimate store demos1
xtpcse  encompassing v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  i.Ccodecow i.year if  boix_regime==1    ,hetonly 
estimate store demos2
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if e_regionpol==5    ,hetonly 
estimate store region1
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if e_regionpol!=5        ,hetonly 
estimate store region2
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if farm>33   ,hetonly 
estimate store farm1
xtpcse  encompassing v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if farm<33        ,hetonly 
estimate store farm2

 esttab demos1 demos2 region1 region2 farm1 farm2  using "MAIN_SPLITTSAMPLE_MAINr20.rtf", noconst label nodepvar title("Table A29. Party institutionalization on coverage of major welfare risks (Encompassingness; SPAW)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full ) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
		 
		 
************************* 
** Appendixc Table A30 **
************************* 
		 
xtpcse  universe_all v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  i.Ccodecow i.year  if boix_regime==0    ,hetonly 
estimate store demos1
xtpcse  universe_all v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  i.Ccodecow i.year if  boix_regime==1    ,hetonly 
estimate store demos2
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if e_regionpol==5    ,hetonly 
estimate store region1
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if e_regionpol!=5        ,hetonly 
estimate store region2
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if farm>33   ,hetonly 
estimate store farm1
xtpcse  universe_all v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if farm<33        ,hetonly 
estimate store farm2

 esttab demos1 demos2 region1 region2 farm1 farm2  using "MAIN_SPLITTSAMPLE_MAINr21.rtf", noconst label nodepvar title("Table A30. Testing for heterogeneity: Party institutionalization and SPAW Universalism Index") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full ) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
		 

************************* 
** Appendixc Table A31 **
************************* 		 
		 
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  i.Ccodecow i.year  if boix_regime==0    ,hetonly 
estimate store demos1
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  i.Ccodecow i.year if  boix_regime==1    ,hetonly 
estimate store demos2
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if e_regionpol==5    ,hetonly 
estimate store region1
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if e_regionpol!=5        ,hetonly 
estimate store region2
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if farm>33   ,hetonly 
estimate store farm1
xtpcse  v2dlencmps v2xps_party lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year  if farm<33        ,hetonly 
estimate store farm2

 esttab demos1 demos2 region1 region2 farm1 farm2  using "MAIN_SPLITTSAMPLE_MAINr22.rtf", noconst label nodepvar title("Testing for heterogeneity: Party institutionalization and Public Goods (v2dlencmps; V-Dem)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full ) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
		 
		 
************************* 
** Appendixc Table A32 **
*************************  		 
		 
		 
pca v2psorgs v2psprbrch  v2psplats v2pscohesv, components(1)
predict party_w_link if e(sample),
pwcorr party_factor party_w_link  v2xps_party	 
		 
		
	
xtpcse  encompassing party_w_link  i.Ccodecow i.year      ,hetonly 
estimate store analysis1
summarize encompassing if _est_analysis1==1

xtpcse  encompassing party_w_link lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store analysis2

xtpcse  d.encompassing  l.encompassing party_w_link lnpop  e_migdppcln  i.v2elparlel_full  v2x_polyarchy i.Ccodecow i.year     ,hetonly 
estimate store analysis4


xtpcse  v2dlunivl party_w_link  i.Ccodecow i.year      ,hetonly 
estimate store analysis5
summarize v2dlunivl if _est_analysis5==1, detail

xtpcse  v2dlunivl party_w_link lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store analysis6


xtpcse  d.v2dlunivl l.v2dlunivl  party_w_link lnpop  e_migdppcln  i.v2elparlel_full  v2x_polyarchy i.Ccodecow i.year     ,hetonly 
estimate store analysis8


xtpcse  universe_all party_w_link lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store analysis9

xtpcse  sz4ind party_w_link lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store analysis10


xtpcse  scovratl party_w_link lnpop  e_migdppcln v2x_polyarchy  i.v2elparlel_full  i.Ccodecow i.year      ,hetonly 
estimate store analysis11

xtpcse  sstran party_w_link lnpop  e_migdppcln   i.v2elparlel_full v2x_polyarchy  i.Ccodecow i.year      ,hetonly 
estout  , cells(b(star fmt(%9.3f)) t(par fmt(%9.2f))) 
estimate store analysis12

 esttab analysis1 analysis2  analysis4 analysis5 analysis6 analysis8 analysis9 analysis10 analysis11  analysis12 using "NEW_MAIN_TABLE132.rtf", noconst label nodepvar title("Table A32. Version of Party Institutionalization calculated without v2psprlnks regressed on various welfare measures") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(party_w_link  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
	
		
	
************************* 
** Appendixc Table A33**
************************* 


	 ** Military
	 xtpcse  wdi_megdp   v2xps_party  i.Ccodecow i.year     ,hetonly 
     estimate store placebo1
		 xtpcse  wdi_megdp   v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  v2x_polyarchy i.Ccodecow i.year     ,hetonly 
     estimate store placebo2
		 xtpcse  d.wdi_megdp l.wdi_megdp  v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  v2x_polyarchy i.Ccodecow i.year     ,hetonly 
     estimate store placebo3

	
	 ** public order
	 	 xtpcse  exp_public_order_GDPGSRE   v2xps_party  i.Ccodecow i.year     ,hetonly 
     estimate store placebo4
		 xtpcse  exp_public_order_GDPGSRE   v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  v2x_polyarchy i.Ccodecow i.year     ,hetonly 
     estimate store placebo5
		 xtpcse  d.exp_public_order_GDPGSRE l.exp_public_order_GDPGSRE  v2xps_party lnpop  e_migdppcln  i.v2elparlel_full  v2x_polyarchy i.Ccodecow i.year     ,hetonly 
     estimate store placebo6

	

 esttab placebo1 placebo2 placebo3 placebo4 placebo5 placebo6 using "placebo.rtf", noconst label nodepvar title("Table A33. Placebo tests on v2dlunivl (from V-Dem)") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
		
	
************************* 
** Appendixc Table A34 **
************************* 



xtpcse  v2dlunivl  v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_peginiwi>  41     ,hetonly 
estimate store gini1 
xtpcse  v2dlunivl  v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_peginiwi<  41      ,hetonly 
estimate store gini2


 esttab gini1 gini2 using "gini.rtf", noconst label nodepvar title("Table A34. Party institutionalization and universalism (v2dlunivl from V-Dem), split sample according to income inequality Gini coefficients.") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors.)
		

tabulate e_regionpol, generate(regional_super)

gen west_super =.
replace west_super = 1 if e_regionpol==5
replace west_super = 0 if e_regionpol!=. & e_regionpol!=5

************************* 
** Appendixc Table A35 **
************************* 
	
xtpcse  v2dlunivl  c.v2xps_party##c.farms lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_peginiwi<  41      ,hetonly 
estimate store het1
xtpcse  v2dlunivl  c.v2xps_party##c.e_peginiwi lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_peginiwi<  41      ,hetonly 
estimate store het2	
xtpcse  v2dlunivl  c.v2xps_party##i.west_super lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_peginiwi<  41      ,hetonly 
estimate store het3
	
	
 esttab het1 het2 het3 using "het.rtf", noconst label nodepvar title("Table A35. Party institutionalization and universalism (v2dlunivl: V-Dem). Interaction specifications.") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values (in parentheses) calculated with panel corrected standard errors. Constant, year- and country-fixed effects, and controls (GDP [Log], population [Log], Polyarchy index, electoral system dummies) omitted from table.)

************************* 
** Appendixc Table A36 **
************************* 	 
	 
xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year    ,hetonly 
	estimate store reg
xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_regionpol!=1      ,hetonly 
estimate store reg1
xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_regionpol!=2      ,hetonly 
estimate store reg2
xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_regionpol!=3      ,hetonly 
estimate store reg3
xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_regionpol!=4      ,hetonly 
estimate store reg4
xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_regionpol!=5      ,hetonly 
estimate store reg5
xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_regionpol!=6      ,hetonly 
estimate store reg6
xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_regionpol!=7      ,hetonly 
estimate store reg7
xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_regionpol!=8      ,hetonly 
estimate store reg8
xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_regionpol!=9      ,hetonly 
estimate store reg9
xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if e_regionpol!=10      ,hetonly 
estimate store reg10
	
 esttab reg reg1 reg2 reg3 reg4 reg5 reg6 reg7 reg8 reg9 reg10 using "reg.rtf", noconst label nodepvar title("Table A36. Party institutionalization on universalism (v2dlunivl from V-Dem), omitting regions from sample") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values calculated with panel corrected standard errors. Constant, fixed time, country effects and controls (GDP [Log], population [Log] electoral rules, and Polyarchy) excluded. * p < 0.05, ** p < 0.01, *** p < 0.001)
	
	
************************* 
** Appendixc Table A37 **
************************* 

	xtpcse  v2dlunivl  c.v2xps_party  i.Ccodecow i.year  if regional_super2==1      ,hetonly 
	estimate store latin1
	xtpcse  v2dlunivl  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if regional_super2==1      ,hetonly 
	estimate store latin2
	xtpcse  encompassing  c.v2xps_party  i.Ccodecow i.year  if regional_super2==1      ,hetonly 
	estimate store latin3
	xtpcse  encompassing  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if regional_super2==1      ,hetonly 
	estimate store latin4
	xtpcse  universe_all  c.v2xps_party  i.Ccodecow i.year  if regional_super2==1      ,hetonly 
	estimate store latin5
	xtpcse  universe_all  c.v2xps_party lnpop  e_migdppcln  i.v2elparlel_full v2x_polyarchy i.Ccodecow i.year  if regional_super2==1      ,hetonly 
	estimate store latin6

	 esttab latin1 latin2 latin3 latin4 latin5 latin6 using "latin.rtf", noconst label nodepvar title("Table A37. Party institutionalization on various welfare state measures, with sample restricted to Latin American countries") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party  v2elparlel_full v2x_polyarchy) ///
	 addnotes(T-values calculated with panel corrected standard errors.)
	 
	 
	 
	 
	 
*******************************************************	 
*****
** Appx A11 General sensitivity analysisysis here	 
*****	 
*******************************************************	 
	 
	 
	 
************************* 
** Appendixc Table A38 **
************************* 

xtpcse  v2dlunivl c.v2xps_party##c.gov_left1 v2x_polyarchy lnpop  e_migdppcln  i.v2elparlel_full i.Ccodecow i.year     ,hetonly 
estimate store regre23
xtpcse  v2dlunivl c.v2xps_party##left_executive v2x_polyarchy lnpop  e_migdppcln  i.v2elparlel_full i.Ccodecow i.year    ,hetonly 
estimate store regre24
xtpcse  v2dlunivl c.v2xps_party##c.union_density_wage v2x_polyarchy lnpop  e_migdppcln  i.v2elparlel_full i.Ccodecow i.year    ,hetonly 
estimate store regre25


 esttab regre23 regre24 regre25  using "interactions.rtf", noconst label nodepvar title("Table A38 The effect of party institutionalization conditioned by ideology and labor market organization on Universalism  )") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party)  ///
	 addnotes(t statistics in parentheses. T-values calculated with panel corrected standard errors. Constant, fixed time, country effects and controls (GDP [Log] and population [Log]) excluded.)

	 
************************* 
** Appendixc Table A39 **
************************* 
	 by Ccodecow: gen cum_polyarchy=sum(v2x_polyarchy)  
xtpcse  v2dlunivl c.v2xps_party  cum_polyarchy lnpop  e_migdppcln  i.v2elparlel_full i.Ccodecow i.year     ,hetonly 
estimate store analysis2
xtpcse  d.v2dlunivl  l.v2dlunivl cum_polyarchy v2xps_party lnpop  e_migdppcln  i.v2elparlel_full   i.Ccodecow i.year     ,hetonly 
estimate store analysis3
nlcom (_b[v2xps_party]*1)/(-_b[l.v2dlunivl])
 
 esttab analysis2 analysis3  using "polyarchy_cum.rtf", noconst label nodepvar title("Table A39 The effect of party institutionalization on welfare state Universalism") b(a2) replace ///
  indicate("Country Dummies = *Ccodecow*" "Year Dummies = *year*" ) order(v2xps_party)  ///
	 addnotes(t statistics in parentheses. T-values calculated with panel corrected standard errors. Constant, fixed time, country effects and controls (GDP [Log] and population [Log]) excluded.)

save "C:\Users\carlhk\Dropbox\the origin of industrial disputes\Party Strenght and the (national) Welfare state\BJPS\FINAL_Submission\replication_BJPS.dta", replace	 
