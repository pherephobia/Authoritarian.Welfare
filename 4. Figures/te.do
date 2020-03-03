xtset Ccodecow year
gen cash = univers_oldageprog + univers_sick_prog + univers_unemp_prog
gen noncash = univers_mater_prog + univers_working_prog + univers_familiy_prog
xtpcse cash c.(lagv2psplats lagv2psprlnks lagv2dlencmps)##b4.lagregime lagmilsize  laggdppc lagresdep  lagurban  lagtrade   lagpop14  lagpop65 i.Ccodecow i.year, hetonly
est store intmodel1
coefplot intmodel1, keep( c.(lagv2psplats lagv2psprlnks lagv2dlencmps)##b4.lagregime)/*
*/ xline(0) xtitle("Variables") ytitle("Estimates") graphregion(fcolor(white))




 coeflabels(lagv2psplats="Platform" /*
*/lagv2psprlnks="Linkages" lagv2dlencmps="Type of Goods" lagmilsize="Mil.Size" laggdppc="Ln.GDPpc" /*
*/lagresdep="Resource Dep." lagurban="Urban." lagtrade="Trade Openness" lagpop14="Youth Pop."  lagpop65="Elderly Pop.") byopts(row(1)) xsize(10) ysize(4.5)
