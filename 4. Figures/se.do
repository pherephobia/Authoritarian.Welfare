qui xtpcse  univers_oldageprog lagv2psplats lagv2psprlnks lagv2dlencmps lagmilsize  laggdppc  lagresdep  lagurban  lagtrade   lagpop14  lagpop65 i.Ccodecow i.year,hetonly
est store result1
xtpcse  univers_mater_prog lagv2psplats lagv2psprlnks lagv2dlencmps lagmilsize  laggdppc  lagresdep  lagurban  lagtrade   lagpop14  lagpop65 i.Ccodecow i.year,hetonly

xtpcse  univers_sick_prog lagv2psplats lagv2psprlnks lagv2dlencmps lagmilsize  laggdppc  lagresdep  lagurban  lagtrade   lagpop14  lagpop65 i.Ccodecow i.year,hetonly
xtpcse  univers_working_prog lagv2psplats lagv2psprlnks lagv2dlencmps lagmilsize  laggdppc  lagresdep  lagurban  lagtrade   lagpop14  lagpop65 i.Ccodecow i.year,hetonly
xtpcse  univers_unemp_prog lagv2psplats lagv2psprlnks lagv2dlencmps lagmilsize  laggdppc  lagresdep  lagurban  lagtrade   lagpop14  lagpop65 i.Ccodecow i.year,hetonly
xtpcse  univers_familiy_prog lagv2psplats lagv2psprlnks lagv2dlencmps lagmilsize  laggdppc  lagresdep  lagurban  lagtrade   lagpop14  lagpop65 i.Ccodecow i.year,hetonly


coefplot result1, bylabel(Old-age) || result2, bylabel(Mater) ||/*	
*/ result3, bylabel(Sick) || result4, bylabel(Working) || result5, bylabel(Unempl.) || /*
*/result6, keep( lagv2psplats  lagv2psprlnks  lagv2dlencmps  lagmilsize  laggdppc  lagresdep  lagurban  lagtrade   lagpop14  lagpop65 )/*
*/ xline(0) xtitle("Variables") ytitle("Estimates") graphregion(fcolor(white)) coeflabels(lagv2psplats="Platform" /*
*/lagv2psprlnks="Linkages" lagv2dlencmps="Type of Goods" lagmilsize="Mil.Size" laggdppc="Ln.GDPpc" /*
*/lagresdep="Resource Dep." lagurban="Urban." lagtrade="Trade Openness" lagpop14="Youth Pop."  lagpop65="Elderly Pop.") byopts(row(1)) xsize(10) ysize(4.5)

