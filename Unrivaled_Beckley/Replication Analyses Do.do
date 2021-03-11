*Allen and DiGiuseppe (2013), Table 2, Model 1
clear
use "Allen_DiGi_Rep.dta"
set more off
merge m:m ccode year using "power2.dta"
drop if y==. | gdp==.
quietly probit  formationB2N cntgovtdbtgdp1 cinc sumrival1 wovers_1 atwar5 ayr ayr2 ayr3, robust
estat ic
quietly probit  formationB2N cntgovtdbtgdp1 y sumrival1 wovers_1 atwar5 ayr ayr2 ayr3, robust
estat ic
quietly probit  formationB2N cntgovtdbtgdp1 gdp sumrival1 wovers_1 atwar5 ayr ayr2 ayr3, robust
estat ic


*Carter and Poast (2017), Table 4, Model 1
clear
use "Carter.dta"
set more off
rename ccode1 ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename ccode2 ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac=y1/y2
replace yfrac=y2/y1 if yfrac<1
gen gdpfrac=gdp1/gdp2
replace gdpfrac=gdp2/gdp1 if gdpfrac<1
gen lnyfrac=ln(yfrac)
gen lngdpfrac=ln(gdpfrac)
drop if lnyfrac==. | lngdpfrac==. | ln_cap_ratio==.
quielty xtlogit builder_a ln_gdppc_ratioD auto1_dem2 dem1_auto2 autodyad ln_distance atopally ln_cap_ratio cw_b terr_dispute_M, re
estat ic
quietly xtlogit builder_a ln_gdppc_ratioD auto1_dem2 dem1_auto2 autodyad ln_distance atopally lnyfrac cw_b terr_dispute_M, re
estat ic
quietly xtlogit builder_a ln_gdppc_ratioD auto1_dem2 dem1_auto2 autodyad ln_distance atopally lngdpfrac cw_b terr_dispute_M, re
estat ic


*Carter, Bernhard, and Palmer (2012), Table 2, Model 5
clear
use "Carter2.dta"
set more off
rename ccode1 ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename ccode2 ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac = y1/(y1+y2)
gen gdpfrac=gdp1/(gdp1+gdp2)
drop if yfrac==. | gdpfrac==.
quietly oprobit TriOutcome PR1 PR2 polity21 polity22 RelCap, vce(cluster dyad)
estat ic
quietly oprobit TriOutcome PR1 PR2 polity21 polity22 yfrac, vce(cluster dyad)
estat ic
quietly oprobit TriOutcome PR1 PR2 polity21 polity22 gdpfrac, vce(cluster dyad)
estat ic


*Clay and Owsiak (2016), Table 1, Model 4
clear
use "Clay Owsiak JOP Data_May2015.dta"
set more off
rename ccode1 ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename ccode2 ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac=y1/y2
replace yfrac=y2/y1 if yfrac>1
gen gdpfrac=gdp1/gdp2
replace gdpfrac=gdp2/gdp1 if gdpfrac>1
gen lnyfrac=ln(yfrac)
gen lngdpfrac=ln(gdpfrac)
drop if lnyfrac==. | lngdpfrac==.
tsset cdyad year
quietly logit settlem lag_row_wsettlem1 L.noothersetyr L.nothersetyr2 L.noothersetyr3  L.anytp sumneg maxicowsal0 cow_civilwarcombine jtdem6 lncincratio atopally crt07riv dyad_ldrduration totalrivals_nondyad postww1 postww2 nosettleyr nosetyr2 nosetyr3 if L.settlem==0, robust
estat ic
quietly logit settlem lag_row_wsettlem1 L.noothersetyr L.nothersetyr2 L.noothersetyr3  L.anytp sumneg maxicowsal0 cow_civilwarcombine jtdem6 lnyfrac atopally crt07riv dyad_ldrduration totalrivals_nondyad postww1 postww2 nosettleyr nosetyr2 nosetyr3 if L.settlem==0, robust
estat ic
quietly logit settlem lag_row_wsettlem1 L.noothersetyr L.nothersetyr2 L.noothersetyr3  L.anytp sumneg maxicowsal0 cow_civilwarcombine jtdem6 lngdpfrac atopally crt07riv dyad_ldrduration totalrivals_nondyad postww1 postww2 nosettleyr nosetyr2 nosetyr3 if L.settlem==0, robust
estat ic


*Colgan and Weeks (2015), Table 2, Model 1
clear
use "Colgan.dta", clear
set more off
merge m:m ccode year using "power2.dta"
drop if y==. | gdp==.
gen ylag=y[_n-1]
gen gdplag=gdp[_n-1]
quietly nbreg initmid revpers10_lag rev10_lag pers10_lag logopengled_lag total_lag cap_lag totalally_lag civilwar_lag majpow_lag pcyrsinitmid pcyrsinitmids*,  cluster(ccode)
estat ic
quietly nbreg initmid revpers10_lag rev10_lag pers10_lag logopengled_lag total_lag ylag totalally_lag civilwar_lag majpow_lag pcyrsinitmid pcyrsinitmids*,  cluster(ccode)
estat ic
quietly nbreg initmid revpers10_lag rev10_lag pers10_lag logopengled_lag total_lag gdplag totalally_lag civilwar_lag majpow_lag pcyrsinitmid pcyrsinitmids*,  cluster(ccode)
estat ic


*Early (2012), Table 1, Model 6
clear
use "Early.DTA"
set more off
gen usa=2
rename thirdcode ccode
merge m:m ccode year using "power2.dta"
rename y y3
renam gdp gdp3
rename ccode thirdcode
drop _merge
rename usa ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode usa
drop _merge
gen lnyratio=ln(y1/y3)
gen lngdpratio=ln(gdp1/gdp3)
drop if lnyratio==. | lngdpratio==.
sort SanctDyad year 
quietly xtreg lnHSEDiff lagtradeshare3 DefPactT3 DefPactUS3 USDPxTS3 l.lnHSEDiff rivalT3  rivalUS3  lnUS3CapRatio majpow3 postCW mzongo jointdem lagopen3 lagopenT lagtottraT_s1 Cosanctioner IOsupport ExpSanct ImpSanct FinSanct SanctYears , fe robust
estat ic
quietly xtreg lnHSEDiff lagtradeshare3 DefPactT3 DefPactUS3 USDPxTS3 l.lnHSEDiff rivalT3  rivalUS3  lnyratio majpow3 postCW mzongo jointdem lagopen3 lagopenT lagtottraT_s1 Cosanctioner IOsupport ExpSanct ImpSanct FinSanct SanctYears , fe robust
estat ic
quietly xtreg lnHSEDiff lagtradeshare3 DefPactT3 DefPactUS3 USDPxTS3 l.lnHSEDiff rivalT3  rivalUS3  lngdpratio majpow3 postCW mzongo jointdem lagopen3 lagopenT lagtottraT_s1 Cosanctioner IOsupport ExpSanct ImpSanct FinSanct SanctYears , fe robust
estat ic


*Findley, Piazza, Young (2012), Table 1, Model 1  ("JOP-Results-Errata.doc)  	
use "Findley.dta", clear
set more off
rename cowcode1 ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename cowcode2 ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen logyratio=log(y1/y2)
gen loggdpratio=log(gdp1/gdp2)
drop if logyratio==. | loggdpratio==.
quietly nbreg terrorCounts rivalry jointDem1 logcapratio contiguity, nolog cluster(dyadid) dispersion(constant) 
estat ic
quietly nbreg terrorCounts rivalry jointDem1 logyratio contiguity, nolog cluster(dyadid) dispersion(constant) 
estat ic
quietly nbreg terrorCounts rivalry jointDem1 loggdpratio contiguity, nolog cluster(dyadid) dispersion(constant) 
estat ic


* Fuhrmann and Secheser (2014), Table 2, Model 3
clear
use "Fuhrmann-Sechser.dta"
set more off
rename state_a ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode state_a
drop _merge
rename state_b ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode state_b
drop _merge
gen yfrac = y1/(y1+y2)
gen gdpfrac=gdp1/(gdp1+gdp2)
gen lnyfrac=ln(yfrac)
gen lngdpfrac=ln(gdpfrac)
quietly probit military_conflict defense_pact_nuclear_cow nuclear_deployment defense_pact_deployment defense_pact_nonnuclear_cow us_troops challenger_nuclear_weapons target_nuclear_weapons contiguity alliance_with_challenger_cow  foreign_policy_similarity power_ratio challenger_polity target_polity polity_interaction  time_conflict time_conflict2 time_conflict3 if politically_relevant==1, cluster(dyad_id)
estat ic
quietly probit military_conflict defense_pact_nuclear_cow nuclear_deployment defense_pact_deployment defense_pact_nonnuclear_cow us_troops challenger_nuclear_weapons target_nuclear_weapons contiguity alliance_with_challenger_cow  foreign_policy_similarity yfrac challenger_polity target_polity polity_interaction  time_conflict time_conflict2 time_conflict3 if politically_relevant==1, cluster(dyad_id)
estat ic
quietly probit military_conflict defense_pact_nuclear_cow nuclear_deployment defense_pact_deployment defense_pact_nonnuclear_cow us_troops challenger_nuclear_weapons target_nuclear_weapons contiguity alliance_with_challenger_cow  foreign_policy_similarity gdpfrac challenger_polity target_polity polity_interaction  time_conflict time_conflict2 time_conflict3 if politically_relevant==1, cluster(dyad_id)
estat ic


*Grauer and Horowitz 2012, Table 2, Model 4
clear
set more off
use "GH_SS_Replication.dta"
rename cinc cinc1
merge m:m ccode year using "power2.dta"
quietly probit win msadopt polity politysquared cinc1 strat terrain troopsengaged opptroopsengaged bankseducation loggdppercapita bankscoups, cluster(ccode)
estat ic
quietly probit win msadopt polity politysquared y strat terrain troopsengaged opptroopsengaged bankseducation loggdppercapita bankscoups, cluster(ccode)
estat ic
quietly probit win msadopt polity politysquared gdp strat terrain troopsengaged opptroopsengaged bankseducation loggdppercapita bankscoups, cluster(ccode)
estat ic


*Haynes (2012), Table 1, Model 1
clear
use "Haynes.dta"
set more off
rename styear year
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
drop _merge
renam ccode initccode
rename targcode ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode targcode
drop _merge
gen yfrac = y1/(y1+y2)
gen gdpfrac=gdp1/(gdp1+gdp2)
drop if yfrac==. | gdpfrac==.
gen lnyfrac=ln(yfrac)
gen lngdpfrac=ln(gdpfrac)
quietly logit recip finalterm nonconsec allydum landcontig targpolity revtpord usdum initpwrprprtn sqrtnuma, vce(robust)
estat ic
quietly logit recip finalterm nonconsec allydum landcontig targpolity revtpord usdum yfrac sqrtnuma, vce(robust)
estat ic
quietly logit recip finalterm nonconsec allydum landcontig targpolity revtpord usdum gdpfrac sqrtnuma, vce(robust)
estat ic


*Horowitz and Stam (2014), Table 1, Model 1
clear
use "HorowitzStamLeadersIOMIDReplication.dta"
set more off
merge m:m ccode year using "power2.dta"
drop if y==. | gdp==.
quietly logit cwinit milnoncombat combat rebel warwin warloss rebelwin rebelloss age aut cinc tau_lead officetenure1000 fiveyearchallengelag cwpceyrs1 cwpceyrs2 cwpceyrs3, robust cluster(leaderid)
estat ic
quietly logit cwinit milnoncombat combat rebel warwin warloss rebelwin rebelloss age aut y tau_lead officetenure1000 fiveyearchallengelag cwpceyrs1 cwpceyrs2 cwpceyrs3, robust cluster(leaderid)
estat ic
quietly logit cwinit milnoncombat combat rebel warwin warloss rebelwin rebelloss age aut gdp tau_lead officetenure1000 fiveyearchallengelag cwpceyrs1 cwpceyrs2 cwpceyrs3, robust cluster(leaderid)
estat ic


*Huth, Croco, and Appel (2013), Table 4
clear
use "Huth.dta", clear
set more off
rename cowcntry ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename opponent ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac = y1/(y1+y2)
gen gdpfrac=gdp1/(gdp1+gdp2)
quietly logit favorcb slc3b oneethval1 onebenconst1 milratio if dvset==1, robust
estat ic
quietly logit favorcb slc3b oneethval1 onebenconst1 yfrac if dvset==1, robust
estat ic
quietly logit favorcb slc3b oneethval1 onebenconst1 gdpfrac if dvset==1, robust
estat ic


*Kinne and Marinov (2012), Table 2, Model 1
clear
use "Kinne_Marinov_JCR.dta"
set more off
rename ccodecow ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename ccodecow2 ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac = y1/(y1+y2)
gen gdpfrac=gdp1/(gdp1+gdp2)
gen lnyfrac=ln(yfrac)
gen lngdpfrac=ln(gdpfrac)
gen float recip=0
replace recip=1 if cwhost2>1 
gen float capshare1=cap_1/(cap_1 + cap_2)
gen float majmaj=0
replace majmaj=1 if majpow1==1 &majpow2==1
gen float majmin=0
replace majmin=1 if majpow1==1 & majpow2==0
gen float minmaj=0
replace minmaj=1 if majpow1==0 & majpow2==1
gen float revter=0
replace revter=1 if cwrevt11==1
gen float revpol=0
replace revpol=1 if cwrevt11==2
gen float revgov=0
replace revgov=1 if cwrevt11== 3
gen float revoth=0
replace revoth=1 if cwrevt11==4
rename contig contig1
gen contig=0
replace contig=1 if contig1<6
quietly logit recip reg1 majmaj minmaj majmin capshare1 contig s_wt_glo s_ld_1 s_ld_2 revter revgov revpol revoth, robust cluster(cwkeynum) nolog
estat ic
quietly logit recip reg1 majmaj minmaj majmin yfrac contig s_wt_glo s_ld_1 s_ld_2 revter revgov revpol revoth, robust cluster(cwkeynum) nolog
estat ic
quietly logit recip reg1 majmaj minmaj majmin gdpfrac contig s_wt_glo s_ld_1 s_ld_2 revter revgov revpol revoth, robust cluster(cwkeynum) nolog
estat ic
quietly logit recip reg1 majmaj minmaj majmin lnyfrac contig s_wt_glo s_ld_1 s_ld_2 revter revgov revpol revoth, robust cluster(cwkeynum) nolog
estat ic
quietly logit recip reg1 majmaj minmaj majmin lngdpfrac contig s_wt_glo s_ld_1 s_ld_2 revter revgov revpol revoth, robust cluster(cwkeynum) nolog
estat ic


*Kroenig (2013), Table 3, Model 5
clear
use "Kroenig.dta"
set more off
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename ccode2 ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac = y1/(y1+y2)
gen gdpfrac=gdp1/(gdp1+gdp2)
gen lnyfrac=ln(yfrac)
gen lngdpfrac=ln(gdpfrac)
quietly probit victory nuc_rat proximity stakes polity21 capshare strikea tpop_1 viol cris_ave if crisis==1, cluster(crdynum)
estat ic
quietly probit victory nuc_rat proximity stakes polity21 yfrac strikea tpop_1 viol cris_ave if crisis==1, cluster(crdynum)
estat ic
quietly probit victory nuc_rat proximity stakes polity21 gdpfrac strikea tpop_1 viol cris_ave if crisis==1, cluster(crdynum)
estat ic


*Lupu (2016)
clear
use "Lupu.dta"
set more off
merge m:m ccode year using "power2.dta"
gen lny=ln(y)
gen lngdp=ln(gdp)
quietly regress  coord1d polity2 polconv cap loggdp logtrade  Asia Europe MidEast Africa  western islamic african latin orthodox sinic if year==2000
estat ic
quietly regress  coord1d polity2 polconv lny logtrade  Asia Europe MidEast Africa  western islamic african latin orthodox sinic if year==2000
estat ic
quietly regress  coord1d polity2 polconv lngdp loggdp logtrade  Asia Europe MidEast Africa  western islamic african latin orthodox sinic if year==2000
estat ic


*Moon and Souva (2016), Table 1, Model 2
clear
use "Moon and Souva JCR replication data Table 1 MCT data.dta"
set more off
rename ccode1 ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename ccode2 ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac = y1/(y1+y2)
gen gdpfrac=gdp1/(gdp1+gdp2)
quietly logit failure_100_ks mctstrataccd mctstratnoaccd nomctstrataccd  majmaj_ks majmin_ks minmaj_ks capshare_a_ks contig_ks swt_dyad tau_lead_a tau_lead_b dummy1939_ks dummy1940_ks dummy1941_ks dummy1942_ks dummy1943_ks dummy1945_ks, robust cluster(mct_code)
estat ic
quietly logit failure_100_ks mctstrataccd mctstratnoaccd nomctstrataccd  majmaj_ks majmin_ks minmaj_ks yfrac contig_ks swt_dyad tau_lead_a tau_lead_b dummy1939_ks dummy1940_ks dummy1941_ks dummy1942_ks dummy1943_ks dummy1945_ks, robust cluster(mct_code)
estat ic
quietly logit failure_100_ks mctstrataccd mctstratnoaccd nomctstrataccd  majmaj_ks majmin_ks minmaj_ks gdpfrac contig_ks swt_dyad tau_lead_a tau_lead_b dummy1939_ks dummy1940_ks dummy1941_ks dummy1942_ks dummy1943_ks dummy1945_ks, robust cluster(mct_code)
estat ic


*Narang and Talmadge (2017), Table 2, Model 5
clear
use "NarangTalmadge.dta"
set more off
drop if WarOutcome > 1 
rename StartYear1 year
merge m:m ccode year using "power2.dta"
quietly logit WarOutcome Inititator ConventionalCapability Defense PolityScore civwarCOW sumBad2, vce(cluster ccode)
estat ic
quietly logit WarOutcome Inititator y Defense PolityScore civwarCOW sumBad2, vce(cluster ccode)
estat ic
quietly logit WarOutcome Inititator gdp Defense PolityScore civwarCOW sumBad2, vce(cluster ccode)
estat ic


*Powell (2015), Table 3, Full Model
clear
use "Powell_replication_IO_2015_final.dta"
set more off
rename ccode num
rename challenger ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename target ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac = y1/(y1+y2)
gen rely = yfrac if yfrac>=.5 
replace rely =1-yfrac if yfrac<.5
gen gdpfrac=gdp1/(gdp1+gdp2)
gen relgdp= gdpfrac if gdpfrac>=.5
replace relgdp=1-gdpfrac if gdpfrac<.5
drop if rely==. | relgdp==.
rename num ccode
quietly mlogit  sett_attempt_3  edu_in_const_2 p_edu_sharia  women_either_courts   constitution_HO   constitution_SI  supreme_court   courts_secular PRD    pacsett  ethnic economic strategic power_asymmetry democ  pastfight, cluster (ccode) baseoutcome (3)
estat ic
quietly mlogit  sett_attempt_3  edu_in_const_2 p_edu_sharia  women_either_courts   constitution_HO   constitution_SI  supreme_court   courts_secular PRD    pacsett  ethnic economic strategic rely democ  pastfight, cluster (ccode) baseoutcome (3)
estat ic
quietly mlogit  sett_attempt_3  edu_in_const_2 p_edu_sharia  women_either_courts   constitution_HO   constitution_SI  supreme_court   courts_secular PRD    pacsett  ethnic economic strategic relgdp democ  pastfight, cluster (ccode) baseoutcome (3)
estat ic


*Sechser and Fuhrmann (2013), Table  1, Model 2
clear
use "Sechser-Fuhrmann-Appendix-A.dta"
set more off
rename ccode_a ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename ccode_b ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac = y1/(y1+y2)
gen gdpfrac=gdp1/(gdp1+gdp2)
quietly probit success1 nuclear_challenger nuclear_target nuclear_challenger_x_target stakes capability_ratio dispute_history resolve, cluster(dyadid)
estat ic
quietly probit success1 nuclear_challenger nuclear_target nuclear_challenger_x_target stakes yfrac dispute_history resolve, cluster(dyadid)
estat ic
quietly probit success1 nuclear_challenger nuclear_target nuclear_challenger_x_target stakes gdpfrac dispute_history resolve, cluster(dyadid)
estat ic


*Shelef (Winter), Table 3, All MIDs
clear
use "ShelefUnequal ground, IO data, final.dta"
set more off
rename loseccode ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode loseccode
drop _merge
rename gainccode ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode gainccode
drop _merge
gen yfrac = y1/(y1+y2)
gen gdpfrac=gdp1/(gdp1+gdp2)
drop if yfrac==. | gdpfrac==.
quietly reg midpost homelandclaim priormid coethnicity oil_gas capital_distance_log dyaddem allydummy cincrat i.regionfe_region_decade if colony2==0, cluster(dyadid4)
estat ic
quietly reg midpost homelandclaim priormid coethnicity oil_gas capital_distance_log dyaddem allydummy yfrac i.regionfe_region_decade if colony2==0, cluster(dyadid4)
estat ic
quietly reg midpost homelandclaim priormid coethnicity oil_gas capital_distance_log dyaddem allydummy gdpfrac i.regionfe_region_decade if colony2==0, cluster(dyadid4)
estat ic


*Way and Weeks (2013), Table 1, Plus Capabilities
clear
use "WayWeeksAJPS.dta"
set more off
rename gdp lngdppc
merge m:m ccode year using "power2.dta"
xtset ccode year
btscs pursueonly year ccode, g(time)
gen time2=time*time
gen time3=time2*time
gen lny=ln(y)
gen lngdp=ln(gdp)
drop if y==. | gdp==.
quietly xtlogit pursueonly persdumjlw_lag land cap time time2 time3, nolog
estat ic
quietly xtlogit pursueonly persdumjlw_lag land lny time time2 time3, nolog
estat ic
quietly xtlogit pursueonly persdumjlw_lag land lngdp time time2 time3, nolog
estat ic


*Weeks (2012), Table 2, Model 4
clear
use "WeeksAPSR2012.dta"
set more off
rename ccode1 ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename ccode2 ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac = y1/(y1+y2)
gen gdpfrac=gdp1/(gdp1+gdp2)
drop if dirdyadid==. | cap_1==. | cap_2==. | y1==. | y2==. | gdp1==. | gdp2==.
xtset dirdyadid year
xtlogit mzinit persrat_1a milrat_1 persXmila 										      newregime_1 cap_1 cap_2 initshare majmaj minmaj majmin pcyrsmzinit pcyrsmzinits*, fe
estat ic
xtlogit mzinit persrat_1a milrat_1 persXmila 										      newregime_1 y1 y2 yfrac majmaj minmaj majmin pcyrsmzinit pcyrsmzinits*, fe
estat ic
xtlogit mzinit persrat_1a milrat_1 persXmila 										      newregime_1 gdp1 gdp2 gdpfrac majmaj minmaj majmin pcyrsmzinit pcyrsmzinits*, fe
estat ic


*Weisiger and Yarhi-Milo (2015), Table 1, Model 6
clear
use "Weisiger.dta"
set more off
rename ccode1 ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename ccode2 ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac = y1/(y1+y2)
gen gdpfrac=gdp1/(gdp1+gdp2)
gen ysum=y1+y2
gen gdpsum=gdp1+gdp2
drop if yfrac==. | gdpfrac==.
xtset dyadid year
quietly xtlogit mzmidl backdown1 midsin10 capratio capsum demlo atopally cntgdumy bothmajr onemajor t t2 t3, fe
estat ic
quietly xtlogit mzmidl backdown1 midsin10 yfrac ysum demlo atopally cntgdumy bothmajr onemajor t t2 t3, fe
estat ic
quietly xtlogit mzmidl backdown1 midsin10 gdpfrac gdpsum demlo atopally cntgdumy bothmajr onemajor t t2 t3, fe
estat ic


*Wright and Diehl (2016), Table 2, Model 3
clear
use "Wright_Diehl_2014_JCR_1816-2001.dta"
set more off
rename ccode1 ccode
merge m:m ccode year using "power2.dta"
rename y y1
renam gdp gdp1
rename ccode ccode1
drop _merge
rename ccode2 ccode
merge m:m ccode year using "power2.dta"
rename y y2
renam gdp gdp2
rename ccode ccode2
drop _merge
gen yfrac=y1/y2
replace yfrac=y2/y1 if yfrac>1
gen gdpfrac=gdp1/gdp2
replace gdpfrac=gdp2/gdp1 if gdpfrac>1
drop if yfrac==. | gdpfrac==.
quietly logit cowwar mixed terrXmixed territor  jtdem caprat terrcount  rival
estat ic
quietly logit cowwar mixed terrXmixed territor  jtdem yfrac terrcount  rival
estat ic
quietly logit cowwar mixed terrXmixed territor  jtdem gdpfrac terrcount  rival
estat ic
