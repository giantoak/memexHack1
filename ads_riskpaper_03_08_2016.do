*Prepared by Jeffrey Borrowitz, Gregory DeAngelo, and Jacob Shapiro

******************************************************************
* This .do file plays around with the ad data to produce a set of tables for the risk paper
*
* Three steps:
*	1. Risk and prices
*		a. Service venue
*		b. Partial correlations
*	2. Price discrimination regressions
******************************************************************

clear
set more off

cd "D:\Dropbox\Giant Oak\Ad Data"

*creating msa_characteristics_crosssection file
import delimited "D:\Dropbox\Giant Oak\Ad Data\msa_characteristics.csv"
save "msa_characteristics_crossssection.dta", replace

* Clean up files to be merged into ad data
import delimited "D:\Dropbox\Giant Oak\Ad Data\msa_month_characteristics.csv", clear
compress
foreach var of varlist is_massage_parlor_ad-both {
	ren `var' `var'_mean
	}
*ren number_of_indicents_prostitution number_of_incidents_prostitution
save "msa_month_vars.dta", replace

* Clean up files to be merged into ad data
import excel "D:\Dropbox\Giant Oak\Ad Data\std.xlsx", sheet("Sheet2") firstrow case(lower) clear

compress
gen chl = disease=="Chlamydia"
gen gon = disease=="Gonorrhea"
gen syp = disease=="Syphilis"

foreach var of varlist chl gon syp {
	gen `var'_cases=`var'*cases
	gen `var'_rate=`var'*rate
	}

* Collapse and clean up, more MSA fixes needed
collapse (max) *_cases *_rate, by(msa year)	
ren msa census_msa_code
replace census_msa_code = "31000US31080" if census_msa_code == "31000US31100"

/*
* Extrapolate disease trends into 2014
bysort census_msa_code (year) : gen byte last = _n == _N 
local n = _N + 1 
expand 2 if last 
replace year = year + 1 in `n'/l 

drop last 

foreach var of varlist total_chls-syp_rate {
	replace `var' = . if year==2014
	bys census_msa_code: ipolate `var' year, e gen(temp)
	replace `var' = temp if `var'==.
	drop temp
	}
	
compress

merge 1:1 census_msa_code year using le_std.dta, gen(le_std)
destring chl_rateper100k_2012 gon_rateper100k_2012, force replace
replace chl_rate = chl_rateper100k_2012 if year==2012
replace gon_rate = gon_rateper100k_2012 if year==2012
drop calc_num_gon_2012 calc_num_chl_2012 pop_report_gon_2012 pop_report_chl_2012 chl_rateper100k_2012 gon_rateper100k_2012 ftsworn_2013 ftsworn_rateper100k_2013 pop_estimate_2013 le_std
save "msa_year_disease.dta", replace
*/
use "D:\Dropbox\Giant Oak\Ad Data\ad_price_ad_level1.dta", clear

* Quick cleanup
compress
*keep if sex_ad==1
ren massage_ad massage

gen incall = incall_outcall if incall_outcall=="incall" | incall_outcall=="incall and outcall" 
gen outcall = incall_outcall if incall_outcall=="outcall" | incall_outcall=="incall and outcall"
gen inc = 0
replace inc = 1 if incall != ""
gen outc = 0
replace outc = 1 if outcall != ""
replace incall = "True" if inc==1
replace incall = "False" if inc==0
replace outcall = "True" if outc==1
replace outcall = "False" if outc==0

*mean price_per_hour, over(massage)
gen service_venue = 0
replace service_venue = 2 if massage=="not massage parlor" & incall=="True" & outcall == "False"
replace service_venue = 3 if massage=="not massage parlor" & incall=="False" & outcall == "True"
replace service_venue = 4 if massage=="not massage parlor" & incall=="True" & outcall == "True"
replace service_venue = 5 if service_venue==0
lab var service_venue "Service Venue"

gen p1 = price_per_hour
replace p1 = 1500 if p1>1500
lab var p1 "Price Per Hour"

* Generate date variables for merging
gen date = date(post_date, "YMD")
gen year = yofd(date)
gen month = month(date)
gen m1 = mofd(date)

* Provider ID
bys cluster_id: gen size=_N
bys cluster_id: gen firstobs=_n
replace msa_code = "" if msa_code=="NA"
gen census_msa_code = "31000US"+msa_code

* Do figure 2 plot of prices by venue
*mean p1, over(service_venue)
*bys service_venue: summ p1 if price<1000 & size<201, d
*tab1 service_v if price<1000 & size<201

lab def venue 1 "Massage Parlor (mean=$72, median=$54, n=8,552)" 2 "Incall (mean=$142, median=$130, n=474,344)" 3 "Outcall (mean=$170, median=$155, n=280,683)" ///
			  4 "Incall/Outcall (mean=$166, median=$150, n=287,868)" 5 "Unspecified (mean=$155, median=$140, n=899,117)", replace
lab val service_venue venue
*hist p1 if service_v<5 & price<500 & size<201, by(service_v, rows(4) compact legend(off)) scheme(s1mono) start(0) width(25) ylab(,labsize(small)) ///
*	name(price_hist, replace) xtitle(" ",size(zero)) xsize(4) ysize(6) kdensity kdenopts(bwidth(25)) 
	
* Quick plot of size over service venue
*hist size if firstobs==1, by(service_v, rows(5) compact legend(off)) scheme(s1mono) start(0) width(10) ylab(,labsize(small)) ///
*	name(size_hist, replace) xtitle(" ",size(zero)) xsize(4) ysize(6) 

* Now do individual-level regressions, merging in from MSA, you lose the match on ads from 2015 and those from small MSAs or those with dates pre-2013
merge m:1 census_msa_code year month using msa_month_vars.dta, gen(msa_month_merge)
merge m:1 census_msa_code using msa_characteristics_crosssection.dta, gen(cross_section_merge)
merge m:1 census_msa_code year using msa_year_std.dta, gen(std_merge)

*merge m:1 census_msa_code year using msa_year_disease.dta, gen(disease_merge) force
compress

* Impute crime numbers
replace rape=0 if rape==.
replace violent=0 if violent==.
replace property=0 if property==.

bys census_msa_code: egen rape2=max(rape)
bys census_msa_code: egen violent2=max(violent)
bys census_msa_code: egen property2=max(property)

* Generate dummies
gen inc1 = service_venue==2
gen out1 = service_venue==3
gen both = service_venue==4
gen unclear = service_venue==5
gen commute = avg_commute

* Generate clustering variable at the MSA/month level
gen one =1 
egen msa_month = group(census_msa_code m1)
*line below used to count 'sex_ad'
bys msa_month: egen adcount = count(one)
unique cluster_id, by(msa_month) gen(up)
bys msa_month: egen uniqueproviders = max(up)


* Violence variables 
gen rapepc = rape2/(population/100000)
gen violentpc = violent2/(population/100000)
gen proppc = property2/(population/100000)
egen msayear = group(msa year)
gen vwpc = number_of_indicents_violent_to_w/(population/1000)
gen lerisk = number_of_indicents_prostitution/uniqueproviders

* Interactions
foreach var1 of varlist inc1 out1 both unclear {
	foreach var2 of varlist lerisk commute violentpc proppc violent2 property2 vwpc {
		gen `var1'X`var2' = `var1' * `var2'
		}
	}

* Month
gen m2=m1^2 
gen m3=m1^3
gen m4=m1^4

* MSA quarter FE
gen quarter = qofd(date)
qui tab quarter, gen(q_)
qui tab month, gen(m_)
egen msa_quarter = group(census_msa_code quarter)
*qui tab msa_quarter, gen(msaq_)
*qui tab msa, gen(msa_)

* Year dummies
gen y2008 = year==2008
gen y2009 = year==2009
gen y2010 = year==2010
gen y2011 = year==2011
gen y2012 = year==2012
gen y2013 = year==2013
gen y2014 = year==2014
gen y2015 = year==2015

* Generate a variable for the website
*gen colon = strpos(ad_id,":")
*gen website = substr(ad_id,1,colon)
qui tab site, gen(web_)

compress
save adprice_working.dta, replace

* Analysis
use adprice_working.dta, clear
drop if m1<0
*dropping 2015 data
*drop if year==2015
* TABLE 2: Outcall risk is mostly about commute costs *************************************

summ avg_commute if massage!="massage parlor" & p1<1000 & size<201, d
scalar cshort = r(p1)
scalar cmed = r(p50)
scalar clong = r(p99)


*For some reason these regressions do not include a variable that breaks up commute time

qui areg p1 out1 both unclear avg_commute out1Xc bothXc unclearXc adcount uniqueproviders population unemployment frac_white web_2-web_19 y2009-y2015 i.m1 if massage!="massage parlor" & p1<1000 & size<201, a(census_msa_code) cluster(census_msa_code)
gen sample=e(sample)

areg p1 out1 both unclear web_2-web_19 y2009-y2015 if sample==1 & massage!="massage parlor" & p1<1000 & size<201, a(m1) cluster(census_msa_code)
est store reg1
areg p1 out1 both unclear adcount uniqueproviders avg_commute population unemployment frac_white web_2-web_19 y2009-y2015 if massage!="massage parlor" & p1<1000 & size<201, a(m1) cluster(census_msa_code)
est store reg2
/*
areg p1 out1 both unclear avg_commute out1Xc bothXc unclearXc adcount uniqueproviders population unemployment frac_white web_2-web_19 y2009-y2015 if massage!=1 & p1<1000 & size<201 & sample==1, a(m1) cluster(census_msa_code)
est store reg3
lincom out1+cshort*out1Xc
lincom out1+cmed*out1Xc
lincom out1+clong*out1Xc
*/
areg p1 out1 both unclear avg_commute out1Xc bothXc unclearXc adcount uniqueproviders web_2-web_19 y2009-y2015 i.m1 if massage!="massage parlor" & p1<1000 & size<201 & sample==1, a(census_msa_code) cluster(census_msa_code)
est store reg4
lincom out1+cshort*out1Xc
lincom out1+cmed*out1Xc
lincom out1+clong*out1Xc


estout reg1 reg2 reg4 , replace style(tex) eqlabels(none) drop(_cons adcount uniqueproviders population unemployment frac_white web_* *m1) nolabel cells(b(fmt(%9.3f) star) ///
se(par fmt(%9.3f))) stats(r2 N N_clust, fmt(3 0 0) label("R-Squared" "Observations" "Clusters")) ///
starlevels(* .1 ** .05 *** .01) label

* TABLE 3: Prices and violence *************************************

*Price quantile = f(UCR or NIBRS based measure of LE risk)
*	- Prostitution arrests (which is 'share_of_incidents_prostitution' and 'number_of_incidents_prostitution'), perhaps per-capitized in some way, would be the LE risk measure.
*	- Violence is a logical and important item to include. Again, I think violence should be normalized in some way. 
*		We don't want to normalize by number of ads because some women that are raped/abused are not posting ads. 


* Run simple regressions but first need demeaned prices for the estimation sample only, areg gets that wrong
reg p1 female_wage_inst_mean female_wage_inst_employment adcount uniqueproviders out1 both unclear avg_commute out1Xc bothXc unclearXc web_2-web_19 y2009-y2015 i.m1 if massage!=1 & p1<1000 & size<201
replace sample = e(sample)

bys census_msa_code: egen p1demean_msa = mean(p1) if massage!=1 & p1<1000 & size<201 & sample==1
replace p1demean_msa = p1-p1demean_msa if massage!=1 & p1<1000 & size<201 & sample==1

*bys census_msa_code quarter: egen p1demean_msaq = mean(p1) if massage!=1 & p1<1000 & size<201 & sample==1
*replace p1demean_msaq = p1-p1demean_msaq

*bys m1: egen p1demean_month = mean(p1) if massage!=1 & p1<1000 & size<201 & sample==1
*replace p1demean_month= p1-p1demean_month

* Indicator for people in the top 75% in their MSA after taking account of temporal trends
reg p1demean_msa i.m1 if sample==1
predict p1demean_msa_monthFE, r
summ p1demean_msa_monthFE, d
gen highend = p1demean_msa > r(p75)
gen lowend = p1demean_msa < r(p25)

* And calculate unique providers at lowend and highend
unique cluster_id if lowend==1, by(census_msa_code m1) gen(lp)
bys census_msa_code m1: egen lowend_providers = max(lp)
unique cluster_id if highend==1, by(census_msa_code m1) gen(hp)
bys census_msa_code m1: egen highend_providers = max(hp)

* And dummy for subsetting to first observation in each MSA month
bys census_msa_code m1: gen msamonth_counter=_n if sample==1
bys census_msa_code m1: egen sharelow = mean(lowend) if sample==1
bys census_msa_code m1: egen sharehigh = mean(highend) if sample==1
gen sharemid = 1-sharelow-sharehigh

* Generate a flag for the first time you post
sort cluster_id date
by cluster_id: gen provider_counter=_n
gen firstpost = provider_counter==1
gen lowfirst = lowend*firstpost
gen regular = provider_counter>2
gen lowregular = lowend*regular
gen midfirst = firstpost * (1-lowend) * (1-highend)
gen highregular = highend*regular
gen highfirst = highend*firstpost
**Omitted because it's included twice
*gen highregular = highend*(1-firstpost)
bys census_msa_code m1: egen sharefirst = mean(firstpost) if sample==1
bys census_msa_code m1: egen shareregular = mean(regular) if sample==1
bys census_msa_code m1: egen sharelowfirst = mean(lowfirst) if sample==1 
bys census_msa_code m1: egen sharemidfirst = mean(midfirst) if sample==1
bys census_msa_code m1: egen sharehighfirst = mean(highfirst) if sample==1

bys census_msa_code m1: egen sharelowregular = mean(lowregular) if sample==1
bys census_msa_code m1: egen sharehighregular = mean(highregular) if sample==1
gen lowvhighfirst = sharelowfirst - sharehighfirst

* Generate shares of prices for first timers
bys census_msa_code m1: egen firstsharelow = mean(lowend) if sample==1 & firstpost==1
bys census_msa_code m1: egen firstsharehigh = mean(highend) if sample==1 & firstpost==1
gen firstsharemid = 1 - firstsharelow -firstsharelow
bys census_msa_code m1: gen f_msamonth_counter=_n if sample==1 & firstpost==1
foreach var of varlist firstsharelow firstsharehigh firstsharemid {
	bys census_msa_code m1: egen `var'1 = max(`var')
	}



* Then rescale to percentages
foreach var of varlist sharelow sharemid sharehigh sharelowfirst sharemidfirst sharehighfirst firstsharelow firstsharemid firstsharehigh {
	replace `var' = `var'*100
	replace `var' = 0 if `var'<0
	}
	

* Column (1) in cross section higher wages mean higher prices
reg p1 female_wage_inst_mean out1 both unclear avg_commute out1Xc bothXc unclearXc web_2-web_19 y2009-y2015 i.m1 unemployment population frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(census_msa_code) 
est store t31
* Column (2) when take out cross-sectional differences its negative
reg p1demean_msa female_wage_inst_mean out1 both unclear out1Xc bothXc unclearXc web_2-web_19 y2009-y2015 i.m1 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(census_msa_code) 
est store t32
* Columns (3-5) what's happening is that the number of lowend ads is going up, midend is going down, and highend is unchanges 
areg sharelow female_wage_inst_mean y2009-y2015 i.m1 if msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t33
areg sharemid female_wage_inst_mean y2009-y2015 i.m1 if msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t34
areg sharehigh female_wage_inst_mean y2009-y2015 i.m1 if msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t35
* And it's about entry, the share of ads that are both first time posting
mean firstpost if massage!=1 & p1<1000 & size<201 & sample==1
areg firstsharelow female_wage_inst_mean y2009-y2015 i.m1  if f_msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t36
areg firstsharemid female_wage_inst_mean y2009-y2015 i.m1  if  f_msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t37
areg firstsharehigh female_wage_inst_mean y2009-y2015 i.m1 if f_msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t38


estout t3* , replace style(tex) eqlabels(none) keep(female_wage_inst_mean) nolabel cells(b(fmt(%9.3f) star) ///
se(par fmt(%9.3f))) stats(r2 N N_clust, fmt(3 0 0) label("R-Squared" "Observations" "Clusters")) ///
starlevels(* .1 ** .05 *** .01) label

[[STOP]]

**Utilizing the same set up as Table 3, but looking at the role of LE & STD RISK
gen syp_rate = syp1_rate+syp2_rate
gen syp_case = total_syp1+total_syp2
* Column (1) in cross section higher wages mean higher prices
reg p1 lerisk total_gon total_chl syp_case  out1 both unclear avg_commute out1Xc bothXc unclearXc web_2-web_19 y2009-y2015 i.m1 unemployment population frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(census_msa_code) 
est store t41
* Column (2) when take out cross-sectional differences its negative
reg p1demean_msa lerisk total_gon total_chl syp_case out1 both unclear out1Xc bothXc unclearXc web_2-web_19 y2009-y2015 i.m1 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(census_msa_code) 
est store t42
* Columns (3-5) what's happening is that the number of lowend ads is going up, midend is going down, and highend is unchanges 
areg sharelow lerisk total_gon total_chl syp_case y2009-y2015 i.m1 if msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t43
areg sharemid lerisk total_gon total_chl syp_case y2009-y2015 i.m1 if msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t44
areg sharehigh lerisk total_gon total_chl syp_case y2009-y2015 i.m1 if msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t45
* And it's about entry, the share of ads that are both first time posting
mean firstpost if massage!=1 & p1<1000 & size<201 & sample==1
areg firstsharelow lerisk total_gon total_chl syp_case y2009-y2015 i.m1  if f_msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t46
areg firstsharemid lerisk total_gon total_chl syp_case y2009-y2015 i.m1  if  f_msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t47
areg firstsharehigh lerisk total_gon total_chl syp_case y2009-y2015 i.m1 if f_msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
est store t48


estout t4* , replace style(tex) eqlabels(none) keep(lerisk total_*) nolabel cells(b(fmt(%9.3f) star) ///
se(par fmt(%9.3f))) stats(r2 N N_clust, fmt(3 0 0) label("R-Squared" "Observations" "Clusters")) ///
starlevels(* .1 ** .05 *** .01) label


* Columns (5-7) and the number of providers is not shifting
*areg uniqueproviders female_wage_inst_mean i.m1  if  msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
*areg highend_providers female_wage_inst_mean i.m1 if  msamonth_count==1, cluster(census_msa_code) a(census_msa_code)
*areg lowend_providers female_wage_inst_mean i.m1 if  msamonth_count==1, cluster(census_msa_code) a(census_msa_code)


* Panel A - Prices
qreg2 p1demean_month female_wage_inst_mean female_wage_inst_employment out1 both unclear adcount web_2-web_19 y2009-y2015 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.2)
est store regA1
qreg2 p1demean_month female_wage_inst_mean female_wage_inst_employment out1 both unclear adcount web_2-web_19 y2009-y2015 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.5)
est store regA2
qreg2 p1demean_month female_wage_inst_mean female_wage_inst_employment out1 both unclear adcount web_2-web_19 y2009-y2015 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.8)
est store regA3

* Panel B - Unit FE
qreg2 p1demean female_wage_inst_mean female_wage_inst_employment out1 both unclear adcount web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.2)
est store regB1
qreg2 p1demean female_wage_inst_mean female_wage_inst_employment out1 both unclear adcount web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.5)
est store regB2
qreg2 p1demean female_wage_inst_mean female_wage_inst_employment out1 both unclear adcount web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.8)
est store regB3

* Adding interactions
* Panel C - Prices
set more off
qreg2 p1 female_wage_inst_mean female_wage_inst_employment lerisk gon_rate chl_rate syp_rate out1 both unclear avg_commute out1Xc bothXc unclearXc adcount web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.1)
est store t51
qreg2 p1 female_wage_inst_mean female_wage_inst_employment lerisk gon_rate chl_rate syp_rate out1 both unclear avg_commute out1Xc bothXc unclearXc adcount web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.25)
est store t52
qreg2 p1 female_wage_inst_mean female_wage_inst_employment lerisk gon_rate chl_rate syp_rate out1 both unclear avg_commute out1Xc bothXc unclearXc adcount web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.5)
est store t53
qreg2 p1 female_wage_inst_mean female_wage_inst_employment lerisk gon_rate chl_rate syp_rate out1 both unclear avg_commute out1Xc bothXc unclearXc adcount web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.75)
est store t56
qreg2 p1 female_wage_inst_mean female_wage_inst_employment lerisk gon_rate chl_rate syp_rate out1 both unclear avg_commute out1Xc bothXc unclearXc adcount web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.9)
est store t57

estout t5* , replace style(tex) eqlabels(none) keep(female_wage_inst_mean female_wage_inst_employment lerisk gon_rate chl_rate syp_rate) nolabel cells(b(fmt(%9.3f) star) ///
se(par fmt(%9.3f))) stats(r2 N N_clust, fmt(3 0 0) label("R-Squared" "Observations" "Clusters")) ///
starlevels(* .1 ** .05 *** .01) label

* Panel D - Unit FE
qreg2 p1demean female_wage_inst_mean female_wage_inst_employment out1 both unclear avg_commute out1Xc bothXc unclearXc adcount web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.2)
est store regD1
qreg2 p1demean female_wage_inst_mean female_wage_inst_employment out1 both unclear avg_commute out1Xc bothXc unclearXc adcount web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.5)
est store regD2
qreg2 p1demean female_wage_inst_mean female_wage_inst_employment out1 both unclear avg_commute out1Xc bothXc unclearXc adcount web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.8)
est store regD3



[[STOP - OLD QUANTILE REGRESSIONS BELOW]]

* Panel A - Simple regressions
qreg2 p1 female_wage_inst_mean female_wage_inst_employment adcount uniqueproviders out1 both unclear web_2-web_19 y2009-y2015 m_2-m_12 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.1)
est store reg11
qreg2 p1 lerisk vwpc y2009-y2015 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.5)
est store reg12
qreg2 p1 lerisk vwpc y2009-y2015 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.9)
est store reg13



reg p1 lerisk vwpc y2009-y2015 out1 both unclear female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201, cluster(msayear)
replace sample = e(sample)
* Simple regression
qreg2 p1 lerisk vwpc y2009-y2015 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.1)
est store reg11
qreg2 p1 lerisk vwpc y2009-y2015 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.5)
est store reg12
qreg2 p1 lerisk vwpc y2009-y2015 if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.9)
est store reg13

* Adding in venue controls
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.1)
est store reg21
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.5)
est store reg22
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.9)
est store reg23

* Adding in venue and labor market conditions
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.1)
est store reg31
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.5)
est store reg32
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.9)
est store reg33

* Adding in venue and labor market conditions with other cutpoints
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.2)
est store reg31a
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.4)
est store reg32a
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.6)
est store reg33a
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.8)
est store reg34a

* Allowing venue to interact with commute
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear avg_commute out1Xc bothXc unclearXc female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.1)
est store reg41
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear avg_commute out1Xc bothXc unclearXc female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.5)
est store reg42
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear avg_commute out1Xc bothXc unclearXc female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.9)
est store reg43

* Adding in interactions with venue
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear out1Xlerisk bothXlerisk unclearXlerisk out1Xvwpc bothXvwpc unclearXvwpc female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.1)
est store reg51
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear out1Xlerisk bothXlerisk unclearXlerisk out1Xvwpc bothXvwpc unclearXvwpc female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.5)
est store reg52
qreg2 p1 lerisk vwpc y2009-y2015 out1 both unclear out1Xlerisk bothXlerisk unclearXlerisk out1Xvwpc bothXvwpc unclearXvwpc female_wage_inst_mean female_wage_inst_employment unemployment frac_white if massage!=1 & p1<1000 & size<201 & sample==1, cluster(msayear) quantile(.9)
est store reg53


* Check msa by quarter
reg p1 female_wage_inst_mean female_wage_inst_employment unemployment frac_white population if massage!=1 & p1<1000 & size<201, robust
areg p1 female_wage_inst_mean female_wage_inst_employment unemployment if massage!=1 & p1<1000 & size<201, a(msa_quarter) robust
areg p1 female_wage_inst_mean female_wage_inst_employment web_2-web_19 if massage!=1 & p1<1000 & size<201, a(msa_quarter) cluster(msa_quarter)
areg p1 female_wage_inst_mean female_wage_inst_employment adcount uniqueproviders web_2-web_19 if massage!=1 & p1<1000 & size<201, a(msa_quarter) cluster(msa_quarter)
areg p1 female_wage_inst_mean female_wage_inst_employment adcount uniqueproviders out1 both unclear web_2-web_19 if massage!=1 & p1<1000 & size<201, a(msa_quarter) cluster(msa_quarter)
areg p1 female_wage_inst_mean female_wage_inst_employment adcount uniqueproviders out1 both unclear out1Xc bothXc unclearXc web_2-web_19 if massage!=1 & p1<1000 & size<201, a(msa_quarter) cluster(msa_quarter)
qreg2 p1 female_wage_inst_mean female_wage_inst_employment adcount uniqueproviders out1 both unclear out1Xc bothXc unclearXc web_2-web_19 if massage!=1 & p1<1000 & size<201, cluster(msa_quarter) quantile(.2)

log using "D:\Dropbox\Giant Oak\Ad Data\crime_scratch.log"

areg p1 violentpc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 , a(m1) cluster(msayear)
xi: areg p1 i.price_quintile*rapepc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 , a(m1) cluster(msayear)
xi: areg p1 i.price_quintile*violentpc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 , a(m1) cluster(msayear)
areg p1 violentpc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white if massage!=1 & p1<1000 & size<201 & price_quintile==1, a(m1) cluster(msayear)
areg p1 violentpc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 & price_quintile==2, a(m1) cluster(msayear)
areg p1 violentpc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 & price_quintile==3, a(m1) cluster(msayear)
areg p1 violentpc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 & price_quintile==4, a(m1) cluster(msayear)
areg p1 violentpc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 & price_quintile==5, a(m1) cluster(msayear)

areg p1 rapepc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 , a(m1) cluster(msayear)
areg p1 rapepc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white if massage!=1 & p1<1000 & size<201 & price_quintile==1, a(m1) cluster(msayear)
areg p1 rapepc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 & price_quintile==2, a(m1) cluster(msayear)
areg p1 rapepc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 & price_quintile==3, a(m1) cluster(msayear)
areg p1 rapepc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 & price_quintile==4, a(m1) cluster(msayear)
areg p1 rapepc out1 both unclear avg_commute outXc bothXc unclearXc providercount population unemployment frac_white  if massage!=1 & p1<1000 & size<201 & price_quintile==5, a(m1) cluster(msayear)

log close

* TABLE 4: Prices are higher where STDs are higher *************************************

areg providercount unemployment frac_white if massage!=1 & p1<1000 & size<201, a(m1) cluster(msayear)
areg providercount unemployment frac_white p1 if massage!=1 & p1<1000 & size<201, a(m1) cluster(msayear)
areg providercount unemployment frac_white p1 chl_rate gon_rate if massage!=1 & p1<1000 & size<201, a(m1) cluster(msayear)

areg providercount chl_rate p1 unemployment frac_white if massage!=1 & p1<1000 & size<201, a(m1) cluster(msayear)
areg providercount gon_rate p1 unemployment frac_white if massage!=1 & p1<1000 & size<201, a(m1) cluster(msayear)
areg p1 chl_rate gon_rate population unemployment frac_white if massage!=1 & p1<1000 & size<201, a(m1) cluster(msayear)




areg p1 out1 both unclear  avg_commute outXc bothXc unclearXc providercount population unemployment frac_white if massage!=1 & p1<1000 & size<201 & size>4, a(m1) cluster(census_msa_code)
lincom cshort*outXc
lincom cmed*outXc
lincom clong*outXc

areg p1 out1 both unclear  avg_commute outXc bothXc unclearXc providercount population unemployment frac_white if massage!=1 & p1<1000 , a(m1) cluster(census_msa_code)
lincom cshort*outXc
lincom cmed*outXc
lincom clong*outXc
