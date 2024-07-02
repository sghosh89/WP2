Results folder to hold results generated from the Rmd files you have in R/ folder

5 csv files are provided here: all genarated from running "R/get_stability_with_envdata.Rmd"
1) stability_metric_and_env_all.csv
2) birds_splist_consistency_table.csv
3) birds_splist_with_temp_sensitivity.csv
4) fish_splist_consistency_table.csv
5) fish_splist_with_temp_sensitivity.csv

The above five files serve as the starting point of the analysis presented in the manuscript. 
You can consider them as the "minimum dataset" that are necessary to interpret, verify and 
extend the research in the manuscript. 
All codes and steps are documented in the R-markdown file "R/Preliminary_report_traditional_stability.Rmd" 
that uses the minimum dataset to get the final results for the manuscript.


In particular, We are also providing details for "stability_metric_and_env_all.csv" file which is the main input for SEM modeling 

Each row (observation) is considered as a community.
#----------------
Columns are:

source: database/source from where data are taken
STUDY_ID: identification of each study for the corresponding datasource
newsite: plot id if multiple plots are nested within each STUDY_ID, for single plot STUDY_ID=newsite
REALM: Freshwater or Terrestrial (two categories)
TAXA: Taxon name (4 categories - birds, terrestrial invertebrates, fish, freshwater invertebrates)
ORGANISMS: organisms' name
initR: initial species richness for each community 
nsp: species richness considered for each community (sampled 70% of the total study period)
nyr_used:number of sampled years included in the analysis (>=20 years) in the range 1979-2019 to match with environmental annual timeseries
		e.g. a STUDY_ID could have data for 1928 to 2020, but we filter the data only for this range 1979-2019
startyr: starting year of the community timeseries we considered for the study
endyr: ending year of the community timeseries we considered for the study
nint: number of possible pairwise species interactions = nsp*(nsp-1)/2
nind: number of independent pairwise species interactions among nint
npos: number of significant positive (i.e. synchronous) pairwise species interactions among nint
nL: number of lower tail dep. synchronous pairwise species interactions among npos (means both species become simultaneously rare)
nU: number of upper tail dep. synchronous pairwise species interactions among npos (means both species become simultaneously abundant)
nneg: number of significant negative (i.e. asynchronous) pairwise species interactions among nint
L: total lower tail dependence from all synchronous species interactions
U: total upper tail dependence from all synchronous species interactions (negative sign)
f_nind: nind/nint
f_nL: nL/nint
f_nU: nU/nint
f_nneg: nneg/nint
cvsq_real: square of cv for the given community timeseries
cvsq_indep: square of cv for the given community timeseries as if each species fluctuating independently
phi: variance ratio, cvsq_real/cvsq_indep
phi_LdM: variance ratio scaled between 0 to 1, Loreau's method, to be used in this analysis
skw_real: skewness for the given community timeseries
skw_indep: skewness for the given community timeseries as if each species fluctuating independently
phi_skw: skewness ratio, skw_real/skw_indep
iCV: inverse of community variability = mean(total community abundance or biomass timeseries)/std(total community abundance or biomass timeseries)
iCValt:inverse of community variability, non-biased = median(total community abundance or biomass timeseries)/IQR(total community abundance or biomass timeseries)
LONGITUDE: Longitude (we are providing to our best knowledge when we have the exact lon-lat on plot level)
LATITUDE: Latitude
t_med: median of CHELSA-extracted annual temperature timeseries for the study years included in the analysis for each community
t_skw: skewness of CHELSA-extracted annual temperature timeseries for the study years included in the analysis for each community
t_var: temperature variability for the community during the study period = IQR(annual temperature distribution for the study period)/abs(t_med)
t_kurt: kurtosis of CHELSA-extracted annual temperature timeseries for the study years included in the analysis for each community
t_varIQR: temperature variability for the community during the study period = IQR(annual temperature distribution for the study period)
t.lm.slope: trend of CHELSA extracted annual temperature timeseries (computed by linear regression)
t.lm.slope.sig: significance for trend of CHELSA extracted annual temperature timeseries (computed by linear regression): significant(=1) if p value <0.05
t.sens.slope: trend of CHELSA extracted annual temperature timeseries (computed by non-parametric Sen's method)
t.sens.slope.sig: significance for trend of CHELSA extracted annual temperature timeseries (computed by non-parametric Sen's method): significant(=1) if p value <0.05
t_med_celcius: median of CHELSA-extracted annual temperature timeseries in celcius scale
t_skw_celcius: skewness of CHELSA-extracted annual temperature timeseries in celcius scale; t_skw and t_skw_celsius should be same
is.sig_t_skw_celsius: checking if skewness is significant (=1) or not (=0)
t_var_celcius: temperature variability for the community during the study period = IQR(annual temperature distribution for the study period)/abs(t_med_celcius)
t_kurt_celsius: kurtosis of CHELSA-extracted annual temperature timeseries in celcius scale
is.sig_t_kurt_celsius: checking if kurtosis is significant (=1) or not (=0)
t_varIQR_celsius: temperature variability for the community during the study period in celsius scale = IQR(annual temperature distribution in celsius for the study period)
t.lm.slope.celcius: trend of CHELSA extracted annual temperature timeseries in celcius scale (computed by linear regression)
t.lm.slope.sig.celcius: significance for trend of CHELSA extracted annual temperature timeseries in celcius scale (computed by linear regression): significant(=1) if p value <0.05
t.sens.slope.celcius: trend of CHELSA extracted annual temperature timeseries in celcius scale (computed by non-parametric Sen's method)
t.sens.slope.sig.celcius: significance for trend of CHELSA extracted annual temperature timeseries in celcius scale (computed by non-parametric Sen's method): significant(=1) if p value <0.05
is.stationary.adf: checking if temperature time series appears stationary (=1) or not (=0) in Augmented Dickey-Fuller test 
is.trend.stationary.kpss: checking if temperature time series appears trend-stationary (=1) or not (=0) in KPSS test 
GiniSimpson: Diversity index (evenness), Gini 1912 & Simpson 1949 
Simpson: Diversity index (evenness), Simpson 1949 & Magurran 2004
Shannon: Diversity index (evenness), Shannon 1948
Heip: Diversity index (evenness), Heip 1974 & Magurran 2004
McIntosh: Diversity index (evenness), McIntosh 1967 & Pielou 1975
SmithWilson: Diversity index (evenness), Smith & Wilson 1996 & Magurran 2004
Pielou: Diversity index (evenness), Pielou 1975

#=============================
We used only a few set of variables (or columns) from the above for our further analysis to genarate figures for the maintext and for the supplemental information.
Those considered columns are:
TAXA,
source,
STUDY_ID,
newsite,
LONGITUDE,
LATITUDE,
iCV as "stability",
nsp as "richness",
SmithWilson as "evenness",
phi_LdM as "overall_synchrony",
sum of two columns: (L and absolute value of U) = A as "taildep_synchrony"    
t_med_celcius as "MedianT",
t.sens.slope.celsius as "TrendT",
t_skw_celsius as "SkewT",
t_varIQR_celsius as "VarT",
"is.significant.TrendT",
"is.stationary.adf" 

#======================
For source data of main text figures and supplemental figures of the paper titled
"Temperature and biodiversity influence community stability differently in birds and fishes" (accepted in Nature Ecology & Evolution), 
please see the "DATA" folder.















