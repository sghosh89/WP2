Results folder to hold results generated from the Rmd files you have in R/ folder

We are also providing "stability_metric_and_env_all.csv" file (this is generated from R/suppmat.Rmd)

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
LONGITUDE: Longitude (we are providing to our best knowledge when we the exact lon-lat on plot level)
LATITUDE: Latitude
t_med: median of CHELSA-extracted annual temperature timeseries for the study years included in the analysis for each community
tmax_med: median of CHELSA-extracted annual maximum temperature timeseries for the study years included in the analysis for each community
tmin_med: median of CHELSA-extracted annual minimum temperature timeseries for the study years included in the analysis for each community
t_skw: skewness of CHELSA-extracted annual temperature timeseries for the study years included in the analysis for each community
tmax_skw: skewness of CHELSA-extracted annual maximum temperature timeseries for the study years included in the analysis for each community
tmin_skw: skewness of CHELSA-extracted annual minimum temperature timeseries for the study years included in the analysis for each community
t_var: temperature variability for the community during the study period = median(annual temperature)/IQR(annual temperature distribution for the study period)
trend_t_tau: trend of annual temperature timeseries (computed by Mann-Kendall test)
trend_t_tau_sig: 1 for significant, 0 non-significant trend, based on p-value 0.05