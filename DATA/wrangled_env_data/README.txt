The monthlyvalues timeseries files for tas, tasmax, tasmin (1979 - 2019), pr (1979 - 2018) are extracted from 
CHELSA version 2.1: (https://chelsa-climate.org/timeseries/). Though in the analysis we only included temperature data to cover 1979-2019 data period to use maximum communities.

[NOTES: The codes to download and extract monthly data require a stable high-speed internet connection to connect to the server and a lot of storage space.
That's why we provided the data as csv files here. If interested, you can view the codes to download and extract monthly data from DATA/CHELSA_v2/ folder.]

Then we extracted the annual value (avg over 12 months) from monthly values files - 
codes are given in R/wrangling_env_data.R
