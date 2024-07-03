We have downloaded the community timeseries data  from here:

	Shyamolina Ghosh, Blake Matthews, Sarah R. Supp, Roel van Klink, Francesco Pomati, 
	James A. Rusak, Imran Khaliq, Niklaus E. Zimmermann, Ole Seehausen, Christian Rixen, 
	Martin M. Gossner, Anita Narwani, Jonathan M. Chase, & Catherine H. Graham. (2023). 
	Project BioDyn: compilation of long-term (>20yrs) community timeseries data from terrestrial and 
	freshwater realms [Data set]. Zenodo. https://doi.org/10.5281/zenodo.8233591


	The above dataset are arranged into 7 subfolders:
	for BBS, for_BioTIME, for_BioTIMEx, for_swisslake, for_RivFishTIME, for_zoop_2014, for_insectRoel2020;
	and a metadata summary for 2,668 communities included in the above dataset are given as 
	"metadata_summary_with_citation.csv". 
	
	For our analysis, we considered annual abundance timeseries (1979-2019) for 
	a total of 1,826 communities: birds (n=1,246) and fish (n=580). Data for other taxa were 
	not sufficient enough to carry out a comparative analysis. 
	A metadata summary for those 1,826 communities is provided as "metadata1826_summary.csv" file.
	
	Columns of "metadata1826_summary.csv" indicate:
		source: database/source from where data are taken
		STUDY_ID: identification of each study for the corresponding datasource
		newsite: plot id if multiple plots are nested within each STUDY_ID (e.g., plot-equivalents are 
				strata for birds, and hydrobasin for fish)
		REALM: Freshwater or Terrestrial (two categories)
		TAXA: Taxon name (two categories: birds, fish)
		ORGANISMS: organisms' name
		nyr_used: number of sampled years included in the analysis (>=20 years)
		startyr: starting year of the study
		endyr: ending year of the study
		LONGITUDE: longitude
		LATITUDE: lattitude
		citation_1 to citation_6: contains relevent citation for the datasources used in this analysis.



For climate data: see folders "CHELSA_v2", "wrangled_env_data". 

For data related to species' tolerance limits see folder "STI_related".

Though we have not used the traits data (body-measurements) for birds and fish in our analysis, 
"traitsdata" folder contains info to resolve the confusion for species names (scientific names, 
common names, names appeared in the present community data).





