## Repository for analysis: 

	Temperature and biodiversity influence community stability differently in birds and fishes

## Authors: Shyamolina Ghosh(1)*, Blake Matthews(2), Owen Petchey(1)
  
 
## Affiliations:
1. Department of Evolutionary Biology and Environmental Studies, University of Zurich; Winterthurerstrasse 190, 8057 Zurich, Switzerland
2. Department of Fish Ecology and Evolution, Eawag, Swiss Federal Institute of Aquatic Science and Technology; Seestrasse 79, Kastanienbaum, 6047 Switzerland

*Corresponding author: Shyamolina Ghosh (ghoshshyamolina89@gmail.com) 


## About:
	This repository records the complete workflow that produced the analysis from the data. 
	Each folder contains a "README.txt" to guide through the repository. 
	We used the data which are already publicly available (community timeseries data: https://doi.org/10.5281/zenodo.8233591, 
	climate data: CHELSA database https://doi.org/10.1038/sdata.2017.122, global occurrence data: GBIF https://www.gbif.org/). 
	
	The core software dependency is R (R Core Team, 2022. R: A language and environment for statistical computing. 
	R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/). 
	
	Codes are written and performed by Ghosh with R version 4.2.1 on a windows 10 laptop. 
	This repository is intended to record a workflow, and is not designed or tested for distribution and wide use on multiple platforms. 
	For any reproducibility issue, you may contact S. Ghosh at ghoshshyamolina89@gmail.com 

## How to compile the code? 
  - (i) First, run R/get_stability_with_envdata.Rmd.
        It will save the following 5 CSV files in the Results folder:
          1. stability_metric_and_env_all.csv,
          2. birds_splist_consistency_table.csv,
          3. fish_splist_consistency_table.csv,
          4. birds_splist_with_temp_sensitivity.csv,
          5. fish_splist_with_temp_sensitivity.csv.
       It will also save the following 2 CSV files with tolerance limit for each fish and birds species in "DATA/STI_related/fish_gbif_data/cleaned/" 
       and "DATA/STI_related/birds_gbif_data/cleaned/" path, respectively.
            fish_occurrence_metadata_with_tolerance.csv,
            birds_occurrence_metadata_with_tolerance.csv.

	*** NOTE *** The five CSV files (numbered as 1 to 5 above) serve as the the starting point of the analysis presented in the manuscript.
	You can consider them as the "minimum dataset" that are necessary to interpret, verify and extend the research in the manuscript. 

  - (ii) We already provided those csv files in respective folders, so that you can skip step (i)
  - (iii) you can get preliminary reports with further analysis from running
      R/Preliminary_report_traditional_stability.Rmd with the "minimum dataset" as mentioned in step (i).

NOTE: In the "DATA" folder, we have provided source data files for main text figures and supplemental figures of the paper titled "Temperature and biodiversity influence community stability differently in birds and fishes" (accepted in Nature Ecology & Evolution).
      
## Funding and acknowledgment:

	SG and OP were supported by funding from the University of Zurich. 
	BM was supported by funding from Eawag, SNF (grant no. 310030-207910). 

