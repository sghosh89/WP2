Data folder for storing birds gbif occurence data downloaded from gbif site (https://www.gbif.org/)
Manually downloaded data for each species are saved as "x.csv", 
and data downloaded using code (rgbif R-package) are saved as "birdrecords_from_GBIF_for_x.csv";
where, x= species name.
These csv data files are then cleaned and all cleaned data are saved in the "cleaned" folder.
The data files are really big and need a lot of space, that's why we are not providing these open source data. 
A metadata table is provided though in cleaned folder as "birds_occurrence_metadata_filledin.csv"
and for each species max temperature, min temperature tolerance limit is given in cleaned folder as 
"birds_occurrence_metadata_with_tolerance.csv".


