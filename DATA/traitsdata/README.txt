#========== traits data for fish are extracted from FishBase Database ==========
I used "rfishbase" package (https://onlinelibrary.wiley.com/doi/10.1111/j.1095-8649.2012.03464.x), 
see corresponding code in R/get_fishtraits_from_FishBase.R (https://github.com/sghosh89/WP2/blob/main/R/get_fishtraits_from_FishBase.R)
For each species, we tried to collect the TL (total length in cm), sometimes TL is not avilable, then we estimated it from
other measures, if reported (SL=standard length, FL = fork length) and replaced in the corresponding columns. 
So, in "fish_traits_from_FishBase.csv" file, the column named Length (this is the max length of male/unsexed fish) corresponds to
the length type measured in column named LTypeMaxM.

NOTE: 153 TL (male), 3 SL, 1 TL (female) (Scardinius erythrophthalmus) measurement are there. 
Actually, it is 156 unique sp as walleye and Stizostedion vitreum both are same species called: Sander vitreus in FishBase.

See https://cloud.r-project.org/web/packages/rfishbase/index.html for details

#========== traits data for birds are extracted from AVONET ===============
all data compiled for birds are in this file: bird_traits_from_AVONET.csv
see corresponding code in R/get_birdtraits_from_AVONET.R (https://github.com/sghosh89/WP2/blob/main/R/get_birdtraits_from_AVONET.R)
I have compiled all records found in AVONET for the bird species we used in the analysis, 
537 bird species name are found in our data (column "spname" are the species name from bird community data), 
we have found a total of 6428 records for 536 bird species (male/female) from AVONET
(but actually they are 515 unique species, if you see the "possible_sp" column I assigned based on the "comments" column)
No records found for this unidentified hummingbird: Trochilid sp.

Ref:
"AVONET Supplementary dataset 1.xlsx" file was downloaded on 16th Aug, 2022 from 
https://figshare.com/articles/dataset/AVONET_morphological_ecological_and_geographical_data_for_all_birds_Tobias_et_al_2021_Ecology_Letters_/16586228 (version 5)
and saved in AVONET folder.

See related original publication for details:
https://onlinelibrary.wiley.com/doi/full/10.1111/ele.13898


