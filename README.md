# sciamma_etal2026_habitatselection #

Repository for the research article " Area-based estimates reveal distinct habitat associations across life-stages in three coral reef damselfish. ". 

To reproduce the results, set first the computing environment using renv::restore() then run the scripts. For more info about 'renv' see Read Me of the github repository.

For questions contact gabriele.sciamma@unine.ch

# dataset #
The codes take two datasets as inputs.
"habitat_dataset" is the dataset created by recording the substratum under a series of stratified points within each quadrat.
"dataset_singlepoint" is the same dataset where only the substratum in the midpoint of the quadrat is recorded, that is the first location where the fish was observed.

Other datasets are generated throughout the script.

# scripts #
'scripts' folder contains the stepwise scripts used to produce the results. This means that scripts should generally be run in the order they are found to generate all necessary dataframes.

# renv #
'renv' folder contains all packages and their dependencies (the lockfile). 