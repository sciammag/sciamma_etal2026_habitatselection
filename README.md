# Area-based estimates reveal distinct habitat associations across life-stages in three coral reef damselfish. #

Repository for the research article " Area-based estimates reveal distinct habitat associations across life-stages in three coral reef damselfish. ". 

**Authors**: Gabriele Sciamma, Eric Fakan, Andrew Hoey

**Short Summary**: Our study shows that broad habitat-use categories applied to damselfish can overlook critical habitat associations specific to distinct life stages. We found that habitat associations in three damselfish species are dynamic through ontogeny, following species-specific patterns. We also compared two methods for estimating habitat selection, finding that a more comprehensive, area-based approach reveals more insights into benthic composition preferences than the traditional, single-point method.

For questions contact gabriele.sciamma@unine.ch

## Computational environment ##
'renv' is a r package that captures the computational r environment. It takes a snapshot of the packages utilized throughout the scripts with all their dependencies and stores it into a lockfile saving the version of each package. This creates reproducible environment for the project over time, even after new updates of the packages are released.

Since we used `renv` to manage the computational environment, you should make sure you retrieve this environment correctly before reproducing the results. 

1. **Install `renv`**: In your R console run

```r
install.packages("renv")
```

2. **Clone the repository**: Use the web URL to clone the repository and open the project into your software. RStudio, Positron, VSCode all work equally well.

3. **Restore R environment**: The R environment is stored in our project in a lockfile containing all packages and their versions. Run the `renv::restore()` function to synchronize your local R environment with the `renv.lock` file stored in our project.

```r
renv::restore()
```

This function will compare your local R environment with the one of the `renv.lock` file and install any missing package and/or their required versions. If any package (or version) is required, R console will ask you if you want to install the packages. Make sure to type and run 'y' on your console to synchronize (follow the console instructions).

Once the R environment is restored, all scripts should produce exactly the same results found inthe publication.

## Dataset ##
The project takes two datasets as inputs.
`habitat_dataset` is the dataset created by recording the substratum under a series of stratified points within each quadrat.
`dataset_singlepoint` is the same dataset where only the substratum in the midpoint of the quadrat is recorded, that is the first location where the fish was observed.

Other datasets are generated throughout the script.

## Scripts ##
'scripts' folder contains the stepwise scripts used to produce the results. Scripts should be run in the order they are found to generate all dataframes necessary for further analyses.