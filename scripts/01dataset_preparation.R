# Call libraries----
library(dietr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
library(ggpubr)

# Import and clean dataset----
quadrat_data <- read.csv(file.path("dataset", "habitat_dataset.csv"))

## Transform factors and rename columns----
quadrat_data$TL <- as.numeric(quadrat_data$TL)
quadrat_data$Species <- as.factor(quadrat_data$Species)
quadrat_data$`Life.stage` <- as.factor(quadrat_data$`Life.stage`)
quadrat_data$Site <- as.factor(quadrat_data$Site)
quadrat_data <- quadrat_data |>
  rename(life_stage = `Life.stage`,
         Cbranching = `Coarse.Branching`,
         Fbranching = `Fine.Branching`,
         Mcoral = `Mounding.Coral`,
         Soft.cor = `Soft.Coral`)
str(quadrat_data)
summary(quadrat_data)

## Drop "other" and merge coarse and fine rubble----
quadrat_data <- within(quadrat_data, rm(`Other..clam..sponges..holes.`))
quadrat_data <- quadrat_data |>
  mutate(Rubble = `Coarse.rubble` + `Fine.rubble`)
quadrat_data$`Coarse.rubble` <- NULL
quadrat_data$`Fine.rubble` <- NULL

head(quadrat_data)

# Save dataset----
write.csv(quadrat_data,
          file.path("dataset", "quadrat_data.csv"),
          row.names = FALSE)

# End of the script----
