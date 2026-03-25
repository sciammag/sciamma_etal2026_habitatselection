# Call libraries----
library(dietr)
library(dplyr)
library(tidyr)

# Import dataset----
quadrat_data <- read.csv(file.path("dataset", "quadrat_data.csv"))

# Prepare two dataframes to calculate selectivity----
## Availability dataframe----
control <- subset(quadrat_data, Species == "control")
control <- droplevels(control)
control <- control |>
  arrange(Site)

## Create a df for relative habitat availability by site
habitat_cols <- c(
  "Pavement",
  "Sand",
  "Algae",
  "Soft.cor",
  "Cbranching",
  "Fbranching",
  "Mcoral",
  "Rubble"
)

p_df <- control |>
  dplyr::group_by(Site) |>
  dplyr::summarise(
    dplyr::across(
      dplyr::all_of(habitat_cols),
      \(x) sum(x, na.rm = TRUE)
    ),
    .groups = "drop"
  )

### Express relative abundance----
total_habitat <- rowSums(p_df[, habitat_cols])

p_df <- p_df |>
  dplyr::mutate(
    dplyr::across(
      dplyr::all_of(habitat_cols),
      ~ .x / total_habitat
    )
  )

p_df$Site <- as.numeric(p_df$Site)
p_df <- as.data.frame(p_df)

## Create the 'utilized' dataframe----
samples.df <- subset(quadrat_data, Species != "control")
samples.df <- droplevels(samples.df)
r <- within(samples.df,
            rm(Species, TL, Depth, "mean.Rug", ID.chapter, life_stage))

categories1 <- c("ID", "Site", "Pavement", "Sand",
                 "Algae", "Soft.cor", "Cbranching",
                 "Fbranching", "Mcoral", "Rubble")
colnames(r) <- categories1


r <- r[, c("ID", "Site", "Pavement", "Sand",
           "Algae", "Soft.cor", "Cbranching",
           "Fbranching", "Mcoral", "Rubble")]

r[, 3:10] <- r[, 3:10] / 100 # Each row is 100% in the original data

r$Site <- as.numeric(r$Site)
r <- as.data.frame(r)

# Calculate selectivity indexes----
index_data <- list(r = r, p_df = p_df)

my.indices <- Electivity(Diet = index_data$r, Available = index_data$p_df,
                         Indices = c("ForageRatio", "Ivlev",
                                     "Strauss", "Chesson",
                                     "VanderploegScavia"),
                         LogQ = TRUE,
                         CalcAbundance = FALSE, Depleting = FALSE)

summary(my.indices$VanderploegScavia)
summary(my.indices$ForageRatio)

## Extract Forage Ratio index----
forageratio <- my.indices$ForageRatio
forageratio <- forageratio |>
  arrange(Record)
forageratio <- cbind(samples.df[, 1:8], forageratio)
forageratio <- forageratio |>
  select(-Available)

# Calculate average and Bonferroni95% CI----
## Reshape to long format (1 obs for each habitat seectivity)----
ci_long <- forageratio |>
  pivot_longer(cols = all_of(habitat_cols),
               names_to = "Habitat",
               values_to = "wi")

### Save dataset----
write.csv(ci_long,
          file.path("dataset", "ci_long.csv"),
          row.names = FALSE)

## Store number of habitat types----
I <- length(habitat_cols)

## Set Bonferroni critical z-value----
alpha <- 0.05
z_crit <- qnorm(1 - alpha / (2 * I))

## Calculate mean, SE, and CI per Species × Life stage × Habitat----
### Compute mean, SE, and count----
summary_ci <- ci_long |>
  dplyr::group_by(Species, life_stage, Habitat) |>
  dplyr::summarise(
    mean_wi = mean(wi, na.rm = TRUE),
    n = sum(!is.na(wi)),
    sd_wi = sd(wi, na.rm = TRUE),
    .groups = "drop"
  )

### Compute standard error----
summary_ci <- summary_ci |>
  dplyr::mutate(
    se_wi = sd_wi / sqrt(n)
  )

### Compute confidence intervals----
summary_ci <- summary_ci |>
  dplyr::mutate(
    ci_lower = mean_wi - z_crit * se_wi,
    ci_upper = mean_wi + z_crit * se_wi
  ) |>
  dplyr::select(-sd_wi)

## Remove Cbranching----
summary_ci <- summary_ci |>
  filter(Habitat != "Cbranching")

### Save dataset----
write.csv(summary_ci,
          file.path("dataset", "summary_ci.csv"),
          row.names = FALSE)

# End of the script----
