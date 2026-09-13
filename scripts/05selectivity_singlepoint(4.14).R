# Call libraries----
library(dplyr)
library(tidyr)

# Import dataset----
point_data <- read.csv(file.path("dataset", "dataset_singlepoint.csv"))

## Change column names and types----
point_data$TL <- as.numeric(point_data$TL)
point_data$Species <- as.factor(point_data$Species)
point_data$`Life.stage` <- as.factor(point_data$`Life.stage`)
point_data$Site <- as.factor(point_data$Site)
point_data <- point_data |>
  rename(life_stage = `Life.stage`,
         Cbranching = `Coarse.Branching`,
         Fbranching = `Fine.Branching`,
         Mcoral = `Mounding.Coral`,
         Soft.cor = `Soft.Coral`)

str(point_data)
summary(point_data)

## Drop "other" and merge rubbles----
point_data <- within(point_data, rm(`Other..clam..sponges..holes.`))
point_data <- within(point_data, rm(`ID.chapter.1`))
point_data <- within(point_data, rm(`total`))

point_data <- point_data |>
  mutate(Rubble = `Coarse.rubble` + `Fine.rubble`)
point_data$`Coarse.rubble` <- NULL
point_data$`Fine.rubble` <- NULL

head(point_data)

# Prepare two dataframes to calculate selectivity----
## Availability dataframe----
control <- subset(point_data, Species == "control")
control <- droplevels(control)
control <- control |>
  arrange(Site)

p <- colSums(control[, c(9:16)])
p

p_df <- data.frame(p)
p_df <- data.frame(t(p_df))

categories <- c("Pavement", "Sand", "Algae", "Soft.cor", "Cbranching",
                "Fbranching", "Mcoral", "Rubble")
colnames(p_df) <- categories

p_df[1, ] <- p_df[1, ] / sum(p_df[1, ])
sum(p_df[1, ])

## Utilization dataframe----
samples_df <- subset(point_data, Species != "control")
samples_df <- droplevels(samples_df)

r_categories <- samples_df |>
  group_by(life_stage, Species) |>
  summarise(
    Pavement = sum(Pavement, na.rm = TRUE),
    Sand = sum(Sand, na.rm = TRUE),
    Algae = sum(Algae, na.rm = TRUE),
    `Soft.cor` = sum(`Soft.cor`, na.rm = TRUE),
    `Cbranching` = sum(`Cbranching`, na.rm = TRUE),
    `Fbranching` = sum(`Fbranching`, na.rm = TRUE),
    `Mcoral` = sum(`Mcoral`, na.rm = TRUE),
    Rubble = sum(Rubble, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(ID = row_number()) |>
  select(ID, everything())

habitat_cols <- c("Pavement", "Sand", "Algae", "Soft.cor",
                  "Cbranching", "Fbranching", "Mcoral", "Rubble")

r_prop <- r_categories |>
  rowwise() |>
  mutate(total = sum(c_across(all_of(habitat_cols)), na.rm = TRUE)) |>
  mutate(across(all_of(habitat_cols), ~ .x / total)) |>
  ungroup() |>
  select(-total)

# Calculate forage ratio----
## Availability vector----
avail <- as.numeric(p_df[1, habitat_cols])
names(avail) <- habitat_cols

## Compute forage ratio: used / available----
forageratio <- r_prop |>
  mutate(across(all_of(habitat_cols),
                ~ .x / avail[cur_column()],
                .names = "{.col}_wi")) |>
  select(life_stage, Species, ends_with("_wi"))

forageratio

# Calculate SE (Manly et al. 2002)----
avail <- as.numeric(p_df[1, habitat_cols])
names(avail) <- habitat_cols

## sqrt(ri * (1 - ri) / (u_plus * pi^2)) where:
# ri = proportion of used in that habitat (forageratio)
# pi = proportion of available in that habitat (avail)
# u_plus = total used across all habitats (sum of forageratio across habitats)

se_recruits <- r_categories |>
  filter(life_stage == "recruits") |>
  rowwise() |>
  mutate(
    u_plus = sum(c_across(all_of(habitat_cols))),  # total used in that row
    across(all_of(habitat_cols),
           ~ {
             ri <- .x / u_plus    # proportion used for this habitat
             pi <- avail[cur_column()]  # proportion available
             if (!is.na(ri) && pi > 0) {
               sqrt(ri * (1 - ri) / (u_plus * pi^2))
             } else {
               NA_real_
             }
           },
           .names = "{.col}_SE")
  ) |>
  ungroup() |>
  select(ID, life_stage, Species, ends_with("_SE"))

se_recruits

# Calculate CI----
## Number of habitat categories----
I <- length(habitat_cols)
z_crit <- qnorm(1 - 0.05 / (2 * I))

## Restructure forageratio df (only recruits)----
forageratio_long <- forageratio |>
  filter(life_stage == "recruits") |>
  pivot_longer(
    cols = ends_with("_wi"),
    names_to = "Habitat",
    values_to = "mean_wi"
  ) |>
  mutate(Habitat = gsub("_wi", "", Habitat))

## Restructure SE df (only recruits)----
se_long <- se_recruits |>
  pivot_longer(
    cols = ends_with("_SE"),
    names_to = "Habitat",
    values_to = "se_wi"
  ) |>
  mutate(Habitat = gsub("_SE", "", Habitat)) |>
  select(ID, Species, life_stage, Habitat, se_wi)

## Join wi and se----
summary_singlepoint <- forageratio_long |>
  left_join(se_long, by = c("Species", "life_stage", "Habitat")) |>
  mutate(
    ci_lower = mean_wi - z_crit * se_wi,
    ci_upper = mean_wi + z_crit * se_wi
  )

summary_singlepoint

## Save dataset----
write.csv(summary_singlepoint,
          file.path("dataset", "summary_singlepoint.csv"),
          row.names = FALSE)

# End of the script----
