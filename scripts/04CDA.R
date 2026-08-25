# SCRIPT FOR THE CDA----
# Call libraries----
library(rgl)
library(carData)
library(car)
library(broom)
library(candisc)
library(data.table)
library(MASS)
library(nnet)
library(psych)
library(rattle)
library(heplots)
library(ggplot2)
library(GGally)
library(dplyr)
library(readxl)

set.seed(123)
# Import dataset----
data <- read.csv(file.path("output", "dataset", "habitat_dataset.csv"))

## Clean dataframe----
colnames(data)[colnames(data) == "Life.stage"] <- "Stage"
data$TL <- as.numeric(data$TL)
data$Species <- as.factor(data$Species)
data$Stage <- as.factor(data$Stage)
data$Site <- as.factor(data$Site)
data <- within(data, rm(`Other..clam..sponges..holes.`, `Coarse.Branching`))
data <- data |>
  mutate(Rubble = `Coarse.rubble` + `Fine.rubble`)
data$`Coarse.rubble` <- NULL
data$`Fine.rubble` <- NULL

print(data)
head(data)
str(data)

## Normalize data and subset species----
data[, 9:15] <- scale(data[, 9:15])

ambo <- subset(data, Species == "amboinensis" | Species == "control")
ambo <- droplevels(ambo)
chrys <- subset(data, Species == "chrysurus" | Species == "control")
chrys <- droplevels(chrys)
mol <- subset(data, Species == "moluccensis" | Species == "control")
mol <- droplevels(mol)

ambo <- within(ambo,
     rm(ID, TL, Depth, "mean.Rug", "ID.chapter", Site, Species))
chrys <- within(chrys,
     rm(ID, TL, Depth, "mean.Rug", "ID.chapter", Site, Species))
mol <- within(mol, rm(ID, TL, Depth, "mean.Rug", "ID.chapter", Site, Species))

head(ambo)
ggpairs(ambo)

head(chrys)
ggpairs(chrys)

head(mol)
ggpairs(mol)

# p ambo----
describe(ambo[, 2:8])
describeBy(ambo[, 2:8], ambo$Stage)
pairs.panels(ambo[, 2:8])

a_matrix <- as.matrix(ambo[, 2:8])

model_ambo <- manova(a_matrix ~ Stage, data = ambo)

model_ambo
summary(model_ambo)

ambo_disc <- candisc(model_ambo, term = "Stage")
ambo_disc
summary(ambo_disc)

ambo_disc2 <- candisc(model_ambo, data = ambo, ndim = 1)
plot(ambo_disc2, var.col = "black", col = palette, var.lwd = 1, ylim = c(-4, 5))

ambo_disc$structure
ambo_disc$coeffs.std
ambo_disc$coeffs.raw

## Plot----
palette <- c("#227685", "black", "#689836", "#F2501D")

heplot(ambo_disc, col = palette, var.col = "black")
plot(ambo_disc, conf = 0.95, ellipse = TRUE, ellipse.prob = 0.95,
     col = palette, var.col = "black",
     var.lwd = 2, pch = c(20, 20, 20, 20))
plot(ambo_disc, ellipse = FALSE, col = palette, var.col = "black",
     var.lwd = 2, pch = c(20, 20, 20, 20))
plot(ambo_disc, conf = 0.95,
     col = palette, var.col = "black",
     xlim = c(-1, 1), ylim = c(-1, 1),
     var.lwd = 2, type = "n")

# p chrysurus----
describe(chrys[, 2:8])
describeBy(chrys[, 2:8], chrys$Stage)
pairs.panels(chrys[, 2:8])

c_matrix <- as.matrix(chrys[, 2:8])

model_chrys <- manova(c_matrix ~ Stage, data = chrys)

model_chrys
summary(model_chrys)

chrys_disc <- candisc(model_chrys, term = "Stage")
chrys_disc
summary(chrys_disc)

chrys_disc2 <- candisc(model_chrys, data = chrys, ndim = 1)
plot(chrys_disc2, var.col = "black", col = palette, var.lwd = 1, ylim = c(-4, 5))

chrys_disc$structure
chrys_disc$coeffs.std
chrys_disc$coeffs.raw

## Plot----
heplot(chrys_disc, col = palette, var.col = "black")
plot(chrys_disc,
     conf = 0.95, ellipse = TRUE, ellipse.prob = 0.95,
     col = palette, var.col = "black",
     var.lwd = 2, pch = c(20, 20, 20, 20))
plot(chrys_disc, ellipse = FALSE,
     col = palette, var.col = "black",
     var.lwd = 2, pch = c(20, 20, 20, 20))
plot(chrys_disc, conf = 0.95,
     col = palette, var.col = "black",
     xlim = c(-1, 1), ylim = c(-1, 1),
     var.lwd = 2, type = "n")

# p moluccensis----
describe(mol[, 2:8])
describeBy(mol[, 2:8], mol$Stage)
pairs.panels(mol[, 2:8])

## Create a matrix and run MANOVA----
m_matrix <- as.matrix(mol[, 2:8])

model_mol <- manova(m_matrix ~ Stage, data = mol)

model_mol
summary(model_mol)

mol_disc <- candisc(model_mol, term = "Stage")
mol_disc
summary(mol_disc)

mol_disc2 <- candisc(model_mol, data = mol, ndim = 1)
plot(mol_disc2, var.col = "black", col = palette, var.lwd = 1, ylim = c(-4, 5))

mol_disc$structure
mol_disc$coeffs.raw

# Plot----
heplot(mol_disc, col = palette, var.col = "black")
plot(mol_disc,
     conf = 0.95, ellipse = TRUE, ellipse.prob = 0.95,
     col = palette, var.col = "black",
     var.lwd = 2, pch = c(20, 20, 20, 20))
plot(mol_disc, ellipse = FALSE,
     col = palette, var.col = "black",
     var.lwd = 2, pch = c(20, 20, 20, 20))
plot(mol_disc, conf = 0.95,
     col = palette, var.col = "black",
     xlim = c(-1, 1), ylim = c(-1, 1),
     var.lwd = 2, type = "n")

# All Species CDA----
all_species <- within(data, rm(ID, TL, Depth, "mean.Rug", "ID.chapter", Site))

head(all_species)
ggpairs(all_species)

## Combine species and life stage----
all_species$Group <- interaction(
     all_species$Species, all_species$Stage,
     sep = "_", drop = TRUE
)

table(all_species$Species, all_species$Stage)
nlevels(all_species$Group)

describe(all_species[, 3:9])
describeBy(all_species[, 3:9], all_species$Group)
pairs.panels(all_species[, 3:9])

## Create matrix and run MANOVA----
asp_matrix <- as.matrix(all_species[, 3:9])

model_all <- manova(asp_matrix ~ Group, data = all_species)

model_all
summary(model_all)

all_disc <- candisc(model_all, term = "Group")
all_disc
summary(all_disc)

all_disc2 <- candisc(model_all, data = all_species, ndim = 1)
plot(all_disc2, var.col = "black", col = palette, var.lwd = 1, ylim = c(-4, 5))

all_disc$structure
all_disc$coeffs.std
all_disc$coeffs.raw

## Plot----
n_groups <- nlevels(all_species$Group)

palette <- c("#227685", "black", "#689836", "#F2501D",
             "#849736", "#84C5E2", "#7D3C98", "#F1C40F",
             "#16A085", "#C0392B", "#2E86C1", "#D35400")[1:n_groups]
pch_vals <- rep(20, n.groups)

heplot(all_disc, col = palette, var.col = "black")
plot(all_disc,
     conf = 0.95, ellipse = TRUE, ellipse.prob = 0.95,
     col = palette, var.col = "black",
     var.lwd = 2, pch = c(20, 20, 20, 20))
plot(all_disc, ellipse = FALSE,
     col = palette, var.col = "black",
     var.lwd = 2, pch = c(20, 20, 20, 20))
plot(all_disc, conf = 0.95,
     col = palette, var.col = "black",
     xlim = c(-1, 1), ylim = c(-1.5, 1.5),
     var.lwd = 2, type = "n")

# End of script----
