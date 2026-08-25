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
data$Stage<- as.factor(data$Stage)
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

ambo <- within(ambo, rm(ID, TL, Depth, "mean.Rug", "ID.chapter", Site, Species))
chrys <- within(chrys, rm(ID, TL, Depth, "mean.Rug", "ID.chapter", Site, Species))
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

a.matrix <- as.matrix(ambo[, 2:8])

model.ambo <- manova(a.matrix ~ Stage, data = ambo)

model.ambo
summary(model.ambo)

ambo.disc <- candisc(model.ambo, term = "Stage")
ambo.disc
summary(ambo.disc)

ambo.disc.2 <- candisc(model.ambo, data = ambo, ndim = 1)
plot(ambo.disc.2, var.col = "black", col = palette,
var.lwd = 1, ylim = c(-4, 5))

ambo.disc$structure
ambo.disc$coeffs.std
ambo.disc$coeffs.raw

## Plot----
palette <- c("#227685","black","#689836", "#F2501D")

heplot(ambo.disc, col = palette, var.col = "black")
plot(ambo.disc, conf = 0.95, ellipse = TRUE, ellipse.prob = 0.95, col = palette, 
     var.col = "black", var.lwd = 2, pch = c(20,20,20,20))
plot(ambo.disc, ellipse = FALSE, col = palette, var.col = "black", 
     var.lwd = 2, pch = c(20,20,20,20))
plot(ambo.disc, conf = 0.95,
     col = palette, var.col = "black",
     xlim = c(-1, 1), ylim = c(-1, 1),
     var.lwd = 2, type = 'n')

# p chrysurus----
describe(chrys[, 2:8])
describeBy(chrys[, 2:8], chrys$Stage)
pairs.panels(chrys[, 2:8])

c.matrix <- as.matrix(chrys[, 2:8])

model.chrys <- manova(c.matrix ~ Stage, data = chrys)

model.chrys
summary(model.chrys)

chrys.disc <- candisc(model.chrys, term = "Stage")
chrys.disc
summary(chrys.disc)

chrys.disc.2 <- candisc(model.chrys, data = chrys, ndim = 1)
plot(chrys.disc.2, var.col = "black", col = palette,
var.lwd = 1, ylim = c(-4, 5))

chrys.disc$structure
chrys.disc$coeffs.std
chrys.disc$coeffs.raw

## Plot----
heplot(chrys.disc, col = palette, var.col = "black")
plot(chrys.disc,
     conf = 0.95, ellipse = TRUE, ellipse.prob = 0.95,
     col = palette, var.col = "black",
     var.lwd = 2, pch = c(20,20,20,20))
plot(chrys.disc, ellipse = FALSE,
     col = palette, var.col = "black", 
     var.lwd = 2, pch = c(20,20,20,20))
plot(chrys.disc, conf = 0.95,
     col = palette, var.col = "black",
     xlim = c(-1, 1), ylim = c(-1, 1),
     var.lwd = 2, type = 'n')

# p moluccensis----
describe(mol[, 2:8])
describeBy(mol[, 2:8], mol$Stage)
pairs.panels(mol[, 2:8])

## Create a matrix and run MANOVA----
m.matrix <- as.matrix(mol[, 2:8])

model.mol <- manova(m.matrix ~ Stage, data = mol)

model.mol
summary(model.mol)

mol.disc <- candisc(model.mol, term = "Stage")
mol.disc
summary(mol.disc)

mol.disc.2 <- candisc(model.mol, data = mol, ndim = 1)
plot(mol.disc.2, var.col = "black", col = palette,
var.lwd = 1, ylim = c(-4,5))

mol.disc$structure
mol.disc$coeffs.raw

# Plot----
heplot(mol.disc, col = palette, var.col = "black")
plot(mol.disc,
     conf = 0.95, ellipse = TRUE, ellipse.prob = 0.95,
     col = palette, var.col = "black",
     var.lwd = 2, pch = c(20,20,20,20))
plot(mol.disc, ellipse = FALSE,
     col = palette, var.col = "black", 
     var.lwd = 2, pch = c(20,20,20,20))
plot(mol.disc, conf = 0.95,
     col = palette, var.col = "black",
     xlim = c(-1, 1), ylim = c(-1, 1),
     var.lwd = 2, type = 'n')

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
asp.matrix <- as.matrix(all_species[, 3:9])

model.all <- manova(asp.matrix ~ Group, data = all_species)

model.all
summary(model.all)

all.disc <- candisc(model.all, term = "Group")
all.disc
summary(all.disc)

all.disc.2 <- candisc(model.all, data = all_species, ndim = 1)
plot(all.disc.2, var.col = "black", col = palette,
var.lwd = 1, ylim = c(-4,5))

all.disc$structure
all.disc$coeffs.std
all.disc$coeffs.raw

## Plot----
n.groups <- nlevels(all_species$Group)

palette <- c("#227685","black","#689836","#F2501D",
             "#849736","#84C5E2","#7D3C98","#F1C40F",
             "#16A085","#C0392B","#2E86C1","#D35400")[1:n.groups]
pch.vals <- rep(20, n.groups)

heplot(all.disc, col = palette, var.col = "black")
plot(all.disc,
     conf = 0.95, ellipse = TRUE, ellipse.prob = 0.95,
     col = palette, var.col = "black",
     var.lwd = 2, pch = c(20,20,20,20))
plot(all.disc, ellipse = FALSE,
     col = palette, var.col = "black", 
     var.lwd = 2, pch = c(20,20,20,20))
plot(all.disc, conf = 0.95,
     col = palette, var.col = "black",
     xlim = c(-1, 1), ylim = c(-1.5, 1.5),
     var.lwd = 2, type = 'n')

# End of script----