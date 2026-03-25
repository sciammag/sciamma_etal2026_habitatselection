# Call libraries----
library(vegan)
library(RVAideMemoire)
library(openxlsx)

set.seed(123)

# Import dataset----
quadrat_data <- read.csv(file.path("dataset", "quadrat_data.csv"))

quadrat_data$TL <- as.numeric(quadrat_data$TL)
quadrat_data$Species <- as.factor(quadrat_data$Species)
quadrat_data$life_stage <- as.factor(quadrat_data$life_stage)
quadrat_data$Site <- as.factor(quadrat_data$Site)

# Create matrices for each species----
ambo <- subset(
  quadrat_data,
  Species == "amboinensis" | Species == "control"
)
ambo <- droplevels(ambo)
chrys <- subset(
  quadrat_data,
  Species == "chrysurus" | Species == "control"
)
chrys <- droplevels(chrys)
mol <- subset(
  quadrat_data,
  Species == "moluccensis" | Species == "control"
)
mol <- droplevels(mol)

ambo <- within(
  ambo,
  rm(ID, TL, Depth, "mean.Rug", "ID.chapter", Site, Species)
)
chrys <- within(
  chrys,
  rm(ID, TL, Depth, "mean.Rug", "ID.chapter", Site, Species)
)
mol <- within(
  mol,
  rm(ID, TL, Depth, "mean.Rug", "ID.chapter", Site, Species)
)

a_matrix <- as.matrix(ambo[, 2:8])
c_matrix <- as.matrix(chrys[, 2:8])
m_matrix <- as.matrix(mol[, 2:8])

permutations <- 10000

# Run PERMANOVA----
## Amboinensis----
ambo_pa <- adonis2(
  a_matrix ~ life_stage,
  data = ambo, permutations = permutations, method = "bray"
)
ambo_pa

a_matrix <- vegdist(a_matrix, method = "bray")
pw_ambo <- pairwise.perm.manova(
  a_matrix,
  ambo$life_stage,
  nperm = permutations,
  p.method = "bonferroni"
)
pw_ambo

## Chrysurus----
chrys_pa <- adonis2(
  c_matrix ~ life_stage,
  data = chrys, permutations = permutations, method = "bray"
)
chrys_pa

c_matrix <- vegdist(c_matrix, method = "bray")
pw_chrys <- pairwise.perm.manova(
  c_matrix,
  chrys$life_stage,
  nperm = permutations,
  p.method = "bonferroni"
)
pw_chrys # Summary of differences between centroids

## Moluccensis----
mol_pa <- adonis2(
  m_matrix ~ life_stage,
  data = mol, permutations = permutations, method = "bray"
)
mol_pa

m_matrix <- vegdist(m_matrix, method = "bray")
pw_mol <- pairwise.perm.manova(
  m_matrix,
  mol$life_stage,
  nperm = permutations,
  p.method = "bonferroni"
)
pw_mol

# End of script----
