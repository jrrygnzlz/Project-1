### Project 2: Penguin Data Analysis ###
# Author: Gerardo Gonzalez
# Date: April 2025
# Purpose: Perform exploratory and statistical analysis on cleaned penguin data


## ---- Packages ----
# Load required packages
library(ggplot2)
library(dplyr)
library(skimr)
library(knitr)
library(here)

## ---- LoadData ----
# Keep this section the same as in Project 1 to avoid file path issues
data_location <- here("Data", "Processed_data", "penguins.rds")
dat <- readRDS(data_location)

## ---- SummaryTable ----
sk <- skim(dat)
sk <- as.data.frame(sk)

## ---- TotalObservations ----
# Total number of observations
nrows <- nrow(dat)

## ---- Nper ----
# Calculate N per variable - find out how many actual (non-missing) - data points there are for each variable in the data set.
sk$N <- nrows - sk$n_missing

## ---- Relcol ---
# Select and rename relevant columns
sk.table <- sk[c("skim_variable", "N", "numeric.mean", "numeric.sd", "factor.top_counts")]
names(sk.table) <- c("Variable", "N", "Mean", "SE", "Counts")

## ---- Standarderror ----
# Calculate standard error
sk.table$SE <- sk.table$SE / sqrt(sk.table$N)

## ---- Viewtable ----
# View summary table in console
options(knitr.kable.NA = "")
kable(sk.table, digits = 2)

# Save summary table
results_path <- here("Results/")
saveRDS(sk.table, file = file.path(results_path, "summary_table.rds"))


## ---- BoxplotMassBySpecies ----
# Question 1: Do Species Differ in Body Mass?
# We can use a boxplot and Anovas to test if species differ significantly in body mass.
# Compare body mass by species using a boxplot

ggplot(dat, aes(x = Species, y = `Body Mass (g)`, fill = Species)) +
  geom_boxplot() +
  labs(title = "Body Mass by Species", y = "Body Mass (g)") +
  theme_minimal()

ggsave(filename = file.path(results_path, "mass_by_species_boxplot.png"))

## ---- ANOVA1 ----
# Test if species differ in body mass using ANOVA
lm1 <- lm(`Body Mass (g)` ~ Species, data = dat)
anova_species <- anova(lm1)
print(anova_species)

# Save ANOVA result
saveRDS(anova_species, file = file.path(results_path, "anova_mass_species.rds"))

## ---- ANOVA2 ----
# Test both species and island effects on body mass
lm2 <- lm(`Body Mass (g)` ~ Species + Island, data = dat)
anova_species_island <- anova(lm2)
print(anova_species_island)

# Save ANOVA result
saveRDS(anova_species_island, file = file.path(results_path, "anova_mass_species_island.rds"))

## ---- InteractionPlot ----
# Visualize interaction of body mass by species across islands
ggplot(dat, aes(x = Species, y = `Body Mass (g)`, fill = Island)) +
  geom_boxplot(position = position_dodge()) +
  labs(title = "Body Mass by Species and Island", y = "Body Mass (g)") +
  theme_minimal()

ggsave(filename = file.path(results_path, "mass_species_island_interaction.png"))

## ---- DensityPlot ----
# Density plot of body mass by species, faceted by island
ggplot(dat, aes(x = `Body Mass (g)`, fill = Species)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Island) +
  labs(title = "Body Mass Distribution by Species across Islands") +
  theme_minimal()

ggsave(filename = file.path(results_path, "mass_density_by_island.png"))

## ---- SaveClean ----
# Save filtered dataset or summary stats for future use (optional)
write.csv(sk.table, file = file.path(results_path, "summary_table.csv"), row.names = FALSE)
