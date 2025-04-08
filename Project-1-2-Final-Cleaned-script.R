### Project 1: Penguin Data Cleaning and Analysis ###
# Author: Gerardo Gonzalez
# Date: March 2025
# Purpose: Clean and analyze penguin data


## ---- Packages ----
# Load required packages
library(dplyr) 
library(tidyr)
library(skimr)
library(ggplot2)
library(here)

## ---- Loaddata ------
data_location <- here("Data", "Raw_data", "penguins_raw_dirty.csv")
rawdata <- read.csv(data_location, check.names = FALSE)
dictionary_path <- here("Data", "Raw_data", "datadictionary.csv")


## ---- Exploredata --------
dplyr::glimpse(rawdata)
summary(rawdata)
skimr::skim(rawdata)


## ---- Cleandata1 --------

# Check for species name inconsistencies

unique(rawdata$Species)

## ---- Createcopy --------

d1 <- rawdata

## ---- Cleandata2 --------

d1$Species <- sub("gTin", "guin", d1$Species)
d1$Species <- sub("gufn", "guin", d1$Species)
d1$Species <- sub("PeOguin", "Penguin", d1$Species) # Fix corrected here!
d1$Species <- sub("Peguin", "Penguin", d1$Species)
d1$Species <- sub("AdeKie", "Adelie", d1$Species)
d1$Species <- sub("AdelieMPenguin", "Adelie Penguin", d1$Species)
d1$Species <- sub("Ventoo", "Gentoo", d1$Species)

# We can also remove the word "penguin" from the species so we jsut get the three:
# Adelie, Gentoo, & Chinstrap

d1$Species <- gsub(" [Pp]enguin \\(.*\\)", "", d1$Species)

# Now lets if our edits were correct and that we have only 3 species ---

unique(d1$Species)


# ---- Fixmissing ---------
c1 <- d1$`Culmen Length (mm)`
c1[c1 == "missing"] <- NA  
c1 <- as.numeric(c1)  
d1$`Culmen Length (mm)` <- c1

# Check to make sure we only have numeric values using skimr command

skimr::skim(d1)


# ---- Idirregulars -------
hist(d1$`Culmen Length (mm)`)
plot(d1$`Body Mass (g)`, d1$`Culmen Length (mm)`)


# ---- Fixbeak -------
d2 <- d1
c1[!is.na(c1) & c1 > 300] <- c1[!is.na(c1) & c1 > 300] / 10
d2$`Culmen Length (mm)` <- c1

# Check again

skimr::skim(d2)
hist(d2$`Culmen Length (mm)`)
plot(d1$`Body Mass (g)`, d1$`Culmen Length (mm)`)


# ---- Bodymass -------
d3 <- d2
mm <- d3$`Body Mass (g)`
mm[mm < 100] <- NA  
nas <- which(is.na(mm))  
d3 <- d3[-nas, ]  

# Verify fix

skimr::skim(d3)
plot(d3$`Body Mass (g)`, d3$`Culmen Length (mm)`)


# ---- Missingmass -----
d3$Species <- as.factor(d3$Species)
d3$Sex <- as.factor(d3$Sex)
d3$Island <- as.factor(d3$Island)

# Final check

skimr::skim(d3)
hist(d3$`Body Mass (g)`)
plot(d3$`Body Mass (g)`, d3$`Culmen Length (mm)`)


# ---- Category ------
d3$Species <- as.factor(d3$Species)
d3$Sex <- as.factor(d3$Sex)
d3$Island <- as.factor(d3$Island)  
skimr::skim(d3)


# ---- Scatter1 -------
plot(d3$`Body Mass (g)`, d3$`Culmen Depth (mm)`,
     main="Body Mass vs. Culmen Depth",
     xlab="Body Mass (g)", ylab="Culmen Depth (mm)",
     pch=19, col="steelblue")

# ---- Scatter2 --------
plot(d3$`Culmen Length (mm)`, d3$`Culmen Depth (mm)`,
     main="Culmen Length vs. Culmen Depth",
     xlab="Culmen Length (mm)", ylab="Culmen Depth (mm)",
     pch=19, col="forestgreen")

# ---- Scatter3 ------
plot(d3$`Flipper Length (mm)`, d3$`Body Mass (g)`,
     main="Flipper Length vs. Body Mass",
     xlab="Flipper Length (mm)", ylab="Body Mass (g)",
     pch=19, col="darkorange")


# ---- Box1 -------
boxplot(d3$`Body Mass (g)` ~ d3$Species,
        main="Body Mass by Species",
        xlab="Species", ylab="Body Mass (g)",
        col=c("skyblue","lightgreen","lightpink"))


# ---- Box2 -------
boxplot(d3$`Culmen Length (mm)` ~ d3$Island,
        main="Culmen Length by Island",
        xlab="Island", ylab="Culmen Length (mm)",
        col=c("cornflowerblue","aquamarine","gold"))


# ---- Dens1 -----
library(ggplot2)
ggplot(d3, aes(x=`Body Mass (g)`, fill=Species)) +
  geom_density(alpha=0.5) +
  labs(title="Density of Body Mass by Species")


# ---- Dens2 -----
ggplot(d3, aes(x=`Flipper Length (mm)`, fill=Sex)) +
  geom_density(alpha=0.5) +
  labs(title="Density of Flipper Length by Sex")


# ---- Histogram ------
ggplot(d3, aes(x = `Body Mass (g)`, fill = Species)) +
  geom_histogram(binwidth = 250, position = "dodge", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Body Mass by Species")


# ---- Barplot ------
ggplot(d3, aes(x = Island, fill = Island)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Penguin Count per Island")


# ---- Violin ------
ggplot(d3, aes(x = Species, y = `Flipper Length (mm)`, fill = Species)) +
  geom_violin(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Flipper Length Distribution by Species")


# ---- Faceted ------
ggplot(d3, aes(x = `Body Mass (g)`, y = `Culmen Length (mm)`, color = Species)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Species) +
  theme_minimal() +
  labs(title = "Body Mass vs. Culmen Length for Each Species")



# ---- Finalclean ----

colnames(d3)

library(dplyr)

# Remove the unnecessary columns

d_final <- d3 %>% 
  dplyr::select(-c("Comments", 
                   "Delta 15 N (o/oo)", 
                   "Delta 13 C (o/oo)", 
                   "Sample Number"))

# Remove unused factor levels clearly

d_final$Species <- droplevels(d_final$Species)
d_final$Island  <- droplevels(d_final$Island)
d_final$Sex     <- droplevels(d_final$Sex)

# ---- Finalcheck ------
# Do a final check to make sure everything looks good
skimr::skim(d_final)
summary(d_final)
head(d_final)

# ---- Finalsave ------
processeddata <- d_final
dir.create("../../Data/Processed_data", recursive = TRUE)
saveRDS(processeddata, file = "../../Data/Processed_data/penguins.rds")
write.csv(processeddata, file = "../../Data/Processed_data/penguins.csv", row.names = FALSE)

#####################################################################################################################


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
