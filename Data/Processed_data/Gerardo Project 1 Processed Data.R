### Here is my Project 1 ###

### packages needed ###

require(dplyr)
require(tidyr)
require(skimr)

### Load Data ###
# Find the penguins_raw_dirty.csv file on my computer
# set my working directory to it so I can look at and work with the data.

getwd()
setwd(C/Users/jerry/Documents/Git)
getwd()
setwd(Git)
setwd("C:/Users/jerry/Documents/Git") ##this the correct one
setwd("C:/Users/jerry/Documents/Git/Project-1/Data/Raw_data/Penguins_raw_dirty.csv")

setwd("C:/Users/jerry/Documents/Git/Project-1")
setwd("C:/Users/jerry/Documents/Git/Project-1/Data/Raw_data")

## for my final code, take out the set(wd) becauseit won't work for anyone else 
## it's unique to each user

data_path <- "../../Data?Raw_data/"

data_location <- "../../Data/Raw_data/penguins_raw_dirty.csv"

rawdata <- read.csv(data_location, check.names=FALSE)

getwd()

dictionary <- read.csv(paste(data_path, "datadictionary.csv", sep=""))
print(dictionary)
-- DIDNT WORK

## explore the data

dplyr::glimpse(rawdata)

summary(rawdata)

head(rawdata)

unique(rawdata$Species)

# some of the data have typos --> save rawdata as d1, modify d1 so we can compare versions

d1 <- rawdata

# fix the errors -----------------------------

d1$Species <- sub("gTin", "guin", d1$Species)
unique(d1$Species)

d1$Species <- sub("Pe0", "Pen", d1$Species)

unique(d1$Species)

d1$Species <- sub("gufn", "guin", d1$Species)

d1$Species <- sub("Peguin", "Penguin", d1$Species)

unique(d1$Species)

d1$Species <- sub("AdeKie", "Adelie", d1$Species)
d1$Species <- sub("AdelieMPenguin", "Adelie Penguin", d1$Species)
unique(d1$Species)
d1$Species <- sub("penguin", "Penguin", d1$Species)
unique(d1$Species)

# remove Penguin part

d1$Species <- sub("Penguin, ")
d1$Species <- gsub(" Penguin \\(.*\\)", "", d1$Species)

unique(d1$Species)


## fixed the species names so that now I have 4 species w/ no typos :) ##

## time to fix the errors ------------------

head(rawdata)

# fix the mising value

## abbreviate culmen length to cl

c1 <- d1$'Culmen Length (mm)'
c1 [c1 == "missing"] <- NA  # change missing to NA
c1 <- as.numeric(c1) # coerce to numeric

d1$'Culmen Length (mm)' <- c1

## look at the data again

skimr::skim(d1)

hist(d1$'Culmen Length (mm)')

# plot bivariate plot with mass

plot(d1$'Body Mass (g)', d1$'Culmen Length (mm)')

# some penguins beaks are way too long
# fix the errors ----------------------------------

# maybe its because of a misplaced decimal?
d2 <- d1
c1[c1>300]

# exclude the NA's, we don't wan't those
c1[ !is.na(c1) & c1>300]

# got the numbers, no replace with the same divided by 10:

c1[ !is.na(c1) & c1>300 ] <- c1[ !is.na(c1) & c1>300 ]/10
d2$'Culmen Length (mm)'<- c1

### check the culmen values now
skimr::skim(d2)
hist(d2$`Culmen Length (mm)`)
plot(d2$`Body Mass (g)`, d2$`Culmen Length (mm)`)
## looks much better now

## now correct body mass ---------------------------------
# some are gigantic while others have barely any mass

d3 <- d2
mm <- d3$'Body Mass (g)'
mm[ mm < 100] <- NA  # replace tiny masses with NA
nas <- which( is.na(mm) ) # find which rows have NA fro masses
d3 <- d3[ -nas, ]  # drop the penguins (rows) with missing masses

skimr::skim(d3)
hist(d3$`Body Mass (g)`)
plot(d3$`Body Mass (g)`, d3$`Culmen Length (mm)`)
##### body masses look much better now ###

## want to have Species, Sex, Island coded as a cateogrical/factor variabl

## clean the data ----------------------------------------------------

d3$Species <- as.factor(d3$Species)
d3$Sex <- as.factor(d3$Sex)
d3$Island <- as.factor(d3$Island)  
skimr::skim(d3)

# make some plots to check to make sure the data looks good

# Histogram of Body Mass by Species
ggplot2(d3, aes(x = `Body Mass (g)`, fill = Species)) +
  geom_histogram(binwidth = 250, position = "dodge", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Body Mass by Species")

install.packages("ggplot2")
install.packages("ggplot")

library(ggplot2)
library(ggplot2)

# scatter plot

ggplot(d3, aes(x = `Body Mass (g)`, y = `Culmen Length (mm)`)) +
  geom_point(alpha = 0.7, color = "blue") +  # Scatter plot with transparency
  theme_minimal() +  # Clean theme
  labs(title = "Body Mass vs. Culmen Length",
       x = "Body Mass (g)",
       y = "Culmen Length (mm)")

# Histogram 

ggplot(d3, aes(x = `Body Mass (g)`)) +
  geom_histogram(binwidth = 250, fill = "lightblue", color = "black") + 
  theme_minimal() +
  labs(title = "Distribution of Body Mass",
       x = "Body Mass (g)",
       y = "Count")

# boxplot

ggplot(d3, aes(x = Species, y = `Body Mass (g)`, fill = Species)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Body Mass Distribution by Species",
       x = "Species",
       y = "Body Mass (g)")

# density Plot

ggplot(d3, aes(x = `Body Mass (g)`, fill = Species, color = Species)) +
  geom_density(alpha = 0.4) +
  theme_minimal() +
  labs(title = "Density Plot of Body Mass by Species",
       x = "Body Mass (g)")

# Bar plot

ggplot(d3, aes(x = Island, fill = Island)) +
  geom_bar() +
  theme_minimal() +
  labs(title = "Penguin Count per Island",
       x = "Island",
       y = "Count")

# violin plot

ggplot(d3, aes(x = Species, y = `Flipper Length (mm)`, fill = Species)) +
  geom_violin(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Flipper Length Distribution by Species",
       x = "Species",
       y = "Flipper Length (mm)")

# Faceted scatter plot

ggplot(d3, aes(x = `Body Mass (g)`, y = `Culmen Length (mm)`, color = Species)) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ Species) +
  theme_minimal() +
  labs(title = "Body Mass vs. Culmen Length for Each Species")

## Drop unneeded Columns

library(dplyr)

# Select only the columns you need
d4 <- d3 %>% select(Species, Sex, Island, `Culmen Length (mm)`, `Culmen Depth (mm)`, 
                    `Flipper Length (mm)`, `Body Mass (g)`, `Delta 15 N (o/oo)`, `Delta 13 C (o/oo)`)

# Drop empty levels in factors
d4$Species <- droplevels(d4$Species)
d4$Sex <- droplevels(d4$Sex)
d4$Island <- droplevels(d4$Island)

# relevel factors
d4$Species <- factor(d4$Species, levels = c("Adelie", "Chinstrap", "Gentoo", "Ventoo"))

# check of levels were removed
levels(d4$Species)
levels(d4$Sex)
levels(d4$Island)

# Final Check 
skimr::skim(d4)
summary(d4)

processeddata <- d4

## saving the data

save_data_location <- "../../Data/Processed_data/penguins.rds"

saveRDS(d4, file = save_data_location)

save_data_location_csv <- "../../Data/Processed_data/penguins.csv"

write.csv(d4, file = save_data_location_csv, row.names = FALSE)

list.files("../../Data/Processed_data/")
