# **********************
## BINF 6210 Bioinformatics Software Tools
## ASSIGNMENT 01
## Actias species BOLD script
## Maryanne Ogakwu
## 1395098
## 2024-10-02
##
##***************************

## _ Packages used -------
library(tidyverse)
conflicted::conflict_prefer("filter", "dplyr")
library("viridis")
library("vegan")
library("ggthemes")
library("janitor")
library("ggplot2")
library("maps")
library("dplyr")

rm(list = ls())

# package installation
# install.packages("BOLDconnectR")
install.packages("ggthemes")
install.packages("janitor")
# I installed the"BOLD connect R" package from the bold system as suggested by Maddie to produce a geographical distribution map of my data set. I also installed the "ggthemes" package after reading the R for data science ebook because it contains colorblind safe color palettes, which is something I'm sure Professor Karl would appreciate.

#----- OVERVIEW----
# The Actias genus, also known as luna moth or moon moth is a nocturnal species consisting of over 45 bINs. They belong to the Order Lepidopetra, and the family Saturniidae. This genus shows remarkable morphological genetic diversity and variation within and between species. At the rate new species are being classified, we can assume that there are some species within the genus that have likely been yet to be discovered, classified and barcoded.

# Work begins here
###########################################################################################

# checking my present working directory
getwd()
# setting my working directory
setwd("C:/Users/marya/OneDrive/Dokumenty/Masters/binf_6210/Assignment_01/data")


#****ASHLEY INSERT
setwd("C:/Users/isagi/Downloads/BINF6210/BINFAssignment2")

# loading my Actias (luna moth) data file acquired from the BOLD database
dfBOLDactias <- read_tsv("C:/Users/isagi/Downloads/BINF6210/BINFAssignment2/Actias_BOLD_data.tsv")

dfBOLDactias <- dfBOLDactias %>%
  janitor::clean_names() %>%
  rename_with(~ gsub("_", ".", .x))
# What class of objects does my data set contain
class(dfBOLDactias)

# What dimensions do my data set contain
dim(dfBOLDactias)

# Which countries amongst the data set has the most barcodes of the Actias species. According the the BOLD database, based on visual analysis of the data, it can be inferred that the most barcoded specimens come from East Asian countries. But to determine this I need to carry out ***analysis

# Checking the column names present in my data set to determine what
colnames(dfBOLDactias)

#ASHLEY INSERT
dfBOLDactias %>%
 filter(!is.na('country/ocean'))
count('country/ocean', sort = TRUE)

# I want to plot a global map of the distribution of the Actias species but my data set does not contain latitude and longitude values, Maddie and Bartek told me their data sets also only contained coordinate values and that they had to split them into long and lat columns so below I am breaking my coordinate values to obtain my lat and long values to plot a global map.
dflongandlat <- dfBOLDactias %>%
  separate(coord, into = c("lat", "long"), sep = ",", remove = FALSE) %>%
  select(`country.ocean`, coord, long, lat)

# I included the select function to retain the columns "country.ocean", coordinates and add columns for longitude and latitude while keeping it separate from my main data set. I wanted to retain my original data set without any changes, in case I made any mistake or added something by mistake. I also thought adding the extra two columns to the data set also made it a bit harder to navigate through when I already have 92 columns.
# I learnt about the select function from the Rdocumentation.org website because the help function in r didn't give me much clarity on how it worked.

# To create my global map, I need to find the mean latitude and longitude of my data set

dfAvgLat <- aggregate(x = dflongandlat$lat, by = list(`country/ocean` = dflongandlat$`country/ocean`), FUN = function(x) mean(x, na.rm = TRUE))


# this code isn't working
warnings()
# There were 27 warnings produced in my console??? (˃̣̣̥ ᯅ˂̣̣̥).The code is only giving me 27 NA values for 790 objects. I can understand why there are NA's because those are in the data set, but why am I only getting 27 values in the output, because the code is supposed to group all the data by countries.So there should by 27 countries listed, not 27 NA values.
# After troubleshooting I realized the issue came from when I separated the coordinates values into longitude and latitude

dflongandlat<- dfBOLDactias %>%
separate(coord, into = c("lat", "long"), sep = ",", remove = FALSE) %>%
select(`country.ocean`, coord, long, lat)

# This code produced a data set contained special characters like "[]" because the original coordinate data was uploaded to the data base with square brackets.
rm(dflongandlat)

# Creating a new data set with corrected longitude and latitude values
dfBOLDAc <- dfBOLDactias %>%
  separate(coord, into = c("lat", "long"), sep = ",", remove = FALSE) %>%
  mutate(lat = as.numeric(str_replace_all(str_trim(lat), "\\[|\\]", "")), long = as.numeric(str_replace_all(str_trim(long), "\\[|\\]", ""))) %>%
  select(`country.ocean`, `province.state`, coord, long, lat, processid, species)

# This code should separate the longitude and latitude columns without the square brackets [] and move those columns along with the `country/ocean` column and coordinate column into a new data frame called dfBOLDAc. I included the columns 'province.state', 'processid', and 'species' so I could use this data frame for other downstream analyses that needed long and lat along with other columns, since I didn't add the  long and lat values to the original dfBOLDactias data frame.

# Calculating the average longitude for each country/ocean and sorting them by ascending order.
dfAvgLong <- aggregate(x = dfBOLDAc$long, by = list(`country/ocean` = dfBOLDAc$`country.ocean`), FUN = function(x) mean(x, na.rm = TRUE))
(dfAvgLong <- dfAvgLong[order(dfAvgLong$x), ])

# Calculating the average latitude for each country/ocean and sorting them by ascending order.
dfAvgLat <- aggregate(x = dfBOLDAc$lat, by = list(`country/ocean` = dfBOLDAc$`country.ocean`), FUN = function(x) mean(x, na.rm = TRUE))
(dfAvgLat <- dfAvgLat[order(dfAvgLat$x), ])
# Question 1:
# Determining the ecological of distribution of barcoded BINs on a global scale

#Ashley Insert
df_freq <- dfBOLDAc %>%
  count(long, lat, name = "freq")

dfBOLDAc <- dfBOLDAc %>% filter(!is.na(long) & !is.na(lat))

# Now that I have all my prileminary data, I can plot a map of the global distribution of all the barcoded Actias BINs, using the longitudinal and latitudinal values
world_map <- map_data("world")
# Plotting the Global distribution of barcoded Actias spp.
ggplot(data = dfBOLDAc) +
  geom_polygon(
    data = world_map, aes(x = long, y = lat, group = group),
    fill = "gray90", color = "gray60"
  ) +
  geom_point(
    data = dfBOLDAc,
    aes(x = long, y = lat),
    size = 2, alpha = 0.7
  ) +
  labs(
    title = "Global Distribution of Actias spp.",
    x = "Longitude", y = "Latitude"
  ) +
  theme_minimal()


# Based on the map produced,it's clear that Actias BINs have been barcoded from North America all the way to East Asia. Though it seems like there is a greater amount of BINs in East Asia, suggesting that the area is richer is ecological diversity and variation. I'm going to zoom in on these particular areas of interest to see how different the frequencies are.

# Plotting the global distribution of Actias spp. across America, Canada and Mexico
ggplot(data = dfBOLDAc) +
  geom_polygon(
    data = world_map, aes(x = long, y = lat, group = group),
    fill = "gray90", color = "gray60"
  ) +
  geom_point(
    data = dfBOLDAc,
    aes(x = long, y = lat),
    size = 2, alpha = 0.7
  ) +
  labs(
    title = "Distribution of Actias spp. in North America",
    x = "Longitude", y = "Latitude", color = "species"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(-130, -60), ylim = c(20, 60)) #

# Plotting the distribution of Actias spp. across East Asia
ggplot(data = dfBOLDAc) +
  geom_polygon(
    data = world_map, aes(x = long, y = lat, group = group),
    fill = "gray90", color = "gray60"
  ) +
  geom_point(
    data = dfBOLDAc,
    aes(x = long, y = lat),
    size = 2, alpha = 0.7
  ) +
  labs(
    title = "Distribution of Actias spp. in East Asia",
    x = "Longitude", y = "Latitude", color = "species"
  ) +
  theme_minimal() +
  coord_cartesian(xlim = c(100, 150), ylim = c(10, 55)) # East Asia
#################################################################################
# Question 2
# Determining what region has the most recorded BINs

# Based on the distribution shown on the global map, and from visually analyzing the data set,it seems like the most barcoded species come from East Asia, suggesting the region is richer in BIN diversity, compared to North America.Below I'm determining the top 10 countries with the most barcoded species.
country_summary <- dfBOLDactias %>%
  count(`country.ocean`, sort = TRUE)

#Ashley insert
country_summary <- dfBOLDactias %>%
  filter(!is.na(country.ocean)) %>%
  count(country.ocean, sort = TRUE)


top10 <- country_summary %>%
  slice_max(n, n = 10)
# Top 10 countries by record count

#Ashley Insert - Finalized graph has NA category removed and an added colour gradient + legend for easier visualization.
ggplot(top10, aes(x = reorder(`country.ocean`, n), y = n, fill = n)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "viridis", name = "Number of Records") +
  labs(
    title = "Top 10 Countries with Most Actias spp. BINs",
    x = "Country",
    y = "Number of Records"
  ) +
  theme_minimal()

#Ashley insert-  visualizing top 10 sites on world map

world_centroids <- map_data("world") %>%
  group_by(region) %>%
  summarize(
    long = mean(long),
    lat = mean(lat)
  )

top10_coords <- top10 %>%
  left_join(world_centroids, by = c("country.ocean" = "region"))

ggplot() +
  # Draw the world map
  geom_polygon(
    data = map_data("world"),
    aes(x = long, y = lat, group = group),
    fill = "gray90",
    color = "gray60"
  ) +
  geom_point(
    data = top10_coords,
    aes(x = long, y = lat, size = n, color = n),
    alpha = 0.8
  ) +
  scale_size_continuous(range = c(3, 10), name = "Number of BINs") +
  scale_color_viridis_c(option = "viridis", name = "Number of BINs") +
  labs(
    title = "Top 10 Countries with Most Actias spp. BINs",
    x = "Longitude",
    y = "Latitude"
  ) +
  coord_fixed(1.3) +
  theme_minimal() +
  theme(legend.position = "bottom")


# Based on the graphs produced, China has the most barcoded BINs in the data set, showing that the region is highly diverse. But what makes this country the most diverse in barcoded species, it could be due to multiple reasons. Firstly there could be sampling bias, meaning there are some regions that have an Actias spp. that simply hasn't been barcoded due to lack of interest from researchers.

#ASHLEY INSERT - BIN Similarities for top 10 countries


#####################################################
# Question 3
# Investigating Species completedness for barcoded individuals across the Actias ssp.

# Grouping Actias species by their various BINs to determine which species has been barcoded and analysed more
dfCount.by.BIN2 <- dfBOLDactias %>%
  group_by(bin.uri) %>%
  count(bin.uri)
# Manipulating my BIN data set by switching the columns with the rows to 'spread the values'.
dfBINs.spread2 <- pivot_wider(data = dfCount.by.BIN2, names_from = bin.uri, values_from = n)

# Creating a rarecurve using extracted data from the BINs spread to assess species richness and biodiversity
x <- rarecurve(dfBINs.spread2, xlab = "Individuals Barcoded", ylab = "BIN Richness")

# Next I'll be plotting a species accumulation graph to determine the rate at which new species of the Actias genus are likely to be barcoded and classified

# Creating a summarized data set the amount of times each BIN appears to have been barcoded in each country
dfBINs.by.country <- dfBOLDactias %>%
  group_by(`country.ocean`, bin.uri) %>%
  count(bin.uri)
# This code is to remove any NA values from the dfBINs.by.country, because they don't give information on what country the barcode originated from.
dfBINs.by.country.na.rm <- dfBINs.by.country[!is.na(dfBINs.by.country$`country.ocean`) & !is.na(dfBINs.by.country$bin.uri), ]

# This will create a pivot wider data set where the columns will be the species barcode
dfBINs.spread.by.country <- pivot_wider(data = dfBINs.by.country.na.rm, names_from = bin.uri, values_from = n)

# Converting NA values in the data set to 0 for downstream analysis in ploting a species accumulation curve. If a BIN does not occur in a country, it will be recorded as 0 not NA
dfBINs.spread.by.country[is.na(dfBINs.spread.by.country)] <- 0

# Checking the names of the columns in the data set, these should be the species barcodes
names(dfBINs.spread.by.country)

# Changing the row names as special attributes, country/ocean to allow the vegan package to work, as all data within the data frame needs to be and integer. The country/ocean names are interpreted as string characters.
dfBINs.spread.by.country <- dfBINs.spread.by.country %>%
  remove_rownames() %>%
  column_to_rownames(var = "country.ocean")

# Species accumulation curve below
AccumCurve <- specaccum(dfBINs.spread.by.country)

plot(AccumCurve, xlab = "Countries Sampled", ylab = "BIN Richness")
# The results shown in the curve suggest that as more countries are sampled, the amount of species or BINs increases.
