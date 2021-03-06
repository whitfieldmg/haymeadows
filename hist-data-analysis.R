## Meadowlife historical survey data analysis

# This R script performs data processing and analysis on historical hay meadow survey data
# collected for Cumbria Wildlife Trust. The results can be found in the report 'Meadow Life:
# an analysis of meadow survey data 1987 - 2015'.

# Load required packages
# ipak function checks if packages are installed, installs if necessary
# ipak function by Steven Worthington on github
# https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("magrittr", "dplyr", "tidyr", "ggplot2", "lubridate", "vegan", "stringr",
              "RColorBrewer", "ggmap", "rgdal", "nlme")
ipak(packages)

# Set path to data
# Note that this path will depend on where you store data files
fpath <- "Dropbox/Cumbria Wildlife Trust/Haymeadows/"

# Historical data
# Load historical data

# Dates: Where survey dates were not available, dummy dates (first of July)
# were used so that dates can be used as Date class. This makes downstream
# analysis and plotting possible.

# Tidy data operations
# Change class of date variable to Date
# Add Year variable
# Drop surveyors' names column
# Rename some variables for ease of use
# Group by site
# Create nyear variable - number of years of data for each site

histdat <- 
  tbl_df(read.csv(paste0(fpath, "hist-data-trans-clean.csv"))) %>%
  mutate(Date = dmy(Date),
         Year = year(Date)) %>%
  select(-Surveyor.s.name.s.) %>%
  rename(Site = Meadow.Life.Site.Ref) %>%
  rename(Fieldnum = Field.number..RLR.) %>%
  rename(easting = Easting) %>%
  rename(northing = Northing) %>%
  group_by(Site) %>%
  mutate(nyear = n())

# Load 'contemporary' data - 2013-15

# Tidy data operations
# Change class of date variable to Date
# Add Year variable
# Repeat above for restoration date and year
# Group by site
# Create nyear variable - number of years of data for each site

contdat <-
  tbl_df(read.csv(paste0(fpath, "data-matrix-13-15-freq.csv"))) %>%
  mutate(Date = dmy(date),
         Year = year(Date),
         restdate = dmy(restdate),
         restyear = year(restdate)) %>%
  rename(Site = siteref) %>%
  group_by(Site) %>%
  mutate(nyear = n())

# For each dataset, gather species names into species column

histdat.stk <-
  histdat %>% gather(key = species, value = freq, Agrostis.capillaris:Viola.spp)

contdat.stk <-
  contdat %>% gather(species, freq, Agrostis.capillaris:Viola)

# Replace dots in species names with spaces
histdat.stk$species <- str_replace_all(histdat.stk$species, "[.]", " ")
contdat.stk$species <- str_replace_all(contdat.stk$species, "[.]", " ")

# Read 'contemporary' survey species lookup table with functional
# type classifications

contspp <-
  tbl_df(read.csv(paste0(fpath, "cont-spp-list.csv"))) %>%
  rename(species = contspp)

# Create variable containing positive indicator species

target_pos <-
  contspp %>%
    filter(target == "positive") %>%
    select(species) %>%
    unlist %>%
    str_replace_all(" ", ".") %>%
    str_replace_all("-", ".")

# Create variable containing negative indicator species

target_neg <- contspp %>%
  filter(target == "negative") %>%
  select(species) %>%
  unlist %>%
  str_replace_all(" ", ".") %>%
  str_replace_all("-", ".")

# Left join contemporary survey data to species lookup table
# to assign functional type classifications

contdat2 <-
  left_join(contdat.stk, contspp, by = "species")

# Read historical species lookup table
histspp <-
  tbl_df(read.csv(paste0(fpath, "hist-spp-list.csv")))

# Left join historical survey data to species lookup table
# to assign functional type classifications

histdat2 <-
  left_join(histdat.stk, histspp, by = "species")

# Bind historical and contemporary data together into one dataset
datbind <- bind_rows(histdat2, contdat2)

# Ensure that year is coded as an integer, not a character
datbind$Year <- as.integer(datbind$Year)

# Extract NGR 10km square from site name
datbind$Region <- str_sub(datbind$Site, 1, 4)

# Using sites for which there is more than one year of data,
# Group by site, year and type, calculate mean frequency
# Plot using lines to show trajectories and points to show means
# Vertical lines on plots indicate restoration dates (there aren't many of these)

datbind %>%
  filter(nyear > 1) %>%
  group_by(Site, Year, type) %>%
  summarise(site.mean = mean(freq, na.rm = TRUE),
            Date = unique(Date),
            restyear = unique(restyear),
            restdate = unique(restdate)) %>%
  mutate(Date = as.Date(Date),
         restdate = as.Date(restdate)) %>%
  filter(!is.na(type)) %>% 
  ggplot(aes(Date, site.mean, colour = type)) +
  geom_vline(aes(xintercept = as.numeric(restdate))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Site, scales = "free_x") +
  scale_x_date(date_labels = "%y") +
  labs(title = "Frequency of functional groups",
       subtitle = "Data points are means",
       x = "Year", y = "Mean frequency", colour = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0))

# Save plot at A4 size
ggsave(paste0(fpath, "functional-group-freq.png"), width = 297, height = 210, units = "mm")
  

# Bind historical and contemporary survey matrices
matbind <-
  bind_rows(histdat, contdat)

# Remove unecessary variables to enable simple row-wise calculation of species richness
allsppmat <- 
  matbind %>%
  select(-c(Year, nyear, ngr, fieldnum, date, Date, NGR, Fieldnum, Farm, Site))

# Just grasses
grassmat <-
  allsppmat %>%
  select(Agrostis.capillaris:Vulpia.bromoides)

# Just forbs
forbmat <- 
  allsppmat %>%
  select(Achillea.millefolium:Viola) %>%
  select(-contains("Carex")) %>%
  select(-contains("Juncus")) %>%
  select(-contains("Luzula"))

# Just legumes
legmat <-
  allsppmat %>%
  select(Lathyrus.pratensis, contains("Lotus"), contains("Trifolium"), contains("Vicia"))

# Positive indicators
posmat <-
  allsppmat %>%
  select_(., .dots = target_pos)

# Negative indicators
negmat <-
  allsppmat %>%
  select_(., .dots = target_neg)

# Add species richness variables to matbind

matbind$rich.all   <- rowSums(allsppmat > 0, na.rm = TRUE)
matbind$rich.pos   <- rowSums(posmat > 0, na.rm = TRUE)
matbind$rich.neg   <- rowSums(negmat > 0, na.rm = TRUE)
matbind$rich.grass <- rowSums(grassmat > 0, na.rm = TRUE)
matbind$rich.forb  <- rowSums(forbmat > 0, na.rm = TRUE)
matbind$rich.leg   <- rowSums(legmat > 0, na.rm = TRUE)

# Plot species richness (all species) by change over time by site
matbind %>%
  filter(nyear > 1) %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot(aes(Date, rich.all)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Site, scales = "free_x") +
  scale_x_date(date_labels = "%y") +
  labs(title = "Richness (all species)", x = "Year", y = "Richness") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0))

# Save plot at A4 size
ggsave(paste0(fpath, "all-species-richness.png"), height = 210, width = 297, units = "mm")

# Plot change in species richness over time, by site, with species split
# into functional groups. Each functional group is represented by a different
# colour
matbind %>%
  filter(nyear > 1) %>%
  mutate(Date = as.Date(Date)) %>%
  gather(key = type, value = rich, starts_with("rich")) %>%
  separate(type, c("measure", "type"), "\\.") %>%
  ggplot(aes(Date, rich, colour = type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Site, scales = "free") +
  scale_x_date(date_labels = "%y") +
  labs(title = "Functional group richness", x = "Year", y = "Richness", colour = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0))

# Save plot at A4 size
ggsave(paste0(fpath, "functional-group-richness.png"), height = 210, width = 297, units = "mm")

# Summarise to % change in richness by site
# This code is unwieldy - there is probably a better way to do this!
# The pdiff. variables added to matbind contain the relative percentage change in
# species richness for the specified functional groups

matbind <-
  matbind %>%
  filter(nyear > 1) %>%
  group_by(Site) %>%
  mutate(pdiff.rich.all = (last(rich.all, order_by = Year) - first(rich.all, order_by = Year)) / first(rich.all, order_by = Year) * 100,
         pdiff.rich.grass = (last(rich.grass, order_by  = Year) - first(rich.grass, order_by = Year)) / first(rich.grass, order_by = Year) * 100,
         pdiff.rich.forb = (last(rich.forb, order_by = Year) - first(rich.forb, order_by = Year)) / first(rich.forb, order_by = Year) * 100,
         pdiff.rich.leg = (last(rich.leg, order_by = Year) - first(rich.leg, order_by = Year)) / first(rich.leg, order_by = Year) * 100,
         pdiff.rich.pos = (last(rich.pos, order_by = Year) - first(rich.pos, order_by = Year)) / first(rich.pos, order_by = Year) * 100,
         pdiff.rich.neg = (last(rich.neg, order_by = Year) - first(rich.neg, order_by = Year)) / first(rich.neg, order_by = Year))

# The following operations select species richness values for the first and last
# years at all sites and tidy them into a dataframe.

rich.all.last <- 
  matbind %>%
    filter(Year == max(Year, na.rm = TRUE)) %>%
    select(rich.all)

rich.all.first <- 
  matbind %>%
    filter(Year == min(Year, na.rm = TRUE)) %>%
    select(rich.all)

rich.grass.last <-
  matbind %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  select(rich.grass)

rich.grass.first <-
  matbind %>%
  filter(Year == min(Year, na.rm = TRUE)) %>%
  select(rich.grass)

rich.forb.last <-
  matbind %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  select(rich.forb)

rich.forb.first <-
  matbind %>%
  filter(Year == min(Year, na.rm = TRUE)) %>%
  select(rich.forb)

rich.leg.last <-
  matbind %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  select(rich.leg)

rich.leg.first <-
  matbind %>%
  filter(Year == min(Year, na.rm = TRUE)) %>%
  select(rich.leg)

rich.pos.last <-
  matbind %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  select(rich.pos)

rich.pos.first <-
  matbind %>%
  filter(Year == min(Year, na.rm = TRUE)) %>%
  select(rich.pos)

rich.neg.last <-
  matbind %>%
  filter(Year == max(Year, na.rm = TRUE)) %>%
  select(rich.neg)

rich.neg.first <-
  matbind %>%
  filter(Year == min(Year, na.rm = TRUE)) %>%
  select(rich.neg)

start.end.rich <- 
bind_cols(rich.all.first,
          rich.all.last = rich.all.last[, 2],
          rich.grass.first[, 2],
          rich.grass.last[, 2],
          rich.forb.first[, 2],
          rich.forb.last[, 2],
          rich.leg.first[, 2],
          rich.leg.last[, 2],
          rich.pos.first[, 2],
          rich.pos.last[, 2],
          rich.neg.first[, 2],
          rich.neg.last[, 2])

colnames(start.end.rich) <- c("Site", "rich.all.first", "rich.all.last", "rich.grass.first",
                 "rich.grass.last", "rich.forb.first", "rich.forb.last",
                 "rich.leg.first", "rich.leg.last", "rich.pos.first", "rich.pos.last",
                 "rich.neg.first", "rich.neg.last")

start.end.rich.tidy <- 
start.end.rich %>%
  gather(key = type, value = richness, -Site) %>%
  separate(type, c("what", "type", "survey"), "[.]") %>%
  select(-what) %>%
  spread(survey, richness)

# Replace type abbreviations with more meaningful full words

start.end.rich.tidy$type[start.end.rich.tidy$type == "leg"] <- "legumes"
start.end.rich.tidy$type[start.end.rich.tidy$type == "neg"] <- "negative"
start.end.rich.tidy$type[start.end.rich.tidy$type == "pos"] <- "positive"

# Plot the first and last values for species richness at each site against
# each other, for each functional group

start.end.rich.tidy %>%
  ggplot(aes(first, last)) +
    geom_point() +
    geom_abline(lty = 2) +
    facet_wrap(~ type, scales = "free") +
    coord_fixed() +
    theme_minimal() +
    theme(panel.border = element_rect(colour = "black", fill = NA)) +
    labs(x = "Initial richness", y = "Final richness")

# Save plot at A4 size
ggsave(paste0(fpath, "first-last-richness-groups.png"), height = 210, width = 297, units = "mm")

# Parse site grid refs into x and y coordinates
matbind$coord <- str_sub(matbind$NGR, start = 3)
matbind$coord_x <- sapply(str_split(matbind$coord, " "), "[", 1)
matbind$coord_y <- sapply(str_split(matbind$coord, " "), "[", 2)

# Create dataframe containing the percentage change in species richness for each site

rich.pdiff <-
  matbind %>%
    group_by(Site) %>% 
    summarise(rich.all = unique(pdiff.rich.all),
              rich.grass = unique(pdiff.rich.grass),
              rich.forb = unique(pdiff.rich.forb),
              rich.leg = unique(pdiff.rich.leg),
              rich.pos = unique(pdiff.rich.pos),
              rich.neg = unique(pdiff.rich.neg)) %>%
  gather(type, rich.pdiff, -Site) %>%
  separate(type, c("stat", "type"), "\\.") %>%
  select(-stat)

# Extract 10km National Grid code from site name
rich.pdiff$Region <- str_sub(rich.pdiff$Site, 1, 4)

# Remove duplicated sites
rich.pdiff <- 
  matbind %>%
  group_by(Site) %>%
  filter(!duplicated(Site)) %>%
  select(Area) %>%
  right_join(., rich.pdiff, by = "Site")

# Plot the percentage change in functional group richness for each site
# as a barplot

rich.pdiff %>%
  ggplot(aes(type, rich.pdiff, fill = type)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Site) +
  labs(title = "Change in functional group richness", y = "Change (%)", x = "Group",
       fill = "Group") +
  coord_flip() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

# Save plot at A4 size
ggsave(paste0(fpath, "func-rich-change.png"), height = 210, width = 297, units = "mm")

# Plot the percentage change in overall species richness at each site,
# as a barplot

rich.pdiff %>%
  filter(type == "all") %>%
  ggplot(aes(reorder(Site, rich.pdiff), rich.pdiff, fill = rich.pdiff)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "#8e0152", mid = "white", high = "#276419") +
  labs(title = "Change in species richness",
       y = "Change (%)", x = "Site", fill = "Change (%)") +
  coord_flip() +
  # guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0),
        axis.title.y = element_blank())

# Save plot at A4 size
ggsave(paste0(fpath, "rich-change.png"), height = 297, width = 210, units = "mm")

# Plot percentage change in positive indicator species richness
# at each site, as a barplot

rich.pdiff %>%
  filter(type == "pos") %>%
  ggplot(aes(reorder(Site, rich.pdiff), rich.pdiff, fill = rich.pdiff)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "#8e0152", mid = "white", high = "#276419") +
  labs(title = "Change in positive indicator species richness", y = "Change (%)",
       x = "Site", fill = "Change (%)") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0),
        axis.title.y = element_blank())

# Save plot at A4 size
ggsave(paste0(fpath, "pos-rich-change.png"), height = 297, width = 210, units = "mm")

# Plot percentage change in negative indicator species richness
# at each site, as a barplot

rich.pdiff %>%
  filter(type == "neg") %>%
  ggplot(aes(reorder(Site, rich.pdiff), rich.pdiff, fill = rich.pdiff)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(high = "#8e0152", mid = "white", low = "#276419") +
  labs(title = "Change in negative indicator species richness", y = "Change (%)",
       x = "Site", fill = "Change (%)") +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0),
        axis.title.y = element_blank())

# Save plot at A4 size
ggsave(paste0(fpath, "neg-rich-change.png"), height = 297, width = 210, units = "mm")

# Plot trends in positive and negative indicator species richness over time

datbind %>%
  filter(nyear > 1) %>%
  group_by(Site, Year, target) %>%
  filter(target == "negative" | target == "positive") %>%
  summarise(target.mean = mean(freq, na.rm = TRUE),
            Date = as.Date(unique(Date))) %>%
  ggplot(aes(Date, target.mean, colour = target)) +
  geom_line() +
  geom_point() +
  scale_colour_manual(values = c("black", "red")) +
  scale_x_date(date_labels = "%y") +
  facet_wrap(~ Site, scales = "free") +
  labs(title = "Frequency of indicator species", x = "Year", y = "Mean frequency (%)",
       colour = "Indicator") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0))

# Save plot at A4 size
ggsave(paste0(fpath, "freq-targets.png"), width = 297, height = 210, units = "mm")

## Statistical tests

# Analysis of variance using Generalised Least Squares models
# to test for differences in the overall percentage change in
# the species richness of each functional group between areas

# GLS models are used here because they allow for the inclusion
# of a term to account for differences in the within-group variance
# within groups. Without this, we would be violating the assumptions
# of parametric ANOVA, which requires the variances within groups
# to be the same

rich.pdiff.all.gls <- rich.pdiff %>%
  filter(type == "all") %>%
    gls(rich.pdiff ~ Area,
        data = .,
        weights = varIdent(form = ~ 1 | Area))

rich.pdiff.grass.gls <- rich.pdiff %>%
  filter(type == "grass") %>%
  gls(rich.pdiff ~ Area,
      data = .,
      weights = varIdent(form = ~ 1 | Area))

rich.pdiff.leg.gls <- rich.pdiff %>%
  filter(type == "leg") %>%
  gls(rich.pdiff ~ Area,
      data = .,
      weights = varIdent(form = ~ 1 | Area))

rich.pdiff.forb.gls <- rich.pdiff %>%
  filter(type == "forb") %>%
  gls(rich.pdiff ~ Area,
      data = .,
      weights = varIdent(form = ~ 1 | Area))

# Generate ANOVA tables
anova(rich.pdiff.all.gls)
anova(rich.pdiff.grass.gls)
anova(rich.pdiff.leg.gls)
anova(rich.pdiff.forb.gls)

# Analyses:
# Paired t-tests: pre and post effects on frequency pos and neg indicators
# Correlate pos indicators with overall sp richness
# anova: effect of broad region on frequency of pos / neg indicators

# Paired t-tests
# These test for differences in species richness between the start and the end
# for the survey period for all sites. A separate test is carried out for each
# functional group

# First the following code creates a dataframe containing a new variable
# (prepost), which indicates whether the record is from the start or the 
# end of the survey period for each site

matbind.prepost <- 
  matbind %>%
  group_by(Site) %>%
  mutate(prepost = ifelse(Year == min(Year, na.rm = TRUE), "start",
                          ifelse(Year == max(Year, na.rm = TRUE), "end", NA))) %>%
  mutate(prepost = as.factor(prepost)) %>%
  filter(!is.na(prepost))

# Carry out t-tests

t.test(rich.all   ~ prepost, data = matbind.prepost, paired = TRUE)
t.test(rich.grass ~ prepost, data = matbind.prepost, paired = TRUE)
t.test(rich.forb  ~ prepost, data = matbind.prepost, paired = TRUE)
t.test(rich.leg   ~ prepost, data = matbind.prepost, paired = TRUE)


## Map sites

# This code was not used to create the map included in the final report,
# but is included here for reference. Producing maps in R removes the need
# for stand-alone GIS programs like ArcMap.

# Remove duplicated rows and rows with missing data and select coordinates

meadow_coords <- 
  datbind %>%
    filter(!duplicated(Site)) %>%
    filter(!is.na(easting)) %>%
    select(easting, northing, Area) 

meadow_coords <- cbind(Easting = meadow_coords$easting, Northing = meadow_coords$northing)

# Remove duplicated rows and rows with missing data and select site names and
# 10km NG square codes

meadow_data <-
  datbind %>%
  filter(!duplicated(Site)) %>%
  filter(!is.na(easting)) %>%
  select(Area, Site)

# Join site names with percentage change in richness data

meadow_data <-
  left_join(meadow_data,
            rich.pdiff %>% filter(type == "all"),
            by = "Site")
  
meadow_data <- as.data.frame(meadow_data)

# Convert meadow_data dataframe into a SpatialPointsDataFrame
# This allows us to specify the spatial reference system in use
# (OS National Grid) and reproject it into WGS84 for plotting
# with ggmap

meadow_sp <- SpatialPointsDataFrame(meadow_coords,
                                    data = meadow_data,
                                    proj4string = CRS("+init=epsg:27700"))

# Reproject OS NG coordinates into WGS84 (long / lat)
meadow_latlong <- spTransform(meadow_sp, CRS("+init=epsg:4326"))

# Convert SpatialPointsDataFrame back into a dataframe for plotting
# with ggmap

meadow_latlong_df <- as.data.frame(meadow_latlong)

# Create bounding boxes that describe three inset regions that
# will be drawn on separate maps, and set up maps using get_map

# get_map fetches map tiles from Google Maps for the specified region,
# so an internet connection is required for these steps.

bbox1 <- make_bbox(lon = c(-3.5, -2.9), lat = c(54.35, 54.7), f = 0.1)
meadmap1 <- get_map(bbox1, maptype = "roadmap")

bbox2 <- make_bbox(lon = c(-2.9, -2.0), lat = c(54.35, 54.7), f = 0.1)
meadmap2 <- get_map(bbox2, maptype = "roadmap")

bbox3 <- make_bbox(lon = c(-3.2, -2.3), lat = c(54.2, 54.5), f = 0.1)
meadmap3 <- get_map(bbox3, maptype = "roadmap")

# Draw each map and save as 15cm square images

ggmap(meadmap1) +
  geom_point(data = meadow_latlong_df %>% filter(!is.na(rich.pdiff)),
             aes(Easting, Northing, fill = Site),
             shape = 21) +
  # scale_fill_gradient2(low = "#8e0152", mid = "white", high = "#276419") +
  labs(x = "Longitude", y = "Latitude", fill = "Site") +
  theme(legend.position = "none")

ggsave(paste0(fpath, "meadmap1.png"), width = 15, height = 15, units = "cm")

ggmap(meadmap2) +
  geom_point(data = meadow_latlong_df %>% filter(!is.na(rich.pdiff)),
             aes(Easting, Northing, fill = Site),
             shape = 21) +
  # scale_fill_gradient2(low = "#8e0152", mid = "white", high = "#276419") +
  labs(x = "Longitude", y = "Latitude", fill = "Site") +
  theme(legend.position = "none")

ggsave(paste0(fpath, "meadmap2.png"), width = 15, height = 15, units = "cm")

ggmap(meadmap3) +
  geom_point(data = meadow_latlong_df %>% filter(!is.na(rich.pdiff)),
             aes(Easting, Northing, fill = Site),
             shape = 21) +
  # scale_fill_gradient2(low = "#8e0152", mid = "white", high = "#276419") +
  labs(x = "Longitude", y = "Latitude", fill = "Site") +
  theme(legend.position = "none")

ggsave(paste0(fpath, "meadmap3.png"), width = 15, height = 15, units = "cm")

