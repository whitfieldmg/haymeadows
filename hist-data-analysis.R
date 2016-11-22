## Meadowlife historical survey data analysis

# This R script performs data processing and analysis on historical hay meadow survey data
# collected for Cumbria Wildlife Trust. The results can be found in the report 'Meadow Life:
# an analysis of meadow survey data 1987 - 2015'.

# Check if required packages are installed and install if necessary
# Uses function by Steven Worthington on github
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

# Required packages
library(magrittr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(vegan)
library(stringr)
library(RColorBrewer)
library(ggmap)
library(rgdal)
library(nlme)

# Set path to data
fpath <- "Dropbox/Cumbria Wildlife Trust/Haymeadows/"

# Historical data
# Load historical data

# Dates: Some are dummy dates (generally first of July) so that dates can be used as Date class
#        There is only one survey per field per year, so this doesn't matter.

# Tidy data
# Change class of date column to Date
# Add species richness
# Drop surveyors' names column

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

glimpse(histdat)

contdat <-
  tbl_df(read.csv(paste0(fpath, "data-matrix-13-15-freq.csv"))) %>%
  mutate(Date = dmy(date),
         Year = year(Date),
         restdate = dmy(restdate),
         restyear = year(restdate)) %>%
  rename(Site = siteref) %>%
  group_by(Site) %>%
  mutate(nyear = n())

glimpse(contdat)

# For each dataset, gather species names into species column

histdat.stk <-
  histdat %>% gather(key = species, value = freq, Agrostis.capillaris:Viola.spp)

contdat.stk <-
  contdat %>% gather(species, freq, Agrostis.capillaris:Viola)

# Replace dots in species names with spaces
histdat.stk$species <- str_replace_all(histdat.stk$species, "[.]", " ")
contdat.stk$species <- str_replace_all(contdat.stk$species, "[.]", " ")

# Read contemporary survey lookup table
contspp <-
  tbl_df(read.csv(paste0(fpath, "cont-spp-list.csv"))) %>%
  rename(species = contspp)

target_pos <- contspp %>%
  filter(target == "positive") %>%
  select(species) %>%
  unlist %>%
  str_replace_all(" ", ".") %>%
  str_replace_all("-", ".")


target_neg <- contspp %>%
  filter(target == "negative") %>%
  select(species) %>%
  unlist %>%
  str_replace_all(" ", ".") %>%
  str_replace_all("-", ".")


contdat2 <-
  left_join(contdat.stk, contspp, by = "species")

# Group by year and calculate mean freq by species type
contdat2 %>%
  group_by(Year, target) %>%
  summarise(target.mean = mean(freq, na.rm = TRUE))

contdat2 %>%
  group_by(Year, type) %>%
  summarise(type.mean = mean(freq, na.rm = TRUE))

# Group by site, year and type, calculate mean freq
contdat2 %>%
  filter(nyear > 1) %>%
  group_by(Site, Year, type) %>%
  summarise(site.mean = mean(freq, na.rm = TRUE),
            restyear = unique(restyear)) %>%
  ggplot(aes(Year, site.mean, colour = type)) +
  geom_vline(aes(xintercept = restyear)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Site)

# Read historical species table
histspp <-
  tbl_df(read.csv(paste0(fpath, "hist-spp-list.csv")))

histdat2 <-
  left_join(histdat.stk, histspp, by = "species")

# Bind historical and contemporary data together
datbind <- bind_rows(histdat2, contdat2)

datbind$Year <- as.integer(datbind$Year)

datbind$Region <- str_sub(datbind$Site, 1, 4)

# datbind$coord <- str_sub(datbind$NGR, start = 3)
# datbind$coord_x <- sapply(str_split(datbind$coord, " "), "[", 1)
# datbind$coord_y <- sapply(str_split(datbind$coord, " "), "[", 2)

# Group by site, year and type, calculate mean freq
datbind %>%
  filter(nyear > 1) %>%
  group_by(Site, Year, type) %>%
  summarise(site.mean = mean(freq, na.rm = TRUE),
            Date = unique(Date),
            restyear = unique(restyear),
            restdate = unique(restdate)) %>%
  mutate(Date = as.Date(Date),
         restdate = as.Date(restdate)) %>%
  filter(!is.na(type)) %>% # need to investigate why NAs in type
  ggplot(aes(Date, site.mean, colour = type)) +
  geom_vline(aes(xintercept = as.numeric(restdate))) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Site, scales = "free_x") +
  scale_x_date(date_labels = "%y") +
  labs(title = "Frequency of functional groups", subtitle = "Data points are means",
       x = "Year", y = "Mean frequency", colour = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0))

ggsave(paste0(fpath, "functional-group-freq.png"), width = 297, height = 210, units = "mm")
  

# Bind historical and contemporary matrices
matbind <-
  bind_rows(histdat, contdat)

# Add a species richness variable
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


matbind$rich.all <- rowSums(allsppmat > 0, na.rm = TRUE)
matbind$rich.pos <- rowSums(posmat > 0, na.rm = TRUE)
matbind$rich.neg <- rowSums(negmat > 0, na.rm = TRUE)
matbind$rich.grass <- rowSums(grassmat > 0, na.rm = TRUE)
matbind$rich.forb <- rowSums(forbmat > 0, na.rm = TRUE)
matbind$rich.leg <- rowSums(legmat > 0, na.rm = TRUE)

# Plot overall species richness against pos and neg
matbind %>%
  ggplot(aes(rich.all, rich.pos)) +
  geom_smooth() +
  geom_point() +
  labs(x = "Overall species richness", y = "Positive indicator richness") +
  theme_minimal()

matbind %>%
  ggplot(aes(rich.all, rich.neg)) +
  geom_smooth() +
  geom_point() +
  labs(x = "Overall species richness", y = "Negative indicator richness") +
  theme_minimal()

# Plot species richness by change over time by site
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
  
ggsave(paste0(fpath, "all-species-richness.png"), height = 210, width = 297, units = "mm")

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

ggsave(paste0(fpath, "functional-group-richness.png"), height = 210, width = 297, units = "mm")

# Summarise to % change in richness by site

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

start.end.rich.tidy$type[start.end.rich.tidy$type == "leg"] <- "legumes"
start.end.rich.tidy$type[start.end.rich.tidy$type == "neg"] <- "negative"
start.end.rich.tidy$type[start.end.rich.tidy$type == "pos"] <- "positive"

  
start.end.rich.tidy %>%
  ggplot(aes(first, last)) +
    geom_point() +
    geom_abline(lty = 2) +
    facet_wrap(~ type, scales = "free") +
    coord_fixed() +
    theme_minimal() +
    theme(panel.border = element_rect(colour = "black", fill = NA)) +
    labs(x = "Initial richness", y = "Final richness")

ggsave(paste0(fpath, "first-last-richness-groups.png"), height = 210, width = 297, units = "mm")


# Parse NGR grid refs into coordinates
matbind$coord <- str_sub(matbind$NGR, start = 3)
matbind$coord_x <- sapply(str_split(matbind$coord, " "), "[", 1)
matbind$coord_y <- sapply(str_split(matbind$coord, " "), "[", 2)


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


rich.pdiff$Region <- str_sub(rich.pdiff$Site, 1, 4)
# This is all very messy - needs redoing
rich.pdiff <- 
  matbind %>%
  group_by(Site) %>%
  filter(!duplicated(Site)) %>%
  select(Area) %>%
  right_join(., rich.pdiff, by = "Site")

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

ggsave(paste0(fpath, "func-rich-change.png"), height = 210, width = 297, units = "mm")

rich.pdiff %>%
  filter(type == "all") %>%
  ggplot(aes(reorder(Site, rich.pdiff), rich.pdiff, fill = rich.pdiff)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "#8e0152", mid = "white", high = "#276419") +
  labs(title = "Change in species richness", y = "Change (%)", x = "Site", fill = "Change (%)") +
  coord_flip() +
  # guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0),
        strip.text = element_text(hjust = 0),
        axis.title.y = element_blank())

ggsave(paste0(fpath, "rich-change.png"), height = 297, width = 210, units = "mm")

# Plot change in positive indicator richness
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

ggsave(paste0(fpath, "pos-rich-change.png"), height = 297, width = 210, units = "mm")

# Plot change in negative indicator richness
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

ggsave(paste0(fpath, "neg-rich-change.png"), height = 297, width = 210, units = "mm")


# Positive and negative indicator trends
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

ggsave(paste0(fpath, "freq-targets.png"), width = 297, height = 210, units = "mm")

# Differences in change in species richness by region

rich.pdiff %>%
  ggplot(aes(Area, rich.pdiff, fill = Area)) +
  geom_boxplot() +
  facet_wrap(~ type, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_blank())

# Some stats!!! Differences in % change in richness by area

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

anova(rich.pdiff.all.gls)
anova(rich.pdiff.grass.gls)
anova(rich.pdiff.leg.gls)
anova(rich.pdiff.forb.gls)

# Map sites

#Convert to spatial points dataframe for reprojection
meadow_coords <- 
  datbind %>%
    filter(!duplicated(Site)) %>%
    filter(!is.na(easting)) %>%
    select(easting, northing, Area) 

# write.table(meadow_coords, paste0(fpath, "meadow_coords.txt"))

meadow_coords <- cbind(Easting = meadow_coords$easting, Northing = meadow_coords$northing)

meadow_data <-
  datbind %>%
  filter(!duplicated(Site)) %>%
  filter(!is.na(easting)) %>%
  select(Area, Site)

meadow_data <-
  left_join(meadow_data,
            rich.pdiff %>% filter(type == "all"),
            by = "Site")
  
meadow_data <- as.data.frame(meadow_data)

meadow_sp <- SpatialPointsDataFrame(meadow_coords,
                                    data = meadow_data,
                                    proj4string = CRS("+init=epsg:27700"))

meadow_latlong <- spTransform(meadow_sp, CRS("+init=epsg:4326"))
meadow_fort <- fortify(meadow_latlong)

meadow_latlong_df <- as.data.frame(meadow_latlong)

meadow_latlong_df %>%
  ggplot(aes(Easting, Northing)) +
  geom_point()

bbox1 <- make_bbox(lon = c(-3.5, -2.9), lat = c(54.35, 54.7), f = 0.1)

meadmap1 <- get_map(bbox1, maptype = "roadmap")

ggmap(meadmap1) +
  geom_point(data = meadow_latlong_df %>% filter(!is.na(rich.pdiff)),
             aes(Easting, Northing, fill = Site),
             shape = 21) +
  # scale_fill_gradient2(low = "#8e0152", mid = "white", high = "#276419") +
  labs(x = "Longitude", y = "Latitude", fill = "Site") +
  theme(legend.position = "none")

ggsave(paste0(fpath, "meadmap1.png"), width = 15, height = 15, units = "cm")

bbox2 <- make_bbox(lon = c(-2.9, -2.0), lat = c(54.35, 54.7), f = 0.1)

meadmap2 <- get_map(bbox2, maptype = "roadmap")

ggmap(meadmap2) +
  geom_point(data = meadow_latlong_df %>% filter(!is.na(rich.pdiff)),
             aes(Easting, Northing, fill = Site),
             shape = 21) +
  # scale_fill_gradient2(low = "#8e0152", mid = "white", high = "#276419") +
  labs(x = "Longitude", y = "Latitude", fill = "Site") +
  theme(legend.position = "none")

ggsave(paste0(fpath, "meadmap2.png"), width = 15, height = 15, units = "cm")

bbox3 <- make_bbox(lon = c(-3.2, -2.3), lat = c(54.2, 54.5), f = 0.1)

meadmap3 <- get_map(bbox3, maptype = "roadmap")

ggmap(meadmap3) +
  geom_point(data = meadow_latlong_df %>% filter(!is.na(rich.pdiff)),
             aes(Easting, Northing, fill = Site),
             shape = 21) +
  # scale_fill_gradient2(low = "#8e0152", mid = "white", high = "#276419") +
  labs(x = "Longitude", y = "Latitude", fill = "Site") +
  theme(legend.position = "none")

ggsave(paste0(fpath, "meadmap3.png"), width = 15, height = 15, units = "cm")

# ggsn will add a north arrow and scale bar but need to update R first

# Analyses:
# Paired t-tests: pre and post effects on frequency pos and neg indicators
# Correlate pos indicators with overall sp richness
# anova: effect of broad region on frequency of pos / neg indicators

# Paired t-test

matbind.prepost <- 
matbind %>%
  group_by(Site) %>%
  mutate(prepost = ifelse(Year == min(Year, na.rm = TRUE), "start", ifelse(Year == max(Year, na.rm = TRUE), "end", NA))) %>%
  mutate(prepost = as.factor(prepost)) %>%
  filter(!is.na(prepost))

glimpse(matbind.prepost)

t.test(rich.all ~ prepost, data = matbind.prepost, paired = TRUE)
t.test(rich.grass ~ prepost, data = matbind.prepost, paired = TRUE)
t.test(rich.forb ~ prepost, data = matbind.prepost, paired = TRUE)
t.test(rich.leg ~ prepost, data = matbind.prepost, paired = TRUE)

# # Do a quick PCA on the richness and frequency data
# # (for selected years)
# 
# # Which years have the most survey sites?
# 
# matbind %>%
#   count(Year)
# 
# matbind <-
#   matbind %>%
#   separate(Site, c("grid", "Sitename"), "-", remove = FALSE)
# 
# matbind$grid <- as.factor(matbind$grid)
# 
# gridlv <- levels(matbind$grid)
# gridlv
# 
# # Custom colour palette taken from http://stackoverflow.com/questions/9563711/r-color-palettes-for-many-data-classes#comment49070918_9568659
# colvec <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00", "black", "gold1", "skyblue2", "palegreen2", "#FDBF6F", "gray70", "maroon", "orchid1", "darkturquoise", "darkorange4", "brown")
# 
# 
# # 2012 is the first big year for survey data
# 
# spp12 <- allsppmat[matbind$Year == 2012, ]
# spp12[is.na(spp12)] <- 0
# 
# spp11 <- allsppmat[matbind$Year == 2011, ]
# spp11[is.na(spp11)] <- 0
# 
# spp12 <- allsppmat[matbind$Year == 2012, ]
# spp12[is.na(spp12)] <- 0
# 
# spp13 <- allsppmat[matbind$Year == 2013, ]
# spp13[is.na(spp13)] <- 0
# 
# spp14 <- allsppmat[matbind$Year == 2014, ]
# spp14[is.na(spp14)] <- 0
# 
# spp15 <- allsppmat[matbind$Year == 2015, ]
# spp15[is.na(spp15)] <- 0
# 
# 
# 
# rich10.pca <- rda(spp10, scale = TRUE)
# plot(rich10.pca, type = "n", scaling = 3)
# with(matbind[matbind$Year == 2010, ],
#      points(rich10.pca, display = "sites", col = colvec[grid], scaling = 3, pch = 19))
# # with(matbind[matbind$Year == 2010, ],
# #      text(rich10.pca, display = "species", scaling = 3, cex = 0.8, col = "darkcyan"))
# with(matbind[matbind$Year == 2010, ],
#      legend("topright", legend = gridlv, bty = "n", col = colvec, pch = 19))
# 
# 
# rich11.pca <- rda(spp11, scale = TRUE)
# plot(rich11.pca, type = "n", scaling = 3)
# with(matbind[matbind$Year == 2011, ],
#      points(rich11.pca, display = "sites", col = colvec[grid], scaling = 3, pch = 19))
# # with(matbind[matbind$Year == 2011, ],
# #      text(rich11.pca, display = "species", scaling = 3, cex = 0.8, col = "darkcyan"))
# with(matbind[matbind$Year == 2011, ],
#      legend("topright", legend = gridlv, bty = "n", col = colvec, pch = 19))
# 
# rich12.pca <- rda(spp12, scale = TRUE)
# plot(rich12.pca, type = "n", scaling = 3)
# with(matbind[matbind$Year == 2012, ],
#      points(rich12.pca, display = "sites", col = colvec[grid], scaling = 3, pch = 19))
# # with(matbind[matbind$Year == 2012, ],
# #      text(rich12.pca, display = "species", scaling = 3, cex = 0.8, col = "darkcyan"))
# with(matbind[matbind$Year == 2012, ],
#      legend("topright", legend = gridlv, bty = "n", col = colvec, pch = 19))
# 
# rich13.pca <- rda(spp13, scale = TRUE)
# plot(rich13.pca, type = "n", scaling = 3)
# with(matbind[matbind$Year == 2013, ],
#      points(rich13.pca, display = "sites", col = colvec[grid], scaling = 3, pch = 19))
# # with(matbind[matbind$Year == 2013, ],
# #      text(rich13.pca, display = "species", scaling = 3, cex = 0.8, col = "darkcyan"))
# with(matbind[matbind$Year == 2013, ],
#      legend("topright", legend = gridlv, bty = "n", col = colvec, pch = 19))
# 
# rich14.pca <- rda(spp14, scale = TRUE)
# plot(rich14.pca, type = "n", scaling = 3)
# with(matbind[matbind$Year == 2014, ],
#      points(rich14.pca, display = "sites", col = colvec[grid], scaling = 3, pch = 19))
# # with(matbind[matbind$Year == 2014, ],
# #      text(rich14.pca, display = "species", scaling = 3, cex = 0.8, col = "darkcyan"))
# with(matbind[matbind$Year == 2014, ],
#      legend("topright", legend = gridlv, bty = "n", col = colvec, pch = 19))
# 
# rich15.pca <- rda(spp15, scale = TRUE)
# plot(rich15.pca, type = "n", scaling = 3)
# with(matbind[matbind$Year == 2015, ],
#      points(rich15.pca, display = "sites", col = colvec[grid], scaling = 3, pch = 19))
# # with(matbind[matbind$Year == 2015, ],
# #      text(rich15.pca, display = "species", scaling = 3, cex = 0.8, col = "darkcyan"))
# with(matbind[matbind$Year == 2015, ],
#      legend("topright", legend = gridlv, bty = "n", col = colvec, pch = 19))