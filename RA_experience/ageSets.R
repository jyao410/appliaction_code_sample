#
#  Jin Yao, 2022/07
#
# ##################### GOAL #########################

# ------- Data Overview -------

# two sets of data under Data/Agesets/: 
# (i) agesets.dta is data that contains ethnic group names + 
# a indicator for whether an ethnic group in Africa had ageset practices or not
# (ii) a shapefile of ethnic groups in Africa from the murdock map 
# (just in case the name in (i) doesn't match the ethnologue data and matches 
# this murdock map data instead). 

# ------- Task Overview -------

# ---------- First ---------- #
# For each WB project in Africa, assign the ethnic group it falls in 
# (spatially, can use coordinates here) and whether that ethnic group had age 
# sets or not

# ---------- Second --------- #
# For each WB project in Africa, determine whether the project sector 
# is under "governance". (Alternatively, can see if the project description 
# involves "community development", a common name for governance projects.) 

# ----------- Third --------- #
# run regressions comparing the outcome rating of a WB project (IEG_rating 
# I think it's called) for ethnic groups with/without agesets for governance 
# and non-governance projects.

# ---------- Package -------- #

rm(list = ls())  # clear variables

require(sp)
require(sf)
require(dplyr)
require(readstata13)
require(tidyr)
require(data.table)
require(fixest)

# ############## SET WORKING DICTIONARY ##############

user <- Sys.info()["user"]
setwd(paste("/Users/",user,"/Dropbox/RA_Jin",sep=""))

# ################### PROJECTION #####################

wgs84_proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84" # WGS 1984
mercator <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs" 

# #################### READ DATA #####################

# age set data
agesets.dta <- read.dta13("./Data/Agesets/ageset_data.dta")

# murdock data
murdock.shp <- st_read(
  dsn = "./Data/Agesets/borders_tribes.shp",
  quiet = TRUE)

# ethnologue data
ethnologue.shp <- st_read(
  dsn = "./Data/Ancestral_Characteristics_Database_Language_Level/Ethnologue_16_shapefile/langa_no_overlap_biggest_clean.shp", 
  quiet = TRUE)

# projects.csv: more projects, 5000+
WB_project <- read_csv("./Data/WorldBank_GeocodedResearchRelease_Level1_v1.4.2/data/projects.csv")
WB_project_loc <- read_csv("./Data/WorldBank_GeocodedResearchRelease_Level1_v1.4.2/data/locations.csv")
projects_ancillary <- read_csv("./Data/WorldBank_GeocodedResearchRelease_Level1_v1.4.2/data/projects_ancillary.csv")

user <- Sys.info()["user"]
setwd(paste("/Users/",user,"/Dropbox/Research_Proposal",sep=""))

# ################### MAIN CODE ######################

# ------------------------------------------ #
# ------------ Preliminary Clean -------------
# ------------------------------------------ #

# ---------------- murdock ----------------- #

murdock <- murdock.shp %>% 
  mutate(NAM_MURDOCK = toupper(NAME)) %>%
  mutate(TRIBE_CODE_MURDOCK = TRIBE_CODE) %>%
  mutate(LAT_MURDOCK = LAT) %>%
  mutate(LON_MURDOCK = LON) %>%
  dplyr::select(NAM_MURDOCK, TRIBE_CODE_MURDOCK, LAT_MURDOCK, LON_MURDOCK)

murdock.sf <- st_make_valid(murdock)

# -------------- WB projects --------------- #

WB_all.dta <- WB_project %>%  # merge the project data
  left_join(WB_project_loc) %>%
  left_join(projects_ancillary %>% mutate(project_id=`PROJECT ID`)) %>%
  filter(!is.na(longitude) & !is.na(latitude))

# ----------- merge WB and murdock --------- #

WB_all.dta.spatial <- SpatialPointsDataFrame(coords = data.frame(x=as.numeric(WB_all.dta$longitude),
                                                                 y=as.numeric(WB_all.dta$latitude)),
                                             data = WB_all.dta, 
                                             proj4string=CRS((wgs84_proj)))
WB_all.dta.sf <- st_as_sf(WB_all.dta.spatial)

WB_all.dta.af.sf <- WB_all.dta.sf %>%  # only select the projects in Africa
  subset(REGION=="AFRICA")

WB_all.dta.af.in.murdock <- st_join(WB_all.dta.af.sf, murdock.sf, join = st_within)
WB_all.dta.af.in.murdock <- WB_all.dta.af.in.murdock %>% 
  mutate(NAME = toupper(NAM_MURDOCK))

# ------ check WB_all.dta.af.in.murdock ---- #

length(agesets.dta$NAME %in% WB_all.dta.af.in.murdock$NAME %>% .[.!=TRUE]) 

WB_all.dta.af.not.murdock <- WB_all.dta.af.in.murdock %>%  # check if the projects that are not in murdock in ethnologue
  subset(is.na(NAME)) %>%
  select(!NAME)

WB_all.dta.af.is.murdock <- WB_all.dta.af.in.murdock %>%  # keep the part that are not NA of the names
  subset(!is.na(NAME))

# ------------ check ethnologue ------------ #

ethnologue.sf <- ethnologue.shp %>% st_make_valid()
ethnologue.sf <- ethnologue.sf %>%
  mutate(NAM_ETHNOLOGUE = toupper(NAM_LABEL))

WB_all.dta.af.in.ethnologue <- st_join(WB_all.dta.af.not.murdock, ethnologue.sf, join = st_within)
WB_all.dta.af.in.ethnologue <- WB_all.dta.af.in.ethnologue %>%
  mutate(NAME = NAM_ETHNOLOGUE)

# --------- final merge to analyze --------- #

list_WB_in_murdock_eth <- list(WB_all.dta.af.is.murdock,
                               WB_all.dta.af.in.ethnologue)
WB_in_murdock_eth <- list_WB_in_murdock_eth %>% 
  rbindlist(fill = TRUE)

WB_ageSets <- WB_in_murdock_eth %>%
  left_join(agesets.dta) %>%
  drop_na(agesets)

# ------------------------------------------ #
# ------ Create Indicator of Governance ------
# ------------------------------------------ #

# Method: create variable based on the title of the project: PROJECT_TITLE and `MJTHEME 1`
#         "1" represents including string of both "COMMUNITY" and "DEVELOPMENT" appearing in the project name or "governance" in `MJTHEME 1`
#         "0" otherwise

WB_ageSets <- WB_ageSets %>% 
  mutate(PROJECT_TITLE = toupper(project_title)) %>%  
  mutate(idt_governance = ifelse((grepl("COMMUNITY", PROJECT_TITLE) & grepl("DEVELOPMENT", PROJECT_TITLE)) | 
                                   grepl("governance", `MJTHEME 1`), 1, 0)) %>%
  mutate(WB_Rating = ifelse(IEG_Outcome == "Highly Unsatisfactory", 1, 
                            ifelse(IEG_Outcome == "Unsatisfactory", 2,
                                   ifelse(IEG_Outcome == "Moderately Unsatisfactory", 3,
                                          ifelse(IEG_Outcome == "Moderately Satisfactory", 4,
                                                 ifelse(IEG_Outcome == "Satisfactory", 5,
                                                        ifelse(IEG_Outcome == "Highly Satisfactory", 6, "NA")))))))


# ------------------------------------------ #
# ---------------- Regression ----------------
# ------------------------------------------ #

# Reminder: indicator of governance (idt_governance) is 1 if governance

# index(WB_Rating) ~ indicator(agesets)

# ------------ parsimonious linear --------- #

cbind(WB_ageSets$IEG_Outcome, WB_ageSets$WB_Rating)

# for governance projects
WB_ageSets_gov <- WB_ageSets %>%
  subset(idt_governance == "1") %>%
  mutate(idt_governance = as.factor(idt_governance)) %>%
  mutate(agesets = as.factor(agesets)) %>%
  mutate(WB_Rating = as.numeric(WB_Rating))

# many missing values of WB_Rating or IEG_Outcome
mod_1_parsi_gov <- fixest::feols(WB_Rating ~ agesets, 
                                 cluster = "project_location_id",
                                 data = WB_ageSets_gov)

summary(mod_1_parsi_gov)
mean(WB_ageSets_gov$WB_Rating %>% na.omit())

# for non-governance projects
WB_ageSets_non_gov <- WB_ageSets %>%
  subset(idt_governance == "0") %>%
  mutate(idt_governance = as.factor(idt_governance)) %>%
  mutate(agesets = as.factor(agesets)) %>%
  mutate(WB_Rating = as.numeric(WB_Rating))

mod_1_parsi_non_gov <- fixest::feols(WB_Rating ~ agesets, 
                                     cluster = "project_location_id",
                                     data = WB_ageSets_non_gov)

summary(mod_1_parsi_non_gov)
mean(WB_ageSets_non_gov$WB_Rating %>% na.omit())

# combine as interaction

mood_1_parsi_inter <- fixest::feols(as.numeric(WB_Rating) ~ as.factor(agesets)*as.factor(idt_governance), 
                                    cluster = "project_location_id",
                                    data = WB_ageSets)
summary(mood_1_parsi_inter)

sum_list_parsi <- list(summary(mod_1_parsi_gov),
                       summary(mod_1_parsi_non_gov),
                       summary(mood_1_parsi_inter))
mean(WB_ageSets$WB_Rating %>% as.numeric() %>% na.omit())

etable(sum_list_parsi, tex=TRUE, title = "Parsimonious OLS Estimate: Age Sets and WB Rating")

# ----------- cluster and FE linear -------- #

# create year variable
WB_ageSets_year <- WB_ageSets %>%
  dplyr::mutate(start_actual_isodate = as.character(start_actual_isodate)) %>%
  dplyr::mutate(year = substr(start_actual_isodate, 0, 4))

# ------- for governance projects 

WB_ageSets_year_gov <- WB_ageSets_year %>%
  subset(idt_governance == "1") %>%
  mutate(idt_governance = as.factor(idt_governance)) %>%
  mutate(agesets = as.factor(agesets)) %>%
  mutate(WB_Rating = as.numeric(WB_Rating))

# many missing values of WB_Rating or IEG_Outcome
mod_2_gov <- fixest::feols(WB_Rating ~ agesets|COUNTRY, 
                           cluster = "project_location_id",
                           data = WB_ageSets_year_gov)

summary(mod_2_gov)
mean(WB_ageSets_year_gov$WB_Rating %>% na.omit())


# ----- for non-governance projects 

WB_ageSets_year_non_gov <- WB_ageSets_year %>%
  subset(idt_governance == "0") %>%
  mutate(idt_governance = as.factor(idt_governance)) %>%
  mutate(agesets = as.factor(agesets)) %>%
  mutate(WB_Rating = as.numeric(WB_Rating))

mod_2_non_gov <- fixest::feols(WB_Rating ~ agesets|COUNTRY, 
                               cluster = "project_location_id",
                               data = WB_ageSets_year_non_gov)

summary(mod_2_non_gov)
mean(WB_ageSets_year_non_gov$WB_Rating %>% na.omit())

# combine as interaction
mood_2_inter <- fixest::feols(as.numeric(WB_Rating) ~ as.factor(agesets)*as.factor(idt_governance)|COUNTRY, 
                              cluster = "project_location_id",
                              data = WB_ageSets_year)
summary(mood_2_inter)

sum_list_2 <- list(summary(mod_2_gov),
                   summary(mod_2_non_gov),
                   summary(mood_2_inter))
mean(WB_ageSets$WB_Rating %>% as.numeric() %>% na.omit())

etable(sum_list_2, tex=TRUE, title = "OLS Estimate: Age Sets (FE and Clustered) and WB Rating")






















