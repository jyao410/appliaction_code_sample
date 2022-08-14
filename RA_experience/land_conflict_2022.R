#
#  Jin Yao, 2022/07
#
############### Land-related Project and Conflict ###############

options("rgdal_show_exportToProj4_warnings"="none")
devtools::install_github("jonathandroth/staggered")
require(staggered)     # load the staggered package
require(tidyverse)
require(readxl)
require(haven)
require(purrr)
require(foreign)
require(ggplot2)
require(rgdal)
require(rgeos)
require(RColorBrewer)  # creates nice color schemes
require(maptools)      # loads sp library too
require(scales)        # customize scales
require(gridExtra)     # mutiple plots
require(plyr)          # join function
require(dplyr) 
require(mapproj)       # projection tools  
require(raster)        # raster tools
require(animation)     # Saving GIFs
require(tidyr)    
require(readstata13)    
require(gstat)         # interpolation tools
require(ncdf4)      
require(Hmisc)
require(lubridate)
require(lmtest)
require(sandwich)
require(dotwhisker)    # coef plots
require(broom)
require(binsreg)
require(stringi)
require(stringr)
require(GSIF)          # soil data package
require(gdata)
require(exactextractr) # faster extract
require(sf)            # faster extract
require(elevatr)       # elevation data
require(data.table)
require(naniar)        # replace_with_na_all function

# -------- SET WORKING DIRECTORY --------

user <- Sys.info()["user"]
setwd(paste("/Users/", user, "/Dropbox/RA_Jin", sep=""))

# ----------- FILE OVERVIEW -------------

# 1) loads UCDP Conflict data 
# 2) loads World Bank project data
# 3) loads priogrid data
# 4) merge data, grid-based foramt
# 5) preliminary analysis

# ----------- DATA OVERVIEW -------------

# 1) UCDP Conflict data: "./Data/UCDP/ged211.xlsx"
# 2) World Bank project data: "./Stata/output/wb_landprojects.dta"
# 3) priogrid data: "./Data/PRIO_GRID/priogrid_cellshp/priogrid_cell.shp"

# ------------ READ DATA --------------

ged211 <- read_xlsx("./Data/UCDP/ged211.xlsx")
wb_landprojects <- read_dta("./Stata/output/wb_landprojects.dta")
priogrid <- st_read(dsn = "./Data/PRIO_GRID/priogrid_cellshp/priogrid_cell.shp", quiet = TRUE)

# ------------ MERGE DATA --------------

# Projection
wgs84_proj <- "+proj=longlat +ellps=WGS84 +datum=WGS84"   # WGS 1984
mercator <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs" # Project to mercator to calculate distance in meters

# Merge the conflict (ged211) data with priogrid
priogrid.ged211 <- inner_join(priogrid,ged211,            # merge ged211 and priogrid_gid
                              by=c("gid"="priogrid_gid"))

priogrid.ged211.ym <- priogrid.ged211 %>%                 # create date variable (date of the conflict start): yyyy-mm
                      mutate(y_m=format(date_start, format="%Y-%m")) %>%
                      mutate(country_merge=country) %>%
                      mutate(country_merge = replace(country_merge, country_merge == "DR Congo (Zaire)", "Congo, Republic of")) %>%
                      mutate(country_merge = replace(country_merge, country_merge == "Madagascar (Malagasy)", "Madagascar"))

# Merge the World Bank land project data (wb_landprojects) with priogrid
wb_landprojects <- wb_landprojects %>%
                   dplyr::mutate(start_actual_isodate=as.character(start_actual_isodate)) %>%
                   dplyr::mutate(newdate=strptime(start_actual_isodate, "%Y-%m-%d"))

landprojects.arrange <- wb_landprojects %>%
                        mutate(y_m=format(newdate, format="%Y-%m")) %>% 
                        filter(!is.na(longitude) & !is.na(latitude))

landprojects.spatial <- sp::SpatialPointsDataFrame(coords=data.frame(x=as.numeric(landprojects.arrange$longitude),
                                                                     y=as.numeric(landprojects.arrange$latitude)),
                                                   data=landprojects.arrange, 
                                                   proj4string=CRS((wgs84_proj)))
landprojects.sf <- st_as_sf(landprojects.spatial) 

priogrid.sf <- st_make_valid(priogrid)  # make valid sf
priogrid.landprojects.sf <- st_join(landprojects.sf, priogrid.sf, join = st_within)  # find points within polygons
priogrid.landprojects.geo.drop <- priogrid.landprojects.sf %>% 
                            st_drop_geometry() %>%
                            mutate(country_merge=COUNTRY) %>%
                            mutate(country_merge = replace(country_merge, country_merge == "Cambodia", "Cambodia (Kampuchea)")) %>%
                            mutate(country_merge = replace(country_merge, country_merge == "Cote d'Ivoire", "Ivory Coast")) %>%
                            mutate(country_merge = replace(country_merge, country_merge == "Lao People's Democratic Republic", "Laos")) %>%
                            mutate(country_merge = replace(country_merge, country_merge == "Macedonia, former Yugoslav Republic of", "Macedonia, FYR")) %>%
                            mutate(country_merge = replace(country_merge, country_merge == "Serbia", "Serbia (Yugoslavia)"))

# Merge the conflict (ged211) data and land project data (wb_landprojects)
priogrid.ged211.geo.drop <- st_drop_geometry(priogrid.ged211.ym) 
landprojects.ged211 <- priogrid.ged211.geo.drop %>% 
                       full_join(priogrid.landprojects.geo.drop, by=c("country_merge","gid","y_m"))

# Create variables: 1) "project_start_ind": indicator of start of a World Bank project; 
#                   2) "conflict_start_ind": indicator of a start of a conflict;
#                   3) "conflict_month": number of conflicts within the grid per month;
#                   4) "project_start_date": first land project date;
#                   5) "period": time period

landprojects.ged211.conflict.sum <- landprojects.ged211 %>%  # create variable summing conflict per month: conflict_month 
                                    mutate(conflict_start_ind=ifelse(is.na(date_start),0,1)) %>% 
                                    group_by(gid,y_m) %>%
                                    summarise_at(vars(conflict_start_ind),
                                                 list(conflict_month=sum)) 

landprojects.ged211.new <- landprojects.ged211 %>%  # create indicator of start of a project "project_start_ind"             
                           mutate(project_start_ind=ifelse(is.na(project_id),0,1)) %>%  
                           left_join(landprojects.ged211.conflict.sum) 

landprojects.ged211.new.not.na <- landprojects.ged211.new %>% subset(!is.na(project_id))
