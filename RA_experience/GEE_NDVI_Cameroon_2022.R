# --------------------------------------------------------------------------- #
#
#  Jin Yao, 2022/07
#
# --------------------------------------------------------------------------- #
#
#  THIS FILE: USE GEE TO EXTRACT NDVI IN CAMEROON
#
# --------------------------------------------------------------------------- #
# 1. Prepare to use GEE 
#
# Note: 1) First, you may need to sign up to the platform using google account [GEE](https://signup.earthengine.google.com/#!/);
#       2) Then, follow the codes below to initialize the Python virtual environment;
#       3) Then, I will introduce several projects to extract data from satellite, especially extracting data by self-defined boundaries.
# --------------------------------------------------------------------------- #
# --- PACKAGE
require("rgee")
require("reticulate") # upload reticulate before writing python code
require("rgeeExtra")
ee_Initialize() # initialize GEE 
gm <- import("geemap")
require("st")
require("sf")
require("rnaturalearth") # use function ne_countries to extract polygon of country
require("ggplot2")
require("dplyr")
require("tidyr")
require("data.table")
# --------------------------------------------------------------------------- #
# 2. Extract NDVI Data
#
# The first project is extracting NDVI data of Cameroon of year 2013. The observation unit is ethnic groups. 
# --------------------------------------------------------------------------- #
# --- 2.1 Step 1

# Extract geometry feature of Cameroon via uploaded shapefile. 
# Refer [Here](https://thegeoict.com/blog/2019/08/05/uploading-a-shapefile-to-google-earth-engine/). 

# Extract ethnic feature from GEE platform
ethnologue_col <- ee$FeatureCollection('users/jinyao/langa_no_overlap_biggest_clean')
feature_ethnologue_cam <- ethnologue_col$filter(ee$Filter$eq('LMP_C1', 'Cameroon'))

# Collect the feature of country of interest
countries <- ee$FeatureCollection('USDOS/LSIB_SIMPLE/2017')
selected <- countries$filter(ee$Filter$eq('country_na', 'Cameroon')) # filter the country we want

# First interactive map to show the simple polygons of ethnic groups
Map$centerObject(selected) # center the map view
Map$addLayer(feature_ethnologue_cam$style('black',fillColor="00000000", width = 0.5))
# --------------------------------------------------------------------------- #
# --- 2.2 Step 2: Visualize a satellite map of NDVI

nl_viirs <- ee$ImageCollection('MODIS/006/MOD13A2') %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%           # monthly data
  ee$ImageCollection$map(function(x) x$select('NDVI')) %>%                # select band(s)
  ee$ImageCollection$toBands() %>%                                        # from imagecollection to image
  ee$Image$clip(feature_ethnologue_cam) %>%                               # clip the image feature as our defined boundaries  
  ee$Image$reduce(ee$Reducer$mean())                                      # select mean value of the time period

visParams = list(
  min = 0.0,
  max = 9000.0,
  palette = c(
    'FFFFFF', 'CE7E45', 'DF923D', 'F1B555', 'FCD163', '99B718', '74A901',
    '66A000', '529400', '3E8601', '207401', '056201', '004C00', '023B01',
    '012E01', '011D01', '011301'
  )
)

Map$centerObject(selected)
Map$addLayer(nl_viirs, visParams) + # base layer 
  Map$addLayer(feature_ethnologue_cam$style('white',fillColor="00000000", width = 0.5))  # add our defined boundaries 
# --------------------------------------------------------------------------- #
# --- 2.3 Step 3: Extract NDVI data from satellite image

# Mini Steps: 1) Extract the time series data of the entailed polygons;
#             2) First, we use ImageCollection() to select the image we are interested via [Code Earth Engine](https://code.earthengine.google.com/);
#             3) Then use filterDate() to select our interested period. select() is to select our band (NDVI in this example);
#             4) Then toBands() converts imagecollection the image. 
# --------------------------------------------------------------------------- #

nl_viirs <- ee$ImageCollection('MODIS/006/MOD13A2') %>%
  ee$ImageCollection$filterDate("2013-01-01", "2014-01-01") %>%           # monthly data
  ee$ImageCollection$map(function(x) x$select('NDVI')) %>%                # select band(s)
  ee$ImageCollection$toBands() %>%                                        # from imagecollection to image
  ee$Image$clip(feature_ethnologue_cam)                                   # clip the image feature as our defined boundaries  

# Then we use ```ee_extract()``` to extract our data. x is the image where we extract data from. 
# y is where the image selection based, we use "FID_langa", then the data will be extracted based on the observation units of "FID_langa". 

NDVI_af_ethnologue_cameroon <- ee_extract(x = nl_viirs,                   
                                          y = feature_ethnologue_cam$select("FID_langa"),  
                                          sf = FALSE)
tibble(head(NDVI_af_ethnologue_cameroon, 5))

# --------------------------------------------------------------------------- #
# --- 2.4 Step 4: Clean the data and draw time series plot

NDVI_af_ethnologue_cameroon %>%
  tidyr::pivot_longer(-FID_langa, names_to = "day_obs", values_to = "NDVI") %>%
  mutate(day_obs, day_obs=gsub("X2013_", "", day_obs) %>%
                          gsub("_NDVI", "", .) %>%
                          gsub("_", "-", .)) %>%
  ggplot(aes(x = day_obs, y = NDVI, group = FID_langa, color = NDVI)) +
  geom_line(alpha = 0.4) +
  xlab("date of the observation") +
  ylab("NDVI") +
  theme_minimal() +
  ggtitle("NDVI change in Cameroon: Year 2013(Observation gaps are 16 days)") +
  scale_x_discrete(breaks = c("01-01", "04-07", "07-12", "10-16"))

# --------------------------------------------------------------------------- #





