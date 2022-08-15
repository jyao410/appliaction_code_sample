
# ############################################# #
# -------------------- JY --------------------- # 
# ############################################# #
# 
# AUGUST, 2022
#
# -------------------------------------------------------------------------- #
#
# THIS FILE: 1) SERVES AS A BENCHMARK FOR HOW WILL THE FUTURE CODE BE LIKE;
#            2) REWRITE THE FORMER STUDENT'S WORK AND HAS SOME MODIFICATIONS;
#            3) TBC
#
# -------------------------------------------------------------------------- #
# --- Packages
require(readxl)
require(data.table)
require(dplyr)
require(stringr)
require(sf)
require(st)
require(sp)
require(rgdal)
require(purrr)

# --- Functions
get.path <- function(dir, file_dir_name){
  file_dir_name = paste('/', file_dir_name, sep = "")
  paste(dir, file_dir_name, sep = "")
}

# --- Pathes
main_dir <- paste("/Users/", user, "/Dropbox/Jin", sep="")
main.path <- get.path(main_dir, "03. DRC Election")
dir.raw <- get.path(main.path, "01. Data/01. raw")
dir.temp <- get.path(main.path, "01. Data/02. temp")
dir.working <- get.path(main.path, "01. Data/03. working")
dir.output <- get.path(main.path, "01. Data/04. output")
dir.geo <- get.path(main.path, "01. Data/01. raw/06. geo")
# -------------------------------------------------------------------------- #



# ########################################################################## #
# ----------------------- PART 1: MERGE Ceni AND GSM -----------------------
# ########################################################################## #
#
# THIS SECTION: MERGE Ceni_cleaned.RData AND CARTO...KIVU.csv
# 
# ------------------------------ LOAD DATA --------------------------------- #
# --- Load Ceni_cleaned.RData 
load(get.path(dir.working, "Ceni_cleaned.RData"))

# --- Read CARTO...KIVU.csv 
GSM_NK <- read.csv(get.path(dir.raw,"01. Election Results 2018/CARTO RESEAU GSM NORD KIVU.csv")) %>%
  data.table
# -------------------------------------------------------------------------- #


# -------------------------------------------------------------------------- #
# Remove voting results and collapse by location
Ceni_cleaned <- data.combined %>%
  data.table %>%
  select(!c(candidat_id, voix)) %>%
  distinct %>%
  mutate(across(everything(), tolower))

GSM_cleaned <- GSM_NK %>%
  select(!c(N, RESEAU_GSM, PLUS_UTILI.SES)) %>% 
  rename(nom_sv = NOM.CENTRE.ENROLEMENT) %>%
  rename_all(tolower) %>%
  distinct %>%
  mutate(across(everything(), tolower)) 
# -------------------------------------------------------------------------- #


# -------------------------------------------------------------------------- #
# --- strings 
prefixes <- paste(unlist(c("c s ", "c\\.s ", "cs ", "cs. ", "e p 1 ", "e p ", "e.p ", "e\\.p\\. 2 ", "e\\.p\\. 1 ", "e\\.p. ", 
                           "e\\.p\\.1 ", "e\\.p\\.2 ", "e\\.p\\.a. ", "e\\.p.", "ep ", "ep\\. 1 ", "ep\\. 2 ", "ep\\. 3 ", "ep\\. ", "ep2 ", 
                           "epa ", "i t a ", "i\\. ", "i\\.t\\.p\\.i. ", "insitut ", "inst ", "inst\\. ", "inst\\.2 ", "inst\\.", 
                           "institut  ", "int ", "intitut ", "itav ", "itv ", "lycee ", "ps ", "complexe scolaire lecole primaire ", 
                           "complexe scolairecole primaire ", "ecole primaire ", "bureau quartier ","complexe scolaire ", 
                           "centre don bosco ", "complexe scolair", "tech. industr", "tech. agricole ", " tut","2 ", "3 ")), collapse = "|")

ep <- paste(unlist(c("e p 1 ", "e p ", "e\\.p ", "e\\.p. 2 ", "e\\.p. 1 ", "e\\.p. ", "e\\.p\\.1 ", "e\\.p\\.2 ", "e\\.p\\.a. ", "e\\.p.",
                     "ep\\. 1 ", "ep\\. 2 ", "ep\\. 3 ", "ep\\. ", "ep2 ", "epa ", "ep ", "ep\\.", "ep1 ", "e\\. p\\. ", "ep\\, ", "ep\\,", "e\\,p ")), collapse = "|")

cs <- paste(unlist(c("c s ", "c\\.s ", "cs ", "cs\\. ", "complexe scolaire lecole primaire ", "complexe scolairecole primaire ", "c\\.s\\. ")), collapse = "|")

inst <- paste(unlist(c("i t a ", "i\\. ", "i\\.t\\.p\\.i\\. ", "insitut ", "inst ", "inst\\. ", "inst\\.2 ", "inst\\.", "institut  ", "int ", 
                       "intitut ", "itav ", "itv ")), collapse = "|")

extra_prefixes_nom <- paste(unlist(c(" tut", "2 ", "3 ")), collapse = "|")

extra_prefixes_sett_pr <- paste(unlist(c("tut ", "2 ", "3 ")), collapse = "|")
# -------------------------------------------------------------------------- #


# -------------------------------------------------------------------------- #
# --- prepare Ceni_cleaned.NK for merge
# GET THE DATA FOR "nord-kivu" PROVINCE OF Ceni
Ceni_cleaned.NK <- Ceni_cleaned %>%
  subset(province == "nord-kivu") %>%
  subset(!clcr %in% c("goma ville", "butembo ville", "beni ville")) 

Ceni_cleaned.NK <- Ceni_cleaned.NK %>%
  .[, c("sett_pr", "clcr", "adresse_merge", "nom_sv_clean") := {
    
    sett_pr <- gsub(prefixes, "", Ceni_cleaned.NK[["nom_sv"]]) %>% gsub(extra_prefixes_sett_pr, "", .)
    
    clcr <- gsub(" ville", "", clcr)
    
    adresse_merge <- adresse_sv
    
    nom_sv_clean <- gsub(ep, "ecole primaire ", Ceni_cleaned.NK[["nom_sv"]]) %>% 
      gsub(cs, "complexe scolaire ", .) %>%
      gsub(inst, "institut ", .) %>%
      gsub("ps ", "ecole primaire et secondaire ", .) %>%
      gsub(extra_prefixes_nom, "", .)
    
    # VARIABLE ORDER METTERS: CONSISTANT WITH c("sett_pr", "clcr", "adresse_merge", "nom_sv_clean")
    list(sett_pr, clcr, adresse_merge, nom_sv_clean) } ]
# -------------------------------------------------------------------------- #


# -------------------------------------------------------------------------- #
# --- prepare GSM_cleaned.NK for merge
GSM_cleaned.NK <- GSM_cleaned[, c("clcr", "adresse_merge", "nom_sv_clean") := {
  
  # CREATE 'clcr' TO BE MERGED WITH 'clcr' IN Ceni_cleaned.NK
  clcr <- gsub(" ville", "", GSM_cleaned[["territoire_ville"]])
  
  adresse_merge <- GSM_cleaned[["adresse"]]
  
  # CLEAN 'nom_sv' TO BE MERGED WITH 'nom_sv' IN Ceni_cleaned.NK
  nom_sv_clean <- gsub(ep, "ecole primaire ", GSM_cleaned[["nom_sv"]]) %>% 
    gsub(cs, "complexe scolaire ", .) %>%
    gsub(inst, "institut ", . ) %>%
    gsub("ps ", "ecole primaire et secondaire ", .) %>%
    gsub(extra_prefixes_nom, "", .)
  
  # VARIABLE ORDER METTERS: CONSISTANT WITH c("clcr", "adresse_merge", "nom_sv")
  list(clcr, adresse_merge, nom_sv_clean) } ]
# -------------------------------------------------------------------------- #
# Note: 1) Merge GSM and Ceni by:"province","clcr","adresse_merge"and"nom_sv_clean";
#       2) 
# -------------------------------------------------------------------------- #
# --- merge GSM_cleaned.NK and Ceni_cleaned.NK to create Ceni_GSM_merge
Ceni_GSM_merge <- GSM_cleaned.NK %>% 
  left_join(Ceni_cleaned.NK, by = c("province", "clcr", "adresse_merge", "nom_sv_clean") ) %>% 
  mutate_if(is.character, str_trim)

merged_df <- Ceni_GSM_merge
# -------------------------------------------------------------------------- #



# ########################################################################## #
# ----------------- PART 2: GEO-MERGE POLL AND COORDINATES -----------------
# ########################################################################## #
#
# THIS SECTION: 
#
# -------------------------------------------------------------------------- #
# --- Read education facility data
ed_fac <- read.csv(get.path(dir.geo,"01. Education Facilities/osm_rd_congo_education_cl.csv") ) %>%
  data.table %>%
  mutate_if(is.character, str_to_lower)


# --- Read admin boundaries shapefile
shape.prov <- readOGR(dsn = get.path(dir.raw, "04. Admin Boundaries"), layer = "COD_adm1_2015")
shape.terr <- readOGR(dsn = get.path(dir.raw, "04. Admin Boundaries"), layer = "COD_adm2_2017")

# --- Read settlement shapefile
shape.set_p <- readOGR(dsn = get.path(dir.geo, "02. Settlements"), layer = "locality_test")
# -------------------------------------------------------------------------- #


# -------------------------------------------------------------------------- #
# --- Merge ed_fac, shape.terr, and shape.prov spatially
# PREP
df.geo <- ed_fac
coordinates(df.geo) <- ~lon+lat
proj4string(df.geo) <- proj4string(shape.terr)
# -------------------------------------------------------------------------- #
# MERGE ed_fac, shape.terr, and shape.prov SPATAILLY
ed_fac_mer <- ed_fac  %>%
  
  .[, c("name", "NOM", "ADMIN1_NAM") := {
    
    # THE PREVIOUS STRINGS DEFINITION SEEMS NOT WORK VERY WELL
    name = gsub(paste(unlist(c(".*\\cole", ".*\\cole/")), collapse = "|"), "ecole", ed_fac[["name"]] ) %>%
      gsub("^[^lyc]*lyc\\s*([^e]+)e*","lycee", .) %>%
      gsub("institutitut", "institut", .) %>%
      gsub(paste(unlist(c("e\\. p\\. ", "ep ", "ep2 ", "e\\.p ", "ep1 ", "ep5 ", "ep\\. ", "ep\\.", "e\\.p\\.", 
                          "epa1 ", "epa ", "ep7 ", "ep4 ", "ep3 ")), collapse = "|"), "ecole primaire ", .) %>%
      gsub(paste(unlist(c("c s ", "c\\.s ", "cs ", "cs\\. ", "complexe scolaire lecole primaire ", 
                          "complexe scolairecole primaire ", "c\\.s\\. ")), collapse = "|"), "complexe scolaire ", .)
    
    NOM = c(over(df.geo, shape.terr[, "NOM" ] )[["NOM" ]] )
    
    ADMIN1_NAM = c(over(df.geo, shape.prov[, "ADMIN1_NAM" ] )[["ADMIN1_NAM" ]] )
    
    list(name, NOM, ADMIN1_NAM) } ] %>%

  mutate_if(is.character, str_to_lower) %>%
  select(c("name", "lat", "lon", "ADMIN1_NAM", "ADMIN1_NAM") )
# -------------------------------------------------------------------------- #


# -------------------------------------------------------------------------- #
# --- Merge shape.set_p, shape.terr, and shape.prov spatially
# PREP
sett_data_p <- shape.set_p@data %>%
  cbind(shape.set_p@coords) %>%
  rename( c("lon" = "coords.x1",
           "lat" = "coords.x2") ) %>% 
  data.table

# KEEP THE DATA TABLE
df_sett_data_p <- sett_data_p

# SET SPATIAL ATTRIBUTE
coordinates(sett_data_p) <- ~lon+lat
proj4string(sett_data_p) <- proj4string(shape.terr)
# -------------------------------------------------------------------------- #
# MERGE shape.set_p, shape.terr, and shape.prov SPATAILLY
sett_data_p <- df_sett_data_p %>% 
  
  .[, c("NOM", "ADMIN1_NAM") := {
    
    NOM = c(over(sett_data_p, shape.terr[, "NOM" ] )[["NOM" ]] )
    
    ADMIN1_NAM = c(over(sett_data_p, shape.prov[, "ADMIN1_NAM" ] )[["ADMIN1_NAM" ]] )
    
    list(NOM, ADMIN1_NAM ) } ] %>%
  
  mutate(across(everything(), tolower) ) %>%
  select("NOM1",	"NOM2","TERRITOIRE", "COLLECTIV","GROUPEMENT",
         "Province","lon","lat", "ADMIN1_NAM", "NOM")
# -------------------------------------------------------------------------- #    
         
 
