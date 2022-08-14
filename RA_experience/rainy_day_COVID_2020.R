# --------------------------------------------------------------------------------------
#
#  Jin Yao, 2020/05
#
# --------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------

library(magrittr)
library(readxl)
library(haven)
library(dplyr)
library(glue)
library(data.table)

# --------------------------------------------------------------------------------------
# Part one: Clean the data
# --------------------------------------------------------------------------------------
# Read all the excel files in a folder, you may need to set your working folder to the 
# folder that contain only local data
setwd("./input/ASPEP/local")
filenames=list.files("./") # make sure that the folder only contain the local data xls file, I attached my file in the email
local<-c(1993,1994,1995,1997,1998,1999,2000,2001,2002,
         2004,2005,2006,2007,2008,2009,2010,2011,
         2012,2013,2014,2015,2016,2017,2018)
# There are 5 kinds of column names
col_1993_2006_wo_97 <- 
  c("state", "item_des","emp_full_time_equivalent", "emp_full_time_equivalent_coeffvar", "pay_total_march","pay_total_march_coeffvar",
    "emp_full_time", "emp_full_time_coeffvar","emp_part_time","emp_part_time_coeffvar","payroll_full_time","payroll_full_time_coeffvar",
    "payroll_part_time","payroll_part_time_coeffvar","hours_part_time","hours_part_time_coeffvar")
col_1997 <- 
  c("state", "item_des","emp_full_time_equivalent", "pay_total_march",
    "emp_full_time","emp_part_time","payroll_full_time",
    "payroll_part_time","hours_part_time")
col_2009_2018_wo_17_12_11 <- 
  c("state", "item_des", "emp_full_time", "emp_full_time_coeffvar", "payroll_full_time", "payroll_full_time_coeffvar",
    "emp_part_time", "emp_part_time_coeffvar",  "payroll_part_time", "payroll_part_time_coeffvar", 
    "hours_part_time", "hours_part_time_coeffvar", "emp_full_time_equivalent", "emp_full_time_equivalent_coeffvar", 
    "emp_total_march", "emp_total_march_coeffvar", "pay_total_march", "pay_total_march_coeffvar")
col_2007_2008_2011 <-  
  c("state", "item_des", "emp_full_time", "emp_full_time_coeffvar", "payroll_full_time", "payroll_full_time_coeffvar",
    "emp_part_time", "emp_part_time_coeffvar",  "payroll_part_time", "payroll_part_time_coeffvar", 
    "hours_part_time", "hours_part_time_coeffvar", "emp_full_time_equivalent", "emp_full_time_equivalent_coeffvar", 
    "emp_total_march", "emp_total_march_coeffvar", "pay_total_march", "pay_total_march_coeffvar","emp_full_time_equivalent_rep_rate","pay_total_march_rep_rate")
col_2012 <- 
  c("state", "item_des", "emp_full_time",  "payroll_full_time",
    "emp_part_time",  "payroll_part_time", 
    "hours_part_time", "emp_full_time_equivalent", 
    "emp_total_march", "pay_total_march")
col_2017 <- 
  c("state", "item_des", "emp_full_time",  "payroll_full_time",
    "emp_part_time",  "payroll_part_time", 
    "hours_part_time", "emp_full_time_equivalent", 
    "emp_total_march", "pay_total_march")
# --------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------

# The loop of cleaning each data'frame and save the length of each table to a vector

year_list <- c(seq(1993, 1995), seq(1997, 2002), seq(2004, 2018))
length_every_year <- rep(NA,length(filenames)) # keep track of the number of rows of each year for later use
for (i in 1:length(filenames)) {
  original_data <- read_excel(glue(filenames[i])) %>% data.table # Read the original data of local only
  setnames(original_data, c(3), c("emp_full_time"))
  cleaned_data <- original_data[ !is.na(as.numeric(emp_full_time)) ] # Clean data(delete first several lines of blank)
  if (local[i] %in% c(seq(1993, 1995), seq(1998, 2006))){
    setnames(cleaned_data, col_1993_2006_wo_97)    
  } else if (local[i] == 1997){
    setnames(cleaned_data, col_1997)
  } else if (local[i] %in% c(2007, 2008, 2011)){
    setnames(cleaned_data, col_2007_2008_2011)
  } else if (local[i] %in% c(2012)){
    setnames(cleaned_data, col_2012)
  } else if (local[i] %in% c(2009, 2010, 2013, 2014, 2015, 2016, 2018)){
    setnames(cleaned_data, col_2009_2018_wo_17_12_11)
  } else if (local[i] %in% c(2017)){
    setnames(cleaned_data, col_2017)   
  }
  assign(paste0("local_", local[i]), 
         cleaned_data, envir = .GlobalEnv) # Assign the cleaned data to the the data frame of "local_year****"
  length_every_year[i] <- dim(cleaned_data)[1]
} 

# --------------------------------------------------------------------------------------

dt_agg_payroll <- rbindlist(
  list(local_1993, local_1994, local_1995, local_1997, local_1998,
       local_1999, local_2000, local_2001, local_2002, local_2004, local_2005,
       local_2006, local_2007, local_2008, local_2009, local_2010, local_2011, 
       local_2012, local_2013, local_2014, local_2015, local_2016, local_2017,
       local_2018), 
  fill = TRUE)

col_num <- colnames(dt_agg_payroll)[3:length(colnames(dt_agg_payroll))]
dt_agg_payroll[, c(col_num) := lapply(.SD, as.numeric), .SDcols = c(col_num) ]

# --------------------------------------------------------------------------------------
# Part two: rename the old function names
# --------------------------------------------------------------------------------------

# Select all the variables we need

dt_agg_payroll <- dt_agg_payroll %>% 
  select(state,item_des ,emp_full_time,payroll_full_time,emp_part_time,payroll_part_time,hours_part_time,emp_full_time_equivalent)

# -------------------------------------------------- Attach the years -------------------------------------------------------

year_local <- rep(c(1993,1994,1995,1997,1998,1999,2000,2001,2002,2004,2005,2006,2007,2008,2009,2010,2011,2012,
                    2013,2014,2015,2016,2017,2018),c(length_every_year)) # length_every_year is from the loop
dt_agg_payroll <- mutate(dt_agg_payroll, year = year_local)

# --------------------------------------------- Rename the function item names -----------------------------------------------

# Keep the old names of functions 

dt_agg_payroll <- mutate(dt_agg_payroll, item_names_old = dt_agg_payroll$item_des)
dt_agg_payroll$item_des <- as.factor(dt_agg_payroll$item_des)

# Change the names of funcitons that have the inconsistent names(long codes here) 
# Three old function names are not found which marks as: Not Found in last three lines

levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Air transportation"] <- "Air Transportation"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Total"] <- "Total - All Government Employment Functions"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Correction"] <- "Corrections"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Electric power"] <- "Electric Power"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elem & Sec - Other"] <- "Education - Elementary and Secondary Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elem & Sec Instructional"] <- "Education - Elementary and Secondary Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Financial administration"] <- "Financial Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Fire - other"] <- "Fire Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Firefighters"] <- "Fire Protection - Firefighters"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Gas supply"] <- "Gas Supply"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Higher Ed - Other"] <- "Education - Higher Education Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Higher Ed Instructional"] <- "Education - Higher Education Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Housing and community developmen"] <- "Housing and Community Development"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Natural resources"] <- "Natural Resources"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Other government administration"] <- "Other Government Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Parks and recreation"] <- "Parks and Recreation"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Police - other"] <- "Police Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Police with power of arrest"] <- "Police Protection - Persons with Power of Arrest"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Solid waste management"] <- "Solid Waste Management"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Water supply"] <- "Water Supply"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Water transport and canals"] <- "Water Transport and Terminals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elem & Sec  - Other"] <- "Education - Elementary and Secondary Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Housing & Community Development"] <- "Housing and Community Development"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Liquor Stores"] <- "State liquor stores"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Other and Unallocable"] <- "All other and unallocable"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Other Education"] <- "Education - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Persons with power of arrest"] <- "Police Protection - Persons with Power of Arrest"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Police - Other"] <- "Police Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Social insurance administration"] <- "Social Insurance Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elem & Secondary - Other"] <- "Education - Elementary and Secondary Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Housing & Community Development"] <- "Housing and Community Development"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Liquor Stores"] <- "State liquor stores"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Other Police Employees"] <- "Police Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Other Fire Employees"] <- "Fire Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Local Libraries"] <- "Libraries"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Higher Education Total"] <- "Education - Higher Education Total"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Higher Ed Other Employees"] <- "Education - Higher Education Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Higher Ed Instructional Employees"] <- "Education - Higher Education Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Firefighters Only"] <- "Fire Protection - Firefighters"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elementary & Secondary Education Total"] <- "Education - Elementary and Secondary Total"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elem & Sec Other Employees"] <- "Education - Elementary and Secondary Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elem & Sec Instructional Employees"] <- "Education - Elementary and Secondary Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="All Other and Unallocable"] <- "All other and unallocable"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Fire - Other"] <- "Fire Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Water Transport & Terminals"] <- "Water Transport and Terminals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Water Transport & Terminals"] <- "Water Transport and Terminals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Police Officers Only"] <- "Police Protection - Persons with Power of Arrest"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Police Protection - Officers"] <- "Police Protection - Persons with Power of Arrest"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Other education"] <- "Education - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Correction"] <- "Corrections"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elem & Sec School Instruc."] <- "Education - Elementary and Secondary Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elem & Sec School-Other"] <- "Education - Elementary and Secondary Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Financial Admin."] <- "Financial Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Fire-Other"] <- "Fire Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Firefighters"] <- "Fire Protection - Firefighters"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Higher Education-Instruc."] <- "Education - Higher Education Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Higher Education-Other"] <- "Education - Higher Education Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Housing & Community Dev."] <- "Housing and Community Development"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Other & Unallocable"] <- "All other and unallocable"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Other Government Admin."] <- "Other Government Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Parks & Recreation"] <- "Parks and Recreation"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Police Protection-Officers"] <- "Police Protection - Persons with Power of Arrest"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Police Protection-Other"] <- "Police Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Streets & Highways"] <- "Highways"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Total"] <- "Total - All Government Employment Functions"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Water Transport/Terminals"] <- "Water Transport and Terminals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="AIRPORTS . . . . . ."] <- "Air Transportation"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="CORRECTION . . . . ."] <- "Corrections"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="ELEM&SEC INSTRUCT. ."] <- "Education - Elementary and Secondary Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="FINANCIAL ADMIN. . ."] <- "Financial Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="FIREFIGHTERS . . . ."] <- "Fire Protection - Firefighters"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HEALTH . . . . . . ."] <- "Health"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HIGHER ED OTHER. . ."] <- "Education - Higher Education Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HOUSING & COMM DEV ."] <- "Housing and Community Development"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="LIBRARIES. . . . . ."] <- "Libraries"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="OTHER & UNALLOCABLE."] <- "All other and unallocable"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="PARKS & RECREATION ."] <- "Parks and Recreation"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="POLICE-OTHER . . . ."] <- "Police Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="SEWERAGE . . . . . ."] <- "Sewerage"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="SOLID WASTE MGMT . ."] <- "Solid Waste Management"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="STREETS & HWYS . . ."] <- "Highways"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="TRANSIT. . . . . . ."] <- "Transit"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="WATER TRANSPORT. . ."] <- "Water Transport and Terminals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="LOCAL LIBRARIES. . ."] <- "Libraries"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="ELECTRIC POWER . . ."] <- "Electric Power"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="ELEM&SEC OTHER-TOT ."] <- "Education - Elementary and Secondary Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="FIRE-OTHER . . . . ."] <- "Fire Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="GAS SUPPLY . . . . ."] <- "Gas Supply"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HIGHER ED INSTRUCT ."] <- "Education - Higher Education Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HOSPITALS  . . . . ."] <- "Hospitals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="JUDICIAL-LEGAL . . ."] <- "Judicial and Legal"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="NATURAL RESOURCES. ."] <- "Natural Resources"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="OTHER EDUCATION. . ."] <- "Education - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="POLICE-ARREST. . . ."] <- "Police Protection - Persons with Power of Arrest"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="PUBLIC WELFARE . . ."] <- "Public Welfare"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="SOC INSUR ADMIN. . ."] <- "Social Insurance Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="STATE LIQUOR STORES."] <- "State liquor stores"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="TOTAL. . . . . . . ."] <- "Total - All Government Employment Functions"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="WATER SUPPLY . . . ."] <- "Water Supply"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HOSPITALS. . . . . ."] <- "Hospitals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="AIRPORTS"] <- "Air Transportation"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="CORRECTION"] <- "Corrections"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="ELEM&SEC INSTRUCT"] <- "Education - Elementary and Secondary Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="FINANCIAL ADMIN. . ."] <- "Financial Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="FIREFIGHTERS . . . ."] <- "Fire Protection - Firefighters"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HEALTH . . . . . . ."] <- "Health"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HIGHER ED OTHER. . ."] <- "Education - Higher Education Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HOUSING & COMM DEV ."] <- "Housing and Community Development"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="LIBRARIES. . . . . ."] <- "Libraries"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="OTHER & UNALLOCABLE."] <- "All other and unallocable"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="PARKS & RECREATION ."] <- "Parks and Recreation"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="POLICE-OTHER . . . ."] <- "Police Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="SEWERAGE . . . . . ."] <- "Sewerage"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="SOLID WASTE MGMT . ."] <- "Solid Waste Management"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="STREETS & HWYS . . ."] <- "Highways"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="TRANSIT. . . . . . ."] <- "Transit"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="WATER TRANSPORT. . ."] <- "Water Transport and Terminals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="LOCAL LIBRARIES. . ."] <- "Libraries"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="ELECTRIC POWER . . ."] <- "Electric Power"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="ELEM&SEC OTHER-TOT ."] <- "Education - Elementary and Secondary Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="FIRE-OTHER . . . . ."] <- "Fire Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="GAS SUPPLY . . . . ."] <- "Gas Supply"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HIGHER ED INSTRUCT ."] <- "Education - Higher Education Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HOSPITALS  . . . . ."] <- "Hospitals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="JUDICIAL-LEGAL . . ."] <- "Judicial and Legal"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="NATURAL RESOURCES. ."] <- "Natural Resources"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="OTHER EDUCATION. . ."] <- "Education - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="POLICE-ARREST. . . ."] <- "Police Protection - Persons with Power of Arrest"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="PUBLIC WELFARE . . ."] <- "Public Welfare"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="SOC INSUR ADMIN. . ."] <- "Social Insurance Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="STATE LIQUOR STORES."] <- "State liquor stores"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="TOTAL. . . . . . . ."] <- "Total - All Government Employment Functions"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="WATER SUPPLY . . . ."] <- "Water Supply"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HOSPITALS. . . . . ."] <- "Hospitals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="OTHER ED -STATE. . ."] <- "Education - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="ELEM&SEC OTHER-TOT"] <- "Education - Elementary and Secondary Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="FIREFIGHTERS"] <- "Fire Protection - Firefighters"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HEALTH"] <- "Health"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HIGHER ED OTHER"] <- "Education - Higher Education Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HOUSING & COMM DEV"] <- "Housing and Community Development"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="LOCAL LIBRARIES"] <- "Libraries"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="OTHER"] <- "All other and unallocable"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="OTHER ED - STATE"] <- "Education - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="POLICE-ARREST"] <- "Police Protection - Persons with Power of Arrest"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="PUBLIC WELFARE"] <- "Public Welfare"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="SOC INSUR ADMIN"] <- "Social Insurance Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="STATE LIQUOR STORES"] <- "State liquor stores"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="TOTAL"] <- "Total - All Government Employment Functions"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="WATER SUPPLY"] <- "Water Supply"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="ELECTRIC POWER"] <- "Electric Power"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="FINANCIAL ADMIN"] <- "Financial Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="GAS SUPPLY"] <- "Gas Supply"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HIGHER ED INSTRUCT"] <- "Education - Higher Education Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="HOSPITALS"] <- "Hospitals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="JUDICIAL-LEGAL"] <- "Judicial and Legal"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="NATURAL RESOURCES"] <- "Natural Resources"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="OTHER & UNALLOCABLE"] <- "All other and unallocable"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="PARKS & RECREATION"] <- "Parks and Recreation"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="POLICE-OTHER"] <- "Police Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="SEWERAGE"] <- "Sewerage"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="SOLID WASTE MGMT"] <- "Solid Waste Management"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="STREETS & HWYS"] <- "Highways"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="TRANSIT"] <- "Transit"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="WATER TRANSPORT"] <- "Water Transport and Terminals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Correction"] <- "Corrections"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elem & Sec School Instruction"] <- "Education - Elementary and Secondary Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elem & Sec School-Other"] <- "Education - Elementary and Secondary Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Fire-Other"] <- "Fire Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Firefighters"] <- "Fire Protection - Firefighters"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Higher Education-Instruction"] <- "Education - Higher Education Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Higher Education-Other"] <- "Education - Higher Education Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Housing & Community Development"] <- "Housing and Community Development"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Other & Unallocable"] <- "All other and unallocable"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Parks & Recreation"] <- "Parks and Recreation"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Police Protection-Officers"] <- "Police Protection - Persons with Power of Arrest"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Police Protection-Other"] <- "Police Protection - Other"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Streets & Highways"] <- "Highways"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Total"] <- "Total - All Government Employment Functions"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Water Transport & Terminals"] <- "Water Transport and Terminals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Elem & Sec School Instruc."] <- "Education - Elementary and Secondary Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Financial Admin."] <- "Financial Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Higher Education-Instruc."] <- "Education - Higher Education Instructional"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Housing & Community Dev."] <- "Housing and Community Development"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Other Government Admin."] <- "Other Government Administration"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="Water Transport/Terminals"] <- "Water Transport and Terminals"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="State Liquor Stores"] <- "State liquor stores"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="CENTRAL ADMIN"] <- "Not Found"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="OTHER ED -STATE. . ."] <- "Not Found"
levels(dt_agg_payroll$item_des)[levels(dt_agg_payroll$item_des)=="CENTRAL ADMIN. . . ."] <- "Not Found"

# --------------------------------------------------------------------------------------
# Part three: attach the function item codes
# --------------------------------------------------------------------------------------

setwd("./input/ASPEP")
original_item_code <- read.csv("item_code.csv")
original_item_code$Description <- as.character(original_item_code$Description)
# here 1010101010 represents the old function names that is hard to find a close name
original_item_code[nrow(original_item_code) + 1,] = c(NA,"Not Found")
dt_agg_payroll$item_des <- as.factor(dt_agg_payroll$item_des)
original_item_code$Description <- as.character(original_item_code$Description)
# You have to keep the type of the variable as character when you use arrange to order the name of functions
original_item_code <- arrange(original_item_code,Description)

# Count for frequency of each function name

frequency_item <- data.frame(summary(dt_agg_payroll$item_des))
setDT(frequency_item, keep.rownames = TRUE)[]
frequency_item$rn <- as.character(frequency_item$rn)
frequency_item <- arrange(frequency_item, rn)

# Arrange the data according to the character of the item functions

dt_agg_payroll$item_des <- as.character(dt_agg_payroll$item_des)
dt_agg_payroll <- arrange(dt_agg_payroll, item_des)

# Combine the item code with frequency of the items

frequency_all_item <- cbind(original_item_code,frequency_item)
setnames(frequency_all_item,c(4),"freq")
frequency_all_item <- cbind(original_item_code,frequency_all_item)


# Replicate the item code by the frequency of the each item

frequency_all_item <- data.frame(frequency_all_item[rep(row.names(frequency_all_item), frequency_all_item$freq), 1])
setnames(frequency_all_item,"item_code")

# Add the replicated item to the original data

dt_agg_payroll_item <- cbind(dt_agg_payroll,frequency_all_item)


# --------------------------------------------------------------------------------------
# Part four: attach the state item codes
# --------------------------------------------------------------------------------------

state_code <- read.csv("./state_code.csv")
dt_agg_payroll_item$state <- as.factor(dt_agg_payroll_item$state)

# Count for frequency of each state by item

frequency_state <- data.frame(summary(dt_agg_payroll_item$state))

# Arrange the data according to state

dt_agg_payroll_item$state <- as.character(dt_agg_payroll_item$state)
state_code$Abbreviation <- as.character(state_code$Abbreviation)
dt_agg_payroll_state <- arrange(dt_agg_payroll_item,state)
state_code <- arrange(state_code,Abbreviation)

# Combine the state code with frequency

frequency_state <- cbind(state_code,frequency_state)
setnames(frequency_state,c(4),"freq")
frequency_state <- cbind(state_code,frequency_state)

# Replicate the state code by the frequency of the each state

frequency_state_replicate <- data.frame(frequency_state[rep(row.names(frequency_state), frequency_state$freq), 1])
setnames(frequency_state_replicate,"state_code")

# Add the replicated state to the original data

dt_agg_payroll_state <- cbind(dt_agg_payroll_state,frequency_state_replicate)

# Add a new variable: month to indicate the time of collection of the data(M: collected in March; O: collected in October)

month <- rep("M", dim(dt_agg_payroll_state)[1])
all_dt_agg_payroll <- mutate(dt_agg_payroll_state, month=month)
all_dt_agg_payroll$month[(all_dt_agg_payroll$year == 1993)] <- "O"
all_dt_agg_payroll$month[(all_dt_agg_payroll$year == 1994)] <- "O"
all_dt_agg_payroll$month[(all_dt_agg_payroll$year == 1995)] <- "O"

ASPEP_local_payroll_aggregates_1993_2018 <- select(all_dt_agg_payroll,state,state_code,year,item_names_old,item_des,
                                                   item_code,month,emp_full_time,payroll_full_time,emp_part_time,
                                                   payroll_part_time,hours_part_time,emp_full_time_equivalent) 

ASPEP_local_payroll_aggregates_1993_2018 <-  arrange(all_dt_agg_payroll,as.character(state),as.character(item_des),as.numeric(year))


# --------------------------------------------------------------------------------------
# SAVE THE DATA 
write_dta(ASPEP_local_payroll_aggregates_1993_2018, "./output/ASPEP_local_payroll_aggregates_1993_2018.dta")
# --------------------------------------------------------------------------------------
#
#
#
#
#
#
#
message("Log file for code executed at\n")
message(format(Sys.time(), "%a %b %d %X %Y"))
# --------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
library(crayon)
library(devtools)

library(tidyr); 
library(magrittr)
library(stringr);
library(glue);
library(purrr)
library(readr)
library(haven); 
library(readxl); 
library(lubridate)
library(ggplot2)
library(progress);
library(fst);
library(data.table); library(bit64)
library(statar)

library(skimr)
# --------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# APPEND REQUIRED PACKAGES
check_file = file.exists("log/R-session-info.log.R")
sink("log/R-session-info.log.R", append=check_file)
cat(bold("\n\n# -----\n# Session info for import_PublicUseEmp.r\n\n")) 
session_info()
sink()
# --------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# PREAMBLE
fips_state <- fread("./input/fips_state.csv")
setnames(fips_state, c("state_name", "fips", "state"))

state_census_codes <- read_excel("./input/census_state_codes.xlsx", skip=2) %>% data.table
setnames(state_census_codes, c("state_name", "state", "tmp", "state_code"))
state_census_codes[, tmp := NULL ]
state_census_codes <- merge(state_census_codes, fips_state[, .(fips, state)], by = c("state"))
state_census_codes[]
# --------------------------------------------------------------------------------------


# --------------------------------------------------------------------------------------
# 1.1 Individual Unit Data File (Public Use Format)
# This is an ASCII fixed length text file containing a standard 94-character Public Use Format layout. It contains values for all variables by Item Code (Functional Category) for each Individual Unit ID (see Section 1.2). All payroll and part-time hours in the data file are 31-day monthly equivalent values for the month of March. There may be multiple Item Codes (Functional Category) for the same Individual Unit ID (see Section 1.2). The first 14 characters comprise the Individual Unit ID, which is a concatenation of State Code, Unit Type Code, County Code, Unit Identification Number, Supplement Code, and Sub Code.
read_micro <- function(year){
  
  # `yycempst` for census years
  # `yyempst` for non census years

  message(glue("# Processing Census Year ... {year} ") )

  # year = 2006
  files <- list.files(glue("./input/ASPEP/{year}/"), pattern = glue("{str_sub(year, 3, 4)}.*empst") )
  files
  
  if ( (year < 1992) | (year %in% c(1996, 2003)) ){
    dt1 <- data.table()
    return(dt1)
  } else if (year >= 2007){
    dt1 <- read_fwf(glue("./input/ASPEP/{year}/{files[1]}"),
      fwf_cols(state_code = c(1, 2), type = c(3, 3), county = c(4, 6), unit_id = c(7, 9), 
               supplement = c(10, 12), sub_code = c(13, 14), item_code = c(18, 20), 
               emp_full_time = c(21, 30), emp_full_time_flag = c(32, 32),
               payroll_full_time = c(33, 44), payroll_full_time_flag = c(46, 46),
               emp_part_time = c(47, 56), emp_part_time_flag = c(58, 58),
               payroll_part_time = c(59, 70), payroll_part_time_flag = c(72, 72),
               hours_part_time = c(73, 82), hours_part_time_flag = c(84, 84),
               emp_full_time_equivalent = c(85, 94)) ) %>% data.table
  } else if (year <= 2006){
    dt1 <- read_fwf(glue("./input/ASPEP/{year}/{files[1]}"),
      fwf_cols(state_code = c(1, 2), type = c(3, 3), filler = c(4, 17),
               item_code = c(18, 20), 
               emp_full_time = c(21, 30), payroll_full_time = c(31, 42), 
               emp_part_time = c(43, 52), payroll_part_time = c(53, 64), 
               hours_part_time = c(65, 74), emp_full_time_equivalent = c(75, 84)) ) %>% data.table
  }

  # Tag census years
  dt1[, date_y := year ]
  dt1[, census_year := 0 ]
  if (year %in% c(1992, 1997, 2002, 2007, 2012)){ dt1[, census_year := 1 ]  }

  dt1[, state_code := as.integer(state_code) ]
  dt1 <- merge(dt1, state_census_codes, all.x = T, by = c("state_code"))
  return(dt1)
}

dt_micro <- data.table()
for (i_year in seq(1992, 2018)){
  dt_tmp <- read_micro(i_year)
  dt_micro <- rbind(dt_micro, dt_tmp, fill = T)
}
dt_micro[]
# --------------------------------------------------------------------------------------
