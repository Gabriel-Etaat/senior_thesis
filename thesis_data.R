# File:      thesis_data.R
# Author:    Gabriel Etaat
# Created:   12 January 2020
# Last Edit: 20 April 2020
# Purpose:   Upload all databases to be sorted into a master df for analysis

# ------------------------- #
#                           #
#           Setup           #
#                           #
# ------------------------- #

# clear memory
rm(list=ls())

# directory
my_dir  <- "C:/Users/Etaat/Downloads/senior_thesis"
setwd(my_dir)

# libraries
library(AER)
library(plm)
library(tidyverse)
library(gtools)

# -------------------------- #
#                            #
#       Download Data        #
#                            #
# -------------------------- #

# ----- Data ----- #

fire_df <- read.csv("fire_data.csv", stringsAsFactors = F)            
#fire data, to be expanded

permit_df <- read.csv("county_permits.csv")     
#SOCDS data, copied onto an excel sheet

inc_df <- read.csv("bea_medinc.csv", skip = 4, stringsAsFactors = F)
#BEA data, personal inc/pop/per capita personaml inc

house_df <- read.csv("housing_units_csac.csv", stringsAsFactors = F)  
#CSAC housing unit totals by single family/multi family, units column changed to # in excel

# ----- Vectors ----- #

house_name <- c("county", "year", "exist_permit_type", "exist_units")

permit_name <- c("county", "year", "total_mf_sf_perm", "total_sf_perm", 
                 "total_mf_perm", "mf_2u_perm", "mf_34u_perm", "mf_5u_perm")

fact_chn <- sapply(permit_df, is.factor)

inc_name <- c("county_code", "county", "year", "county_inc", 
              "population", "per_cap_inc")

com_var <- c("county", "year")

replace_county_name <- c("Alameda", "Alpine", "Amador", "Butte", "Calaveras", 
                         "California", "Colusa", "Contra Costa", "Del Norte", 
                         "El Dorado", "Fresno", "Glenn", "Humboldt", "Imperial",
                         "Inyo", "Kern", "Kings", "Lake", "Lassen", "Los Angeles",
                         "Madera", "Marin", "Mariposa", "Mendocino", "Merced", 
                         "Modoc", "Mono", "Monterey", "Napa", "Nevada", "Orange",
                         "Placer", "Plumas", "Riverside", "Sacramento", 
                         "San Benito", "San Bernardino", "San Diego", 
                         "San Francisco", "San Joaquin", "San Luis Obispo",
                         "San Mateo", "Santa Barbara", "Santa Clara", "Santa Cruz",
                         "Shasta", "Sierra", "Siskiyou", "Solano", "Sonoma",
                         "Stanislaus", "Sutter", "Tehama", "Trinity", "Tulare",
                         "Tuolumne", "Ventura", "Yolo", "Yuba")

replace_county_code <- c("6001", "6003", "6005", "6007", "6009", "6000", "6011", 
                         "6013", "6015", "6017", "6019", "6021", "6023", "6025", 
                         "6027", "6029", "6031", "6033", "6035", "6037", "6039", 
                         "6041", "6043", "6045", "6047", "6049", "6051", "6053", 
                         "6055", "6057", "6059", "6061", "6063", "6065", "6067", 
                         "6069", "6071", "6073", "6075", "6077", "6079", "6081", 
                         "6083", "6085", "6087", "6089", "6091", "6093", "6095", 
                         "6097", "6099", "6101", "6103", "6105", "6107", "6109", 
                         "6111", "6113", "6115")
#GeoCodes from BEA data, a California Research Bureau standard

# ----- Formatting fire DF ----- #

fire_df$county <- gsub("^([0-9]{4})", "0\\1", fire_df$county)

# ----- Formatting permit DF ----- #

permit_df[fact_chn] <- lapply(permit_df[fact_chn], as.character) 
#change factor-format variables to character

permit_df <- reshape(permit_df, 
                     varying = list(3:23), 
                     v.names="permits_adm", 
                     direction="long"
)
#reshape data to long format, with years differentiated by row and # of permits by year/county/type all in one column

permit_df$id <- NULL

colnames(permit_df)[3] <- "year"
#uniformed variable names for df merging

permit_df <- permit_df %>% 
  pivot_wider(
    names_from=permit_type, 
    values_from=permits_adm
  )
#Create distinct columns for each permit type

permit_df[,2] <- permit_df[,2] + 1997
#changing time stamps to match corresponding year, to be restructured?

colnames(permit_df) <- permit_name
#column names match for df merging

permit_df <- permit_df %>%
  arrange(year, county)

permit_df$county_code <- str_replace_all(
  permit_df$county, 
  replace_county_name, 
  replace_county_code
)
#changing county names to codes after sorting to match string pattern

permit_df$county_code <- strtoi(permit_df$county_code)

# ----- Formatting income DF ----- #

inc_df <- inc_df[-c(178:183), ]

inc_df <- reshape(inc_df, 
                  varying = list(5:25), 
                  v.names="quantity", 
                  direction="long"
)
#Long format for years as columns

inc_df[7] <- NULL

inc_df <- inc_df %>% 
  pivot_wider(
    names_from=LineCode, 
    values_from=quantity
  )
#Create unique columns for county income, populations, and per captia income

inc_df[5] <- inc_df[5] *1000
#Make county income from "in thousands of $" to the actual number

inc_df[3] <- NULL
#Excess data

inc_df <- inc_df %>% 
  group_by(GeoFips,GeoName, time) %>% 
  summarise_each(list(~sum(., na.rm = T))
  )
#group rows together instead of distinct entries for the 3 new variables

inc_df[,3] <- inc_df[,3] + 1997
#format yeara column back into actual years

inc_df$GeoName <- gsub(", CA", "", inc_df$GeoName) 
#formatting for df merge

inc_df$GeoFips <- gsub("^0", "", inc_df$GeoFips)
#formatting county codes to be uniform

my_fun <- function(x) x [!is.na(x)]

inc_df <- inc_df %>% 
  group_by(time) %>% 
  summarise_all(list(~my_fun)
  )

colnames(inc_df) <- inc_name
#naming columns for final merge

inc_df$county_code <- strtoi(inc_df$county_code)

# ----- Formatting house DF ----- #

colnames(house_df) <- house_name
#uniformed variable names for df merging

house_df <- house_df[!(house_df$exist_permit_type=="Mobile Homes"),]
#excluding mobile homes from our observation, no permit data on them (or is there? look into metadata from SOCDS)

house_df$exist_permit_type[house_df$exist_permit_type=="Single Family"] <- "total_sf_exist"

house_df$exist_permit_type[house_df$exist_permit_type=="Multi-Family"] <- "total_mf_exist"
#uniformed category names for df merging

house_df <- house_df %>% 
  pivot_wider(
    names_from=exist_permit_type, 
    values_from=exist_units
  )
#widening data for formating/creating new column of totals
###why do I get "Unknown or uninitialised column: 'permit_type'" warnings after this? isnt that column removed??

house_df <- mutate(house_df, 
                   total_sf_mf_exist = total_sf_exist + total_mf_exist
)
#Trailing spaces in county variable, delete spaces

cali_house <- house_df %>% 
  group_by(year) %>%
  mutate(total_sf_exist = sum(total_sf_exist),
         total_mf_exist = sum(total_mf_exist),
         total_sf_mf_exist = sum(total_sf_mf_exist),
         county = replace(county, is.character(county), "California")) %>%
  distinct()
#create data frame with California totals as county type

house_df <- bind_rows(house_df, cali_house)
#combine california observations to original dataframe

house_diff <- setdiff(house_df$year, permit_df$year)
#identifying excess years 1991-1997 by comparing to-be merged dataframes

house_df <- house_df[!house_df$year %in% house_diff,]
#deleting excess years

house_df$county <- gsub(" $", "", house_df$county)

house_df <- house_df %>%
  arrange(year, county)

house_df$county_code <- str_replace_all(
  house_df$county, 
  replace_county_name, 
  replace_county_code
)

house_df$county_code <- strtoi(house_df$county_code)

# ----- Merging fire & permits ----- #

master_df <- merge(permit_df, house_df, all=T)

master_df <- merge(master_df, inc_df, all=T)

master_df <- merge(master_df, fire_df, 
                   by = c("county", "year", "county_code"),
                   all = T
             )
#merging all dataframes into a master, cumulative dataframe by year and county. a few duplicated
#entries of year and county exist for 5 observations. 2017 Napa makes sense, the others ?

master_df$year <- factor(master_df$year)
master_df$county <- factor(master_df$county)
#this is new, may be affecting other results, but needed for plm
master_df <- master_df %>%
  arrange(county, year)%>%
  group_by(county) %>%
  mutate(
    permit_gr = (total_mf_sf_perm - lag(total_mf_sf_perm))/lag(total_mf_sf_perm)*100,
    permit_var = (total_mf_sf_perm - mean(total_mf_sf_perm))/mean(total_mf_sf_perm)*100,
    exist_gr = (total_sf_mf_exist - lag(total_sf_mf_exist))/lag(total_sf_mf_exist)*100,
    pop_gr = (population - lag(population))/lag(population)*100,
    prop_perm_exist = (total_mf_sf_perm/total_sf_mf_exist)*100,
    year_b4 = dplyr::lead(struc_dest),
    year_0 = struc_dest,
    year_1 = dplyr::lag(struc_dest),
    year_2 = dplyr::lag(struc_dest, n = 2),
    year_3 = dplyr::lag(struc_dest, n = 3),
    year_4 = dplyr::lag(struc_dest, n = 4),
    year_5 = dplyr::lag(struc_dest, n = 5),
    year_6 = dplyr::lag(struc_dest, n = 6),
    year_7 = dplyr::lag(struc_dest, n = 7),
  ) %>%
  mutate(
    year_b4 = ifelse(is.na(year_b4), 0, 1),
    year_0 = ifelse(is.na(year_0), 0, 1),
    year_1 = ifelse(is.na(year_1), 0, 1),
    year_2 = ifelse(is.na(year_2), 0, 1),
    year_3 = ifelse(is.na(year_3), 0, 1),
    year_4 = ifelse(is.na(year_4), 0, 1),
    year_5 = ifelse(is.na(year_5), 0, 1),
    year_6 = ifelse(is.na(year_6), 0, 1),
    year_7 = ifelse(is.na(year_7), 0, 1),
    struc_dest = ifelse(is.na(struc_dest), 0, struc_dest)
  )
#growth rates of total permits administered, existing housing, and population by county. Also 0/1
#indicators variables for year as it relates to a fire in a given county, and NA's changed to 0's
#in struc_dest to accomodate plm operations

write.csv(master_df, 'master_df.csv')
