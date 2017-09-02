## Import Libraries ##

library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(DT)
library(tidyr)
library(corrplot)
library(leaflet)
library(lubridate)
library(mice)
library(stats)
library(flexclust)
library(ade4)

properties <- fread(input = "https://s3.us-east-2.amazonaws.com/aws-emr-dedicated/data/zillow/properties_2016.csv")
train <- fread(input = "https://s3.us-east-2.amazonaws.com/aws-emr-dedicated/data/zillow/train_2016_v2.csv")

## Renaming all columns for conveniance ##
properties <- properties %>% rename(
  id_parcel = parcelid,
  build_year = yearbuilt,
  area_basement = basementsqft,
  area_patio = yardbuildingsqft17,
  area_shed = yardbuildingsqft26, 
  area_pool = poolsizesum,  
  area_lot = lotsizesquarefeet, 
  area_garage = garagetotalsqft,
  area_firstfloor_finished = finishedfloor1squarefeet,
  area_total_calc = calculatedfinishedsquarefeet,
  area_base = finishedsquarefeet6,
  area_live_finished = finishedsquarefeet12,
  area_liveperi_finished = finishedsquarefeet13,
  area_total_finished = finishedsquarefeet15,  
  area_unknown = finishedsquarefeet50,
  num_unit = unitcnt, 
  num_story = numberofstories,  
  num_room = roomcnt,
  num_bathroom = bathroomcnt,
  num_bedroom = bedroomcnt,
  num_bathroom_calc = calculatedbathnbr,
  num_bath = fullbathcnt,  
  num_75_bath = threequarterbathnbr, 
  num_fireplace = fireplacecnt,
  num_pool = poolcnt,  
  num_garage = garagecarcnt,  
  region_county = regionidcounty,
  region_city = regionidcity,
  region_zip = regionidzip,
  region_neighbor = regionidneighborhood,  
  tax_total = taxvaluedollarcnt,
  tax_building = structuretaxvaluedollarcnt,
  tax_land = landtaxvaluedollarcnt,
  tax_property = taxamount,
  tax_year = assessmentyear,
  tax_delinquency = taxdelinquencyflag,
  tax_delinquency_year = taxdelinquencyyear,
  zoning_property = propertyzoningdesc,
  zoning_landuse = propertylandusetypeid,
  zoning_landuse_county = propertycountylandusecode,
  flag_fireplace = fireplaceflag, 
  flag_tub = hashottuborspa,
  quality = buildingqualitytypeid,
  framing = buildingclasstypeid,
  material = typeconstructiontypeid,
  deck = decktypeid,
  story = storytypeid,
  heating = heatingorsystemtypeid,
  aircon = airconditioningtypeid,
  architectural_style= architecturalstyletypeid
)

train <- train %>% rename(
  id_parcel = parcelid,
  date = transactiondate
)

prop = properties
clean_prop = data.table(id_parcel = prop$id_parcel)
imp_prop = data.table(id_parcel = prop$id_parcel)

##  create clean_prop with 

imp_prop$build_year_isna = is.na(prop$build_year)
clean_prop$build_year = ifelse(imp_prop$build_year_isna, -999, prop$build_year)

imp_prop$num_garage_isna = is.na(prop$num_garage)
clean_prop$num_garage = ifelse(is.na(prop$num_garage), -999, prop$num_garage)

imp_prop$area_garage_isna = is.na(prop$area_garage)
clean_prop$area_garage = ifelse(is.na(prop$area_garage), -999, prop$area_garage)

avg_garage_area = (properties %>% 
                     filter(num_garage>0 & area_garage>0) %>% 
                     summarize(sum(area_garage)/sum(num_garage)))[1,1]

imp_prop$num_garage_1_new = clean_prop$area_garage>0 & clean_prop$num_garage==0

clean_prop$num_garage_1 = ifelse(
  imp_prop$num_garage_1_new, 
  1, 
  clean_prop$num_garage
)

imp_prop$area_garage_1_new = clean_prop$num_garage>0 & clean_prop$area_garage==0

clean_prop$area_garage_1 =  ifelse(
  imp_prop$area_garage_1_new,
  clean_prop$num_garage * avg_garage_area,
  clean_prop$area_garage
)     

imp_prop$area_basement_isna = is.na(prop$area_basement)
clean_prop$area_basement = ifelse(is.na(prop$area_basement), -999, prop$area_basement)

imp_prop$area_patio_isna = is.na(prop$area_patio)
clean_prop$area_patio = ifelse(is.na(prop$area_patio), -999, prop$area_patio)

imp_prop$area_shed_isna = is.na(prop$area_shed)
clean_prop$area_shed = ifelse(is.na(prop$area_shed), -999, prop$area_shed)

imp_prop$area_lot_isna = is.na(prop$area_lot)
clean_prop$area_lot = ifelse(is.na(prop$area_lot), -999, prop$area_lot)

imp_prop$area_firstfloor_finished_isna = is.na(prop$area_firstfloor_finished)
clean_prop$area_firstfloor_finished = ifelse(is.na(prop$area_firstfloor_finished), -999, prop$area_firstfloor_finished)

imp_prop$area_total_calc_isna = is.na(prop$area_total_calc)
clean_prop$area_total_calc = ifelse(is.na(prop$area_total_calc), -999, prop$area_total_calc)

imp_prop$area_base_isna = is.na(prop$area_base)
clean_prop$area_base = ifelse(is.na(prop$area_base), -999, prop$area_base)

imp_prop$area_live_finished_isna = is.na(prop$area_live_finished)
clean_prop$area_live_finished = ifelse(is.na(prop$area_live_finished), -999, prop$area_live_finished)

imp_prop$area_liveperi_finished_isna = is.na(prop$area_liveperi_finished)
clean_prop$area_liveperi_finished = ifelse(is.na(prop$area_liveperi_finished), -999, prop$area_liveperi_finished)

imp_prop$area_total_finished_isna = is.na(prop$area_total_finished)
clean_prop$area_total_finished = ifelse(is.na(prop$area_total_finished), -999, prop$area_total_finished)

imp_prop$area_unknown_isna = is.na(prop$area_unknown)
clean_prop$area_unknown = ifelse(is.na(prop$area_unknown), -999, prop$area_unknown)

imp_prop$area_no_lot_and_no_total = imp_prop$area_lot_isna & imp_prop$area_total_calc_isna

imp_prop$num_pool_isna = is.na(prop$num_pool)
clean_prop$num_pool = ifelse(is.na(prop$num_pool), -999, prop$num_pool)

imp_prop$area_pool_isna = is.na(prop$area_pool)
clean_prop$area_pool = ifelse(is.na(prop$area_pool), -999, prop$area_pool)

avg_pool_area = (properties %>% 
                   filter(num_pool>0 & area_pool>0) %>% 
                   summarize(sum(area_pool)/sum(num_pool)))[1,1]

print(paste0("avg. pool area: ", avg_pool_area))


imp_prop$num_pool_1_new = clean_prop$area_pool>0 & clean_prop$num_pool==0

clean_prop$num_pool_1 = ifelse(
  imp_prop$num_pool_1_new, 
  1, 
  clean_prop$num_pool
)

imp_prop$area_pool_1_new = clean_prop$num_pool>0 & clean_prop$area_pool==0

clean_prop$area_pool_1 =  ifelse(
  imp_prop$area_pool_1_new,
  clean_prop$num_pool * avg_pool_area,
  clean_prop$area_pool
)     

imp_prop$pooltypeid2_isna = is.na(prop$pooltypeid2)
clean_prop$pooltypeid2 = ifelse(imp_prop$pooltypeid2_isna, -999, prop$pooltypeid2)

imp_prop$pooltypeid7_isna = is.na(prop$pooltypeid7)
clean_prop$pooltypeid7 = ifelse(imp_prop$pooltypeid7_isna, -999, prop$pooltypeid7)

imp_prop$pooltypeid10_isna = is.na(prop$pooltypeid10)
clean_prop$pooltypeid10 = ifelse(imp_prop$pooltypeid10_isna, -999, prop$pooltypeid10)

imp_prop$region_city_isna = is.na(prop$region_city)
clean_prop$region_city = as.integer(as.factor(ifelse(imp_prop$region_city_isna, -1, prop$region_city)))

imp_prop$region_neighbor_isna = is.na(prop$region_neighbor)
clean_prop$region_neighbor = as.integer(as.factor(ifelse(imp_prop$region_neighbor_isna, -1, prop$region_neighbor)))

imp_prop$region_county_isna = is.na(prop$region_county)
clean_prop$region_county = as.factor(ifelse(imp_prop$region_county_isna, -1, prop$region_county))

imp_prop$region_zip_isna = is.na(prop$region_zip)
clean_prop$region_zip = as.integer(as.factor(ifelse(imp_prop$region_zip_isna, -1, prop$region_zip)))

# no missing values
clean_prop$zoning_landuse_county = as.integer(as.factor(ifelse(is.na(prop$zoning_landuse_county), -1, prop$zoning_landuse_county)))

# no missing values
clean_prop$zoning_landuse = as.factor(ifelse(is.na(prop$zoning_landuse), -1, prop$zoning_landuse))

# no missing values
clean_prop$zoning_property = as.integer(as.factor(prop$zoning_property))

clean_prop$num_room = ifelse(is.na(prop$num_room), -999, prop$num_room)

imp_prop$num_story_isna = is.na(prop$num_story)
clean_prop$num_story = as.factor(ifelse(imp_prop$num_story_isna, -1, prop$num_story))

imp_prop$num_unit_isna = is.na(prop$num_unit)
clean_prop$num_unit = ifelse(imp_prop$num_unit_isna, -999, prop$num_unit)

imp_prop$num_bathroom_isna = is.na(prop$num_bathroom)
clean_prop$num_bathroom = ifelse(imp_prop$num_bathroom_isna, -999, prop$num_bathroom)

imp_prop$num_bedroom_isna = is.na(prop$num_bedroom)
clean_prop$num_bedroom = ifelse(imp_prop$num_bedroom_isna, -999, prop$num_bedroom)

imp_prop$num_bathroom_calc_isna = is.na(prop$num_bathroom_calc)
clean_prop$num_bathroom_calc = ifelse(imp_prop$num_bathroom_calc_isna, -999, prop$num_bathroom_calc)

imp_prop$num_bath_isna = is.na(prop$num_bath)
clean_prop$num_bath = ifelse(imp_prop$num_bath_isna, -999, prop$num_bath)

imp_prop$num_75_bath_isna = is.na(prop$num_75_bath)
clean_prop$num_75_bath = ifelse(imp_prop$num_75_bath_isna, -999, prop$num_75_bath)

imp_prop$num_fireplace_isna = is.na(prop$num_fireplace)
clean_prop$num_fireplace = ifelse(imp_prop$num_fireplace_isna, -999, prop$num_fireplace)

imp_prop$num_fireplace_imp = is.na(prop$num_fireplace) & prop$flag_fireplace=="true"
clean_prop$num_fireplace = ifelse(imp_prop$num_fireplace_imp, 1, clean_prop$num_fireplace)

imp_prop$tax_total_isna = is.na(prop$tax_total)
clean_prop$tax_total = ifelse(imp_prop$tax_total_isna, -999, prop$tax_total)

imp_prop$tax_building_isna = is.na(prop$tax_building)
clean_prop$tax_building = ifelse(imp_prop$tax_building_isna, -999, prop$tax_building)

imp_prop$tax_land_isna = is.na(prop$tax_land)
clean_prop$tax_land = ifelse(imp_prop$tax_land_isna, -999, prop$tax_land)

imp_prop$tax_property_isna = is.na(prop$tax_property)
clean_prop$tax_property = ifelse(imp_prop$tax_property_isna, -999, prop$tax_property)

clean_prop$tax_delinquency = ifelse(prop$tax_delinquency=="Y", 1, -999)

imp_prop$tax_delinquency_year_isna = is.na(prop$tax_delinquency_year)
clean_prop$tax_delinquency_year = ifelse(imp_prop$tax_delinquency_year_isna, -999, prop$tax_delinquency_year)

clean_prop$material = ifelse(is.na(prop$material), -1 , prop$material)

clean_prop$material = factor(clean_prop$material, 0:18, labels=c('NA', 'Adobe', 'Brick', 'Concrete Block', 'Concrete', 'Dome', 'Frame', 
                                                                 'Heavy', 'Log', 'Light', 'Metal', 'Manufactured', 'Mixed', 'Masonry', 
                                                                 'Other', 'Steel', 'Stone', 'Tilt-Up', 'Wood'))

clean_prop$deck = as.factor(ifelse(is.na(prop$deck), -1, prop$deck))

clean_prop$quality = as.factor(ifelse(is.na(prop$quality), -1, prop$quality))

clean_prop$framing = as.factor(ifelse(is.na(prop$framing), -1, prop$framing))

clean_prop$story = as.factor(ifelse(is.na(prop$story), -1, prop$story))

clean_prop$heating = as.factor(ifelse(is.na(prop$heating), -1, prop$heating))

clean_prop$aircon = as.factor(ifelse(is.na(prop$aircon), -1, prop$aircon))

clean_prop$architectural_style = as.factor(ifelse(is.na(prop$architectural_style), -1, prop$architectural_style))

clean_prop$flag_tub = ifelse(prop$flag_tub=="true", 1, -1)

clean_prop$longitude = ifelse(is.na(prop$longitude), -999, prop$longitude)
clean_prop$latitude = ifelse(is.na(prop$latitude), -999, prop$latitude)

clean_prop$censustractandblock = as.integer(as.factor(prop$censustractandblock))

##clean_prop$month = month(clean_prop$date)
clean_prop$num_garage_fac=as.factor(clean_prop$num_garage)
clean_prop$num_garage_1_fac=as.factor(clean_prop$num_garage_1)
clean_prop$num_pool_fac=as.factor(clean_prop$num_pool)
clean_prop$num_pool_1_fac=as.factor(clean_prop$num_pool_1)
clean_prop$num_room_fac=as.factor(clean_prop$num_room)
clean_prop$num_story_fac=as.factor(clean_prop$num_story)
clean_prop$num_unit_fac=as.factor(clean_prop$num_unit)
clean_prop$num_bathroom_fac=as.factor(clean_prop$num_bathroom)
clean_prop$num_bedroom_fac=as.factor(clean_prop$num_bedroom)
clean_prop$num_bath_fac=as.factor(clean_prop$num_bath)
clean_prop$num_75_bath_fac=as.factor(clean_prop$num_75_bath)
clean_prop$num_fireplace_fac=as.factor(clean_prop$num_fireplace)

t = clean_prop[, .(count=length(id_parcel)), by=zoning_property]
t = t[order(-t$count)][1:30,]
clean_prop$zoning_property_fac = as.factor(ifelse(clean_prop$zoning_property %in% t$zoning_property, clean_prop$zoning_property, 0))
clean_prop$rawcensustractandblock <- prop$rawcensustractandblock
clean_prop$fips_blockid <- sapply(clean_prop$rawcensustractandblock, function(x) substr(x, 1, 11))
clean_prop$rawcensustractandblock <- NULL

sales = as.data.table(
  train %>%
    inner_join(properties, by="id_parcel")
)

clean = as.data.table(
  train %>%
    inner_join(clean_prop, by="id_parcel")
)

imp = as.data.table(
  train %>%
    inner_join(imp_prop, by="id_parcel")
)

imp$date = NULL
imp$logerror = NULL

clean$date = as.Date(clean$date)
clean$month_factor = as.factor(month(clean$date))
clean$month = month(clean$date)

qt = quantile(clean$logerror, c(1/3, 2/3))
clean$logerror_q3 = ifelse(clean$logerror<=qt[1], 1, 
                           ifelse(clean$logerror<=qt[2], 2, 3))

clean$logerror_q3 = as.factor(clean$logerror_q3)

write.csv(clean,file="clean.csv")
write.csv(imp,file="imp.csv")
write.csv(clean_prop,file="clean_prop.csv")
write.csv(imp_prop,file="imp_prop.csv")