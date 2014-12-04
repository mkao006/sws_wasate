library(foreign)
library(magrittr)
library(data.table)
library(faosws)

## Process the raw data from Klaus
## ---------------------------------------------------------------------


## Assuming that we have the data ready
rawData = data.table(
    subset(read.dta("./Dataset for Estimation and Prediction in R.dta"),
        select = c(itemcode, itemname, areacode, areaname, newregion, year,
            foodgroup, foodgroupn, unsubregionname, continentname,
            pavedroads, gdp, ratio, num_61, num_51, newfbs)))


## Function to define new food group based on existing food group classification
defineFoodGroups = function(data, foodGroup){
    ## Create generic group
    ##
    ## NOTE (Michael): Is foodGeneralGroup a suitable name?
    data[data[[foodGroup]] %in% c(1:14), foodGeneralGroup := "primary"]
    data[data[[foodGroup]] %in% c(15), foodGeneralGroup := "live animals"]
    data[data[[foodGroup]] %in% c(16:98), foodGeneralGroup := "processed"]
    data[data[[foodGroup]] %in% c(99), foodGeneralGroup := "unclassified"]
    
    ## Create perishableness group
    data[data[[foodGroup]] %in% c(1, 4, 5, 6, 9),
         foodPerishableGroup := "non perishable"]
    data[data[[foodGroup]] %in% c(2, 3),
         foodPerishableGroup := "semi perishable"]
    data[data[[foodGroup]] %in% c(7, 8),
         foodPerishableGroup := "fresh fruits and vegetable"]
    data[data[[foodGroup]] %in% c(10:14),
         foodPerishableGroup := "animal products"]
    ## NOTE (Michael): Why?
    data[is.na(foodPerishableGroup), foodPerishableGroup := foodGeneralGroup]
    data[, `:=`(c("foodGeneralGroup", "foodPerishableGroup"),
                list(as.factor(foodGeneralGroup), as.factor(foodPerishableGroup)))]
    ## NOTE (Klaus): There are undefined item
    data    
}



## Function to create regional classification relevant to loss based
## on it's original region and country.
defineLossRegionClassification = function(data, unsdSubRegion, areaName){
    data[data[[unsdSubRegion]] %in% c("Northern Africa", "Western Asia"),
         lossRegionClass := "North Africa and Middle East"]
    data[data[[unsdSubRegion]] %in% c("Southern Africa", "Eastern Africa",
                                      "Middle Africa", "Western Africa") |
         data[[areaName]] %in% c("Sudan (former)", "South Sudan"),
         lossRegionClass := "Sub Saharan Africa"]    
    data[data[[unsdSubRegion]] %in% c("Southern Asia", "Southeastern Asia",
                                      "Micronesia", "Polynesia", "Melanesia",
                                      "Eastern Asia"),
         lossRegionClass := "South/-eastern Asia and Pacific"]
    data[data[[unsdSubRegion]] %in% c("Caribbean", "Central America",
                                      "South America"),
         lossRegionClass := "Latin America"]
    data[data[[unsdSubRegion]] %in% c("Central Asia") |
         data[[areaName]] %in% c("Iran", "Afghanistan", "Pakistan"),
         lossRegionClass := "Central Asia to Pakistan"]
    data[data[[unsdSubRegion]] %in% c("Western Europe", "Southern Europe",
                                      "Northern Europe") |
         data[[areaName]] %in% c("Belgium-Luxembourg", "Israel"),
         lossRegionClass := "Western Europe and Israel"]

    data[data[[unsdSubRegion]] %in% c("Eastern Europe") |
         data[[areaName]] %in% c( "Albania", "Bosnia and Herzegovina", "Croatia",
                                 "Montenegro" , "Serbia", "Slovenia" ,
                                 "The former Yugoslav Republic of Macedonia",
                                 "Serbia and Montenegro" , "Yugoslav SFR", 
                                 "Czechoslovakia" , "USSR"),
         lossRegionClass := "Eastern Europe"]

    data[data[[areaName]] %in% c("Australia", "New Zealand", "Republic of Korea",
                                 "Japan", "Northern America"),
         lossRegionClass := "AUS, NZ, USA, CAN, JAP, KOR"]
    data[is.na(lossRegionClass), lossRegionClass := .SD[[unsdSubRegion]]]
    data[, lossRegionClass := as.factor(lossRegionClass)]
    data
}

finalData =
    rawData %>%
    defineFoodGroups(data = ., foodGroup = "foodgroupn") %>%
    defineLossRegionClassification(data = .,
                                   unsdSubRegion = "unsubregionname",
                                   areaName = "areaname")
    


## Load the translation mapping
## ---------------------------------------------------------------------

## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "3f113726-f40e-44b3-b2af-d5f0de77c386"
        ## token = "7fe7cbec-2346-46de-9a3a-8437eca18e2a"
        )
}


## Get the area and item mapping
areaMapping = GetTableData(schemaName = "ess", tableName = "fal_2_m49")
setnames(areaMapping,
         old = c("fal", "m49"),
         new = c("geographicAreaFS", "geographicAreaM49"))
itemMapping = GetTableData(schemaName = "ess", tableName = "fcl_2_cpc")
setnames(itemMapping,
         old = c("fcl", "cpc"),
         new = c("measuredItemFS", "measuredItemCPC"))
## WARNING (Michael): This is a hack since there is one cpc to multiple fs item
itemMapping[, measuredItemFS := as.numeric(measuredItemFS)]
itemMapping = itemMapping[!measuredItemFS %in% c("0067", "0068"), ]



## Write out the food group mapping
## ---------------------------------------------------------------------
itemFoodGroupMapping =
    unique.data.frame(finalData[, list(itemcode, itemname, foodgroup, foodgroupn,
                                       foodGeneralGroup, foodPerishableGroup)])
setnames(itemFoodGroupMapping,
         old = c("itemcode", "itemname", "foodgroup", "foodgroupn"),
         new = c("measuredItemFS", "measuredItemNameFS", "foodGroupName",
             "foodGroup"))
lossFoodGroup = merge(itemFoodGroupMapping, itemMapping, by = c("measuredItemFS"),
    all.x = TRUE)

## NOTE (Michael): Don't think item (-1) exist.
lossFoodGroup = lossFoodGroup[measuredItemFS != -1, ]

## TODO (Michael): Check the fcl codes which does not have a CPC then
##                 remove the fcl classification.
##
## TODO (Michael): recode the following
## ifelse(levels(foodgroup) == "meat", "meat and fish",
##        levels(foodgroup)))
write.csv(lossFoodGroup, file = "lossFoodGroup.csv", row.names = FALSE, na = "")


## Write out the region grouping
## ---------------------------------------------------------------------
areaRegionMapping =
    unique.data.frame(finalData[, list(areacode, areaname, lossRegionClass)])
## NOTE (Michael): Remove data where area code is missing
areaRegionMapping = areaRegionMapping[!is.na(areacode), ]
setnames(areaRegionMapping,
         old = c("areacode", "areaname"),
         new = c("geographicAreaFS", "geographicAreaNameFS"))
areaRegionMapping[, geographicAreaFS :=  as.character(geographicAreaFS)]
lossRegionMapping = merge(areaRegionMapping, areaMapping, by = "geographicAreaFS")
write.csv(lossRegionMapping[, list(geographicAreaM49, lossRegionClass)],
          file = "lossRegionMapping.csv", row.names = FALSE, na = "")
