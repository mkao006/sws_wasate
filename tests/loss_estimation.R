## source("update_loss_external_data.R")
## source("get_sua_data.R")
source("commodityFoodGroupClassification.R")

load("lossData.RData")
load("lossExternalData.RData")

suppressMessages({
    library(faosws)
    library(faoswsAupus)
    library(faoswsUtil)
    library(data.table)
    library(foreign)
    library(magrittr)    
})


## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
requiredElements = c("5510", "5610", "5712", "5015")
names(requiredElements) = c("production", "import", "stockWithdrawl", "loss")
valuePrefix = "Value_measuredElement_"
flagObsPrefix = "flagObservationStatus_measuredElement_"
flagMethodPrefix = "flagMethod_measuredElement_"

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
## This is a hack since there is one cpc to multiple fs item
itemMapping = itemMapping[!measuredItemFS %in% c("0067", "0068"), ]



## Merge everything together for now
getLossExternalData = function(){
    ## TODO (Michael): Get the data using R API    
    Reduce(f = function(x, y){
        merge(x, y, all = TRUE, by = intersect(colnames(x), colnames(y)))
    }, x = list(worldBankGeneralData, worldBankClimateData, newFbsAvailable))
}

lossExternalData = getLossExternalData()

## TODO (Michael): Move this to the update part
##
## Remove redudant columns
lossExternalData[, `:=`(c("geographicAreaISO2WB", "geographicAreaNameISO2WB",
                          "geographicAreaISO3"), NULL)]
lossExternalData[, geographicAreaFS := as.character(geographicAreaFS)]
lossData[, timePointYears := as.numeric(timePointYears)]



lossExternalDataFinal =
    merge(lossExternalData, areaMapping, by = "geographicAreaFS")


lossDataFinal =
    merge(lossData, itemMapping, by = "measuredItemCPC")

lossDataFinal[, foodGroup :=
                  commodityToFoodGroup(as.numeric(measuredItemFS), commodityList)]


dataFinal = merge(lossExternalDataFinal, lossDataFinal,
    by = intersect(colnames(lossExternalDataFinal), colnames(lossDataFinal)))



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

defineFoodGroups(rawData, foodGroup = "foodgroupn")

## Function to create regional classification relevant to loss based
## on it's original region and country.
defineLossRegionClassification = function(data, unsdSubRegion, areaName){
    data[data[[unsdSubRegion]] %in% c("Northern Africa", "Western Asia"),
         lossClassRegion := "North Africa and Middle East"]
    data[data[[unsdSubRegion]] %in% c("Southern Africa", "Eastern Africa",
                                      "Middle Africa", "Western Africa") |
         data[[areaName]] %in% c("Sudan (former)", "South Sudan"),
         lossClassRegion := "Sub Saharan Africa"]    
    data[data[[unsdSubRegion]] %in% c("Southern Asia", "Southeastern Asia",
                                      "Micronesia", "Polynesia", "Melanesia",
                                      "Eastern Asia"),
         lossClassRegion := "South/-eastern Asia and Pacific"]
    data[data[[unsdSubRegion]] %in% c("Caribbean", "Central America",
                                      "South America"),
         lossClassRegion := "Latin America"]
    data[data[[unsdSubRegion]] %in% c("Central Asia") |
         data[[areaName]] %in% c("Iran", "Afghanistan", "Pakistan"),
         lossClassRegion := "Central Asia to Pakistan"]
    data[data[[unsdSubRegion]] %in% c("Western Europe", "Southern Europe",
                                      "Northern Europe") |
         data[[areaName]] %in% c("Belgium-Luxembourg", "Israel"),
         lossClassRegion := "Western Europe and Israel"]

    data[data[[unsdSubRegion]] %in% c("Eastern Europe") |
         data[[areaName]] %in% c( "Albania", "Bosnia and Herzegovina", "Croatia",
                                 "Montenegro" , "Serbia", "Slovenia" ,
                                 "The former Yugoslav Republic of Macedonia",
                                 "Serbia and Montenegro" , "Yugoslav SFR", 
                                 "Czechoslovakia" , "USSR"),
         lossClassRegion := "Eastern Europe"]

    data[data[[areaName]] %in% c("Australia", "New Zealand", "Republic of Korea",
                                 "Japan", "Northern America"),
         lossClassRegion := "AUS, NZ, USA, CAN, JAP, KOR"]
    data[is.na(lossClassRegion), lossClassRegion := .SD[[unsdSubRegion]]]
    data[, lossClassRegion := as.factor(lossClassRegion)]
    data
}

defineLossRegionClassification(data = rawData,
                               unsdSubRegion = "unsubregionname",
                               areaName = "areaname")

## Function to perform final manipulation
preEstimationProcessing = function(data){

    ## TODO (Michael): Need to remove these hard coded processing
    data[is.na(num_61), num_61 := 0]
    data[is.na(num_51), num_51 := 0]
    data[, importToProductionRatio := num_61/num_51]
    data[, itemname := as.factor(itemname)]
    data[, time := year - 1960]
    data[is.na(newfbs), newfbs := 0]

    ## NOTE (Klaus): GDP over 25000 PPP are truncated and assume it does
    ##               not have any relevant effects on the changes in losses.
    data[gdp > 25000, gdp := 25000]

    ## NOTE (Klaus): Assume the food group level of meat is the same as
    ##               meat and fishes.
    levels(data$foodgroup) =
        with(data,
             ifelse(levels(foodgroup) == "meat", "meat and fish",
                    levels(foodgroup)))

    data
}

preEstimationProcessing(data = rawData)


## Function to create the desired estimation sample
createEstimationSample = function(data){
    data[year > 1969 & 
         importProductionRatio < 1 &
         ratio != 0 &
         areaname!="Peru" &
         foodGeneralGroup == "primary" &
         !is.na(gdp) &
         !is.na(pavedroads), ]
}


## Function to estimate the loss regression
lossRegression = function(data){

    ## REGESSION (1): Item-specific dummies

    itemSpecificLoss.lm =
        lm(I(log(ratio+0.05)) ~ itemname + lossClassRegion + time +
           foodPerishableGroup + pavedroads + pavedroads:foodPerishableGroup +
           I(log(gdp)) + I(log(gdp)^2) + I(log(gdp)):foodPerishableGroup +
           I(log(gdp)^2):foodPerishableGroup + newfbs, data = data)


    ## REGESSION (2): No item-specific dummies.
    ##
    ## This regression is performed for impute losses for commodities
    ## for which no (or very few) observations are available.
    ##
    ## Use item group-specific dummies. (Both, item and group dummies,
    ## cannot be used at the same time.)

    foodGroupLoss.lm =
        lm(I(log(ratio + 0.05)) ~ foodgroup + lossClassRegion + time +
           foodPerishableGroup + pavedroads + pavedroads:foodPerishableGroup +
           I(log(gdp)) + I(log(gdp)^2) + I(log(gdp)):foodPerishableGroup +
           I(log(gdp)^2):foodPerishableGroup + newfbs, data = data)

    list(itemSpecificModel = itemSpecificLoss.lm,
         foodGroupModel = foodGroupLoss.lm)
}


## Build the model
lossModels =
    rawData %>%
    defineFoodGroups(data = ., foodGroup = "foodgroupn") %>%
    defineLossRegionClassification(unsdSubRegion = "unsubregionname",
                                   areaName = "areaname") %>%
    preEstimationProcessing %>%
    createEstimationSample %>%
    lossRegression

lapply(lossModels, summary)





## TODO (Michael): Find the food group classification in the database,
##                 otherwise request Nick to find it and load it.
##
## TODO (Michael): Make the define region and defined food group into
##                 parameter files.
##
## TODO (Michael): Re-write the hard coded food group and region function.
##
## TODO (Michael): Do the prediction according on the real data.
##
## CHECK (Michael): What is item code (-1)?
