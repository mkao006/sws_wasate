## source("update_loss_external_data.R")
## source("get_sua_data.R")
## source("commodityFoodGroupClassification.R")
## load("lossData.RData")
## load("lossExternalData.RData")

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



## Merge everything together for now
getLossExternalData = function(){
    ## TODO (Michael): Get the data using R API    
    Reduce(f = function(x, y){
        merge(x, y, all = TRUE, by = intersect(colnames(x), colnames(y)))
    }, x = list(worldBankGeneralData, worldBankClimateData, newFbsAvailable))
}


getLossFoodGroup = function(){
    ## NOTE (Michael): This will be replaced by the GetMapping
    ##                 function when loaded into the data base
    data.table(read.csv(file = "lossFoodGroup.csv"))
}

getLossRegionClass = function(){
    ## NOTE (Michael): This will be replaced by the GetMapping
    ##                 function when loaded into the data base.
    regionMapping = data.table(read.csv(file = "lossRegionMapping.csv"))
    regionMapping[, geographicAreaM49 := as.character(geographicAreaM49)]
    regionMapping
}


getNewFbsStatus = function(){
    ## NOTE (Michael): This will be replaced by the GetMapping
    ##                 function when loaded into the data base.
}

getLossData = function(){
    ## Set up the query
    ##
    ## NOTE (Michael): The year is set by Klaus
    allCountryCodesTable =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "geographicAreaM49")

    ## NOTE (Michael): This is just a test, need to get Nick to
    ##                 develop a better hierachical system.
    cerealTree =
        adjacent2edge(GetCodeTree(domain = "agriculture",
                                  dataset = "agriculture",
                                  dimension = "measuredItemCPC",
                                  roots = "011"))
    dimensions =
        list(
            Dimension(name = "geographicAreaM49",
                      keys = allCountryCodesTable[type == "country", code]),
            Dimension(name = "measuredItemCPC", keys = cerealTree$children),
            Dimension(name = "measuredElement", keys = unname(requiredElements)),
            Dimension(name = "timePointYears", keys = as.character(1969:2013))
        )

    newDataKey =
        DatasetKey(domain = "agriculture",
                   dataset = "agriculture",
                   dimensions = dimensions)

    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    query = GetData(
        key = newDataKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
    )
    query[, timePointYears := as.numeric(timePointYears)]
    query
}

mergeAllData = function(lossData, lossExternalData, lossFoodGroup,
    lossRegionClass, newFbs){
    Reduce(f = function(x, y){
        merge(x, y, all.x = TRUE, by = intersect(colnames(x), colnames(y)))
    },
           x = list(lossExternalData, lossFoodGroup, lossRegionClass),
           init = lossData
           )
}

## TODO (Michael): Need to check the missing values!
finalLossData =
    mergeAllData(lossData = getLossData(),
                 lossExternalData = lossExternalDataFinal,
                 lossFoodGroup = getLossFoodGroup(),
                 lossRegionClass = getLossRegionClass(),
                 getNewFbsStatus = getNewFbsStatus())



calculateLossRatio = function(data, productionValue, importValue,
    stockWithdrawlValue, lossValue){

    data[data[[stockWithdrawlValue]] >= 0,
         lossRatio := .SD[[lossValue]]/(.SD[[productionValue]] +
                                        .SD[[importValue]] +
                                        .SD[[stockWithdrawlValue]])]
    data[data[[stockWithdrawlValue]] < 0,
         lossRatio := .SD[[lossValue]]/(.SD[[productionValue]] +
                                        .SD[[importValue]])]
    data
}

calculateLossRatio(data = finalLossData,
                   productionValue = paste0(valuePrefix,
                       requiredElements["production"]),
                   importValue = paste0(valuePrefix,
                       requiredElements["import"]),
                   stockWithdrawlValue = paste0(valuePrefix,
                       requiredElements["stockWithdrawl"]),
                   lossValue = paste0(valuePrefix,
                       requiredElements["loss"]))                   



## Assuming that we have the data ready
rawData = data.table(
    subset(read.dta("./Dataset for Estimation and Prediction in R.dta"),
        select = c(itemcode, itemname, areacode, areaname, newregion, year,
            foodgroup, foodgroupn, unsubregionname, continentname,
            pavedroads, gdp, ratio, num_61, num_51, newfbs)))


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
    ## NOTE (Michael): This is hard coded selection by Klaus
    data[timePointYears > 1969 & 
         importProductionRatio < 1 &
         lossRatio != 0 &
         geographicAreaM49 != "170" &
         foodGeneralGroup == "primary" &
         !is.na(gdp) &
         !is.na(sharePavedRoad), ]
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






## TODO (Michael): Do the prediction according on the real data.


