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



## Function to get the 2 external world bank data sets and merge them
getLossExternalData = function(){
    ## TODO (Michael): Get the data using R API
    worldBankGeneralData =
        data.table(read.csv(file = "data/worldBankGeneralData.csv"))
    ## worldBankGeneral
    worldBankClimateData =
        data.table(read.csv(file = "data/worldBankClimateData.csv"))
    merged = Reduce(f = function(x, y){
        merge(x, y, all = TRUE, by = intersect(colnames(x), colnames(y)))
    }, x = list(worldBankGeneralData, worldBankClimateData))
    merged[, geographicAreaM49 := as.character(geographicAreaM49)]
    merged
}

## Function to load the loss food group classification
getLossFoodGroup = function(){
    ## NOTE (Michael): This will be replaced by the GetMapping
    ##                 function when loaded into the data base
    data.table(read.csv(file = "data/lossFoodGroup.csv"))
}

## Function to load the loss region classification
getLossRegionClass = function(){
    ## NOTE (Michael): This will be replaced by the GetMapping
    ##                 function when loaded into the data base.
    regionMapping = data.table(read.csv(file = "data/lossRegionMapping.csv"))
    regionMapping[, geographicAreaM49 := as.character(geographicAreaM49)]
    regionMapping
}

## Function to load the national fbs dataset
getNationalFbs = function(){
    ## NOTE (Michael): This will be replaced by the GetMapping
    ##                 function when loaded into the data base.
    data.table(read.csv(file = "data/nationalFBS.csv"))
}

## Function to load the data required for loss estimation
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


## Function to merge the national fbs data to the loss data
mergeNationalFbs = function(lossData, nationalFbs){
    lossData[, fromNationalFbs := 0]
    nationalFbs[, fromNationalFbs := 1]
    lossWithNationalFbs = rbind(lossData, nationalFbs, fill = TRUE)
    lossWithNationalFbs
}
    

## Function to merge all the required data
mergeAllLossData = function(lossData, lossExternalData, lossFoodGroup,
    lossRegionClass){
    Reduce(f = function(x, y){
        merge(x, y, all.x = TRUE, by = intersect(colnames(x), colnames(y)))
    },
           x = list(lossExternalData, lossFoodGroup, lossRegionClass),
           init = lossData
           )
}



## TODO (Michael): Need more comprehensive list of food group, there
##                 are missing items.
## WARNING (Michael): This is entirely a hack since the classification
##                    of certain commodities were missing.
hackLossGroupRegion = function(data){
    foodGroupVariable = grep("food", colnames(data), value = TRUE)
    foodGroupVariable = foodGroupVariable[foodGroupVariable != "foodGroup"]
    data[, `:=`(c(foodGroupVariable),
                lapply(data[, foodGroupVariable, with = FALSE],
                       FUN = function(x){
                           x[is.na(x)] = "unclassified"
                       }))]
    data[is.na(lossRegionClass), lossRegionClass := "unclassified"]
    data
}



## TODO (Michael): Need to check the missing values.
##
## Merge everything together
finalLossData =
    mergeAllLossData(lossData = mergeNationalFbs(getLossData(), getNationalFbs()),
                     lossExternalData = getLossExternalData(),
                     lossFoodGroup = getLossFoodGroup(),
                     lossRegionClass = getLossRegionClass())


## Function to calculate the ratio
##
## WARNINGS (Michael): There are infinity and missing values, need to
##                     check division by zero
calculateLossRatio = function(data,
    productionValue = paste0(valuePrefix, requiredElements["production"]),
    importValue = paste0(valuePrefix, requiredElements["import"]),
    stockWithdrawlValue = paste0(valuePrefix, requiredElements["stockWithdrawl"]),
    lossValue = paste0(valuePrefix, requiredElements["loss"])){

    data[data[[stockWithdrawlValue]] >= 0,
         lossRatio := .SD[[lossValue]]/(.SD[[productionValue]] +
                                        .SD[[importValue]] +
                                        .SD[[stockWithdrawlValue]])]
    data[data[[stockWithdrawlValue]] < 0,
         lossRatio := .SD[[lossValue]]/(.SD[[productionValue]] +
                                        .SD[[importValue]])]
    data
}



## NOTE (Michael): Try to remove the hard code
##
## Function to perform final manipulation
preEstimationProcessing = function(data){
    ## Convert variables to factor for modelling
    factorVariables = c("geographicAreaM49", "measuredItemCPC", "foodGroupName",
                  "foodGeneralGroup", "foodPerishableGroup", "lossRegionClass")
    data[, `:=`(c(factorVariables),
                lapply(data[, factorVariables, with = FALSE], as.factor))]
    

    ## TODO (Michael): Need to remove these hard coded processing
    data[is.na(Value_measuredElement_5610), Value_measuredElement_5610 := 0]
    data[is.na(Value_measuredElement_5510), Value_measuredElement_5510 := 0]
    data[, importToProductionRatio :=
             Value_measuredElement_5610/Value_measuredElement_5510]
    data[, scaledTimePointYears := timePointYears - 1960]
    data[is.na(fromNationalFbs), fromNationalFbs := 0]

    ## NOTE (Klaus): GDP per capita over 25000 are truncated and
    ##               assume it does not have any relevant effects on
    ##               the changes in losses.
    data[gdpPerCapita > 25000, gdpPerCapita := 25000]

    ## NOTE (Klaus): Assume the food group level of meat is the same as
    ##               meat and fishes.
    levels(data$foodGroupName) =
        with(data,
             ifelse(levels(foodGroupName) == "meat", "meat and fish",
                    levels(foodGroupName)))

    data
}


## Function to create the desired estimation sample
splitLossData = function(data, estimationSubset){

    ## NOTE (Michael): This is hard coded selection by Klaus
    if(missing(estimationSubset))
        estimationSubset =
            expression(which(timePointYears > 1969 & 
                             importToProductionRatio < 1 &
                             lossRatio != 0 &
                             geographicAreaM49 != "170" &
                             foodGeneralGroup == "primary" &
                             !is.na(gdpPerCapita) &
                             !is.na(sharePavedRoad)))
   
    estimationSubsetIndex = eval(substitute(estimationSubset), data)

    estimationData = data[estimationSubsetIndex, ]
    predictionData = data[-estimationSubsetIndex, ]
    list(estimationData = estimationData, predictionData = predictionData)
}




## Function to estimate the loss regression
lossRegression = function(data){

    ## REGESSION (1): Item-specific dummies

    itemSpecificLoss.lm =
        lm(I(log(lossRatio+0.05)) ~ measuredItemCPC + lossRegionClass +
           scaledTimePointYears + foodPerishableGroup + sharePavedRoad +
           sharePavedRoad:foodPerishableGroup + I(log(gdpPerCapita)) +
           I(log(gdpPerCapita)^2) + I(log(gdpPerCapita)):foodPerishableGroup +
           I(log(gdpPerCapita)^2):foodPerishableGroup +
           fromNationalFbs, data = data)


    ## REGESSION (2): No item-specific dummies.
    ##
    ## This regression is performed for impute losses for commodities
    ## for which no (or very few) observations are available.
    ##
    ## Use item group-specific dummies. (Both, item and group dummies,
    ## cannot be used at the same scaledTimePointYears.)

    foodGroupLoss.lm =
        lm(I(log(lossRatio + 0.05)) ~ foodGroupName + lossRegionClass +
           scaledTimePointYears + foodPerishableGroup + sharePavedRoad +
           sharePavedRoad:foodPerishableGroup + I(log(gdpPerCapita)) +
           I(log(gdpPerCapita)^2) + I(log(gdpPerCapita)):foodPerishableGroup +
           I(log(gdpPerCapita)^2):foodPerishableGroup +
           fromNationalFbs, data = data)

    list(itemSpecificModel = itemSpecificLoss.lm,
         foodGroupModel = foodGroupLoss.lm)
}

## Build the data
trainPredictData =
    finalLossData %>%
    hackLossGroupRegion %>% 
    calculateLossRatio %>%
    preEstimationProcessing %>%
    splitLossData

## Estimate the model and then make the prediction
predictedData =
    trainPredictData$estimationData %>%
    lossRegression %>%
    predict(object = .[[1]], newdata = trainPredictData$predictionData)




## Check the model
lossModel = lossRegression(trainPredictData$estimationData)
lapply(lossModels, summary)

