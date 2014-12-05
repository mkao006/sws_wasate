suppressMessages({
    library(faosws)
    library(faoswsAupus)
    library(faoswsUtil)
    library(data.table)
    library(foreign)
    library(magrittr)    
})

## Add test comment

## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
requiredElements = c("5510", "5610", "5712", "5015")
names(requiredElements) = c("production", "import", "stockWithdrawl", "loss")
oldItems =
    c("0111", "23110", "39120.01", "23710", "0113", "23162",
      "23161.01", "23161.02", "23161.03", "0115", "24310.01", "0112",
      "23120.03", "39120.04", "0116", "0117", "0118", "0114", "01192",
      "01194", "01191", "01199.02", "01199.90", "01510", "23220.05",
      "01530", "01520", "23170.01", "01591", "01550", "01540", "01599",
      "01802", "01801", "23510", "23511.02", "23520", "23540", "23210.04",
      "01701", "01702", "01705", "01703", "01706", "01707", "01704",
      "01709.01", "01709.02", "01709.90", "01377", "01372", "01373",
      "01371", "01376", "01374", "01379.90", "0141", "0142", "21421",
      "01460", "01492", "01491.01", "01450", "2167", "F0262", "01445",
      "21631.01", "01443", "01444", "01442", "01448", "01921.01", "0143",
      "01441", "01449.90", "21691.90", "01212", "01216", "01211", "01214",
      "01215", "01234", "01213", "01235", "01232", "01231", "01253.01",
      "01253.02", "01252", "01254", "01241.02", "01242", "01243", "01251",
      "01290.01", "01270", "01290.90", "F0472", "F0475", "01312", "01313",
      "01323", "01324", "01322", "01321", "01329", "01341", "01342.01",
      "01342.02", "01343", "01344.01", "01344.02", "01345", "01346",
      "01354", "01353.01", "01351.01", "01351.02", "01355.01", "01355.02",
      "01355.90", "01330", "24212.02", "01221", "01229", "01315", "01316",
      "01311", "01318", "01352", "01317", "01319", "01359.90", "21419.05",
      "21439.9", "F0623", "01911", "01919.01", "01610", "01640", "02111",
      "21111.01", "21151", "21512", "21111.02", "02211", "22241.01",
      "22110.02", "22221.01", "22251.01", "22130.03", "22251.04", "22290",
      "02112", "02122", "21115", "02291", "02123", "21116", "21156",
      "02292", "02140", "21113.01", "21153", "21113.02", "21511.02",
      "02151", "21121", "0231", "02154", "02153", "02152", "0232", "02131",
      "02132", "02133", "02121.01", "02191", "21117.02", "21170.02",
      "21170.93", "02199.20")
requiredItems = oldItems[1:30]
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


    dimensions =
        list(
            Dimension(name = "geographicAreaM49",
                      keys = allCountryCodesTable[type == "country", code]),
            Dimension(name = "measuredItemCPC", keys = requiredItems),
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

getTradeData = function()
    ## Set up the query
    ##
    ## NOTE (Michael): The year is set by Klaus
    allCountryCodesTable =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "geographicAreaM49")

    dimensions =
        list(
            Dimension(name = "geographicAreaM49",
                      keys = allCountryCodesTable[type == "country", code]),
            Dimension(name = "measuredItemCPC", keys = requiredItems),
            Dimension(name = "measuredElement", keys = "5610"),
            Dimension(name = "timePointYears", keys = as.character(1969:2013))
        )

    newDataKey =
        DatasetKey(domain = "trade",
                   dataset = "total_trade",
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
    lossRegression



## Check the model
lossModel = lossRegression(trainPredictData$estimationData)
lapply(lossModels, summary)
