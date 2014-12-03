suppressMessages({
    library(faosws)
    library(faoswsAupus)
    library(faoswsUtil)
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

## Just to check which elements should be used.
print(GetCodeList(domain = "agriculture", dataset = "agriculture",
                  dimension = "measuredElement")[, list(code, description)],
      nrow = 252)



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
}
lossData = getLossData()


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

calculateLossRatio(data = lossData,
                   productionValue = paste0(valuePrefix,
                       requiredElements["production"]),
                   importValue = paste0(valuePrefix,
                       requiredElements["import"]),
                   stockWithdrawlValue = paste0(valuePrefix,
                       requiredElements["stockWithdrawl"]),
                   lossValue = paste0(valuePrefix,
                       requiredElements["loss"]))                   

## From here, we can merge with external data
