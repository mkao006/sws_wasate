########################################################################
## Title: This script is the draft for the update loss external data
##        module which will be run separately and scheduled by the
##        system.
## Date: 2014-12-02
########################################################################

library(rWBclimate)
library(FAOSTAT)
library(data.table)
library(magrittr)
library(faosws)
library(zoo)

## TODO (Michael): Need to check with Nick how flexible is the
##                 expansion of the adhoc dataset.

requiredIndicator = c("NY.GDP.PCAP.KD", "NY.GDP.MKTP.PP.KD", "IS.ROD.PAVE.ZS")
requiredIndicatorName = c("gdpPerCapita", "gdpPPP", "sharePavedRoad")


## The country list for using the WB climate API
countryList = as.list(c(NoAm_country, SoAm_country, Oceana_country,
    Africa_country,  Asia_country, Eur_country))



## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "3f113726-f40e-44b3-b2af-d5f0de77c386"
        ## token = "7fe7cbec-2346-46de-9a3a-8437eca18e2a"
        )
}

## Download general World Bank data
## ---------------------------------------------------------------------


## Function for changing the name, this function is currently needed
## because the name are hard coded in Klaus's code.
modifyWBCountryName = function(worldBankData){
    worldBankData[Country == "Cabo Verde", Country := "Cape Verde"]
    worldBankData[Country == "West Bank and Gaza",
                  Country :=  "Occupied Palestinian Territory"]
    worldBankData[Country == "Congo, Dem. Rep.",
                  Country := "Democratic Republic of the Congo"]
    worldBankData[Country == "Congo, Rep.", Country := "Congo"]
    worldBankData[Country == "China", Country := "China; mainland"]
    worldBankData[Country == "Korea, Rep.",
                  Country := "Republic of Korea"]
    worldBankData[Country == "Korea, Dem. Rep.",
                  Country := "Democratic People's Republic of Korea"]
    worldBankData[Country == "Lao PDR", 
                  Country := "Lao People's Democratic Republic"]
    worldBankData[Country == "Micronesia, Fed. Sts",
                  Country := "Micronesia (Federated States of)"]
    worldBankData[Country == "Slovak Republic", Country := "Slovakia"]
    worldBankData[Country == "Sudan", Country := "Sudan (former)"]
}

getWorldBankGeneralData = function(indicator, name){
    raw = data.table(getWDItoSYB(indicator = indicator, name = name)$entity)
    nameModified.dt = modifyWBCountryName(raw)
    translated.dt =
        data.table(translateCountryCode(data = data.frame(nameModified.dt),
                                        from = "ISO2_WB_CODE",
                                        to = "FAOST_CODE"))
    setnames(translated.dt,
             old = c("FAOST_CODE", "ISO2_WB_CODE", "Year", "Country"),
             new = c("geographicAreaFS", "geographicAreaISO2WB", "timePointYears",
                 "geographicAreaNameISO2WB"))
    setkeyv(translated.dt, c("geographicAreaFS", "timePointYears"))
    translated.dt
}

naiveImputeWorldBankGeneralData = function(data, indicator){

    ## NOTE (Michael): This is just linear interpolation followed by first/last
    ##                 observation carried
    naiveImputation = function (x){

        nobserved = length(na.omit(x))
        n = length(x)
        type =
            ifelse(nobserved == 0, "none",
                   ifelse(nobserved ==  1, "repeat", "naive"))
        switch(type, none = {
            tmp = rep(NA, n)
        }, `repeat` = {
            tmp = rep(na.omit(x), n)
        }, naive = {
            tmp = na.locf(na.locf(na.approx(x, na.rm = FALSE), na.rm = FALSE), 
                na.rm = FALSE, fromLast = TRUE)
        })
        as.numeric(tmp)
    }

    data[, `:=`(c(indicator),
                lapply(data[, indicator, with = FALSE], naiveImputation)),
         by = "geographicAreaM49"]
    data
}


translateFStoM49 = function(data){
    ## NOTE (Michael): This is a temporary solution, the GetTableData
    ##                 will be replaced when the GetMapping function
    ##                 is correctly set up .
    areaMapping = GetTableData(schemaName = "ess", tableName = "fal_2_m49")
    setnames(areaMapping,
             old = c("fal", "m49"),
             new = c("geographicAreaFS", "geographicAreaM49"))
    areaMapping[, geographicAreaFS := as.numeric(geographicAreaFS)]
    mapped = merge(data, areaMapping, by = "geographicAreaFS", all.x = TRUE)
    setkeyv(mapped, c("geographicAreaM49", "timePointYears"))
    setcolorder(mapped,
                neworder = c(key(mapped),
                    colnames(mapped)[!colnames(mapped) %in% key(mapped)]))
                    
    mapped
}


## Need to wait for Nick to set up the data base
SaveWorldBankGeneralWBData = function(data){
    ## SaveData(domain = , dataset = , data = data)
    write.csv(data, file = "data/worldBankGeneralData.csv",
              row.names = FALSE, na = "")
}

## TODO (Michael): Check the imputation
worldBankGeneralData =
    getWorldBankGeneralData(indicator = requiredIndicator,
                            name = requiredIndicatorName) %>%
    translateFStoM49 %>%
    naiveImputeWorldBankGeneralData(data = .,
                                    indicator = requiredIndicatorName) %T>%
    SaveWorldBankGeneralWBData



## Download World Bank climate data
## ---------------------------------------------------------------------

getAllHistoricalPrecipitation = function(countryList){

    ## Function to catch error for precipitation function if the call failed
    get_historical_precip_try = function(locator, time_scale){
        attempt =
            try(get_historical_precip(locator = locator, time_scale = time_scale))
        if(inherits(attempt, "try-error"))
            attempt = NULL
        attempt
    }

    ## Download precipitation data
    precipData =
        data.table(
            Reduce(f = function(x, y){
                rbind(x, get_historical_precip_try(locator = y,
                                                   time_scale = "year"))
            }, x = countryList[-1],
                   init = get_historical_precip_try(countryList[1], "year"))
        )
    setnames(precipData, "data", "precipitation")
    precipData
}

getAllHistoricalTemperature = function(countryList){

    ## Function to catch error for temperature function if the call failed
    get_historical_temp_try = function(locator, time_scale){
        attempt =
            try(get_historical_temp(locator = locator, time_scale = time_scale))
        if(inherits(attempt, "try-error"))
            attempt = NULL
        attempt
    }

    ## Download temperature data
    tempData =
        data.table(
            Reduce(f = function(x, y){
                rbind(x, get_historical_temp_try(locator = y, time_scale = "year"))
            }, x = countryList[-1],
                   init = get_historical_temp_try(countryList[1], "year"))
        )
    setnames(tempData, "data", "temperature")
    tempData
}

getWorldBankClimateData = function(countryList){
    list(getAllHistoricalTemperature(countryList),
         getAllHistoricalPrecipitation(countryList))
}

mergeWorldBankClimateData = function(dataList){
    mergedClimateData =
        Reduce(f = function(x, y){
            merge(x, y, all = TRUE, by = intersect(colnames(x), colnames(y)))
           }, x = dataList)
    setcolorder(mergedClimateData,
                neworder = c("locator", "year", "precipitation", "temperature"))
    finalClimateData =
        data.table(translateCountryCode(data.frame(mergedClimateData),
                                        from = "ISO3_CODE", to = "FAOST_CODE",
                                        oldCode = "locator"))
    ## NOTE (Michael): These area are not mapped in the working system
    ##                 and thus are eliminated.
    finalClimateData = finalClimateData[!ISO3_CODE %in% c("GGY", "UMI", "JEY"), ]
    setnames(finalClimateData,
             old = c("FAOST_CODE", "ISO3_CODE", "year"),
             new = c("geographicAreaFS", "geographicAreaISO3", "timePointYears"))

    finalClimateData
}

SaveWorldBankClimateData = function(data){
    ## NOTE (Michael): This should be changed to save back by the R API
    ## SaveData(domain = , dataset = , data = data)

    write.csv(data, file = "data/worldBankClimateData.csv",
              row.names = FALSE, na = "")
}

worldBankClimateData =
    getWorldBankClimateData(countryList) %>%
    mergeWorldBankClimateData %>%
    translateFStoM49 %T>%
    SaveWorldBankClimateData

