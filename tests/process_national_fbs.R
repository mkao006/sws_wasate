library(faosws)
library(data.table)

## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "3f113726-f40e-44b3-b2af-d5f0de77c386"
        ## token = "7fe7cbec-2346-46de-9a3a-8437eca18e2a"
        )
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




## Load new national FBS
## ---------------------------------------------------------------------
nationalFbs = data.table(read.csv(file = "data_original/newNationalFbs.csv"))
translatedNationalFbs =
    translateFStoM49(nationalFbs)
itemMapping = GetTableData(schemaName = "ess", tableName = "fcl_2_cpc")
itemMapping[, fcl := as.numeric(fcl)]
setnames(itemMapping, old = c("fcl", "cpc"),
         new = c("measuredItemFS", "measuredItemCPC"))
mapped = merge(translatedNationalFbs, itemMapping, all.x = TRUE,
    by = "measuredItemFS")
mapped[, `:=`(c("measuredItemFS", "geographicAreaFS"), NULL)]
write.csv(mapped[!is.na(measuredItemCPC), ], file = "data/nationalFbs.csv",
          row.names = FALSE, na = "", quote = TRUE)
