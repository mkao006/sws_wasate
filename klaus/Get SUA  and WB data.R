

#              WORK IN PROGRESS!


# This script should finally download and prepare the SUA data and data from World Bank.

# Ultimately this script can replace the dofile in Stata!


# Things which have to be done:
# Download data from 1961.
# Assign areanames, itemnames and foodgroup to SUA data
# Input missing gdp and paved road data of WB data


setwd("C:/Users/grunberger/Dropbox/FAO TO GO/LOSSES")
#setwd("C:/Users/carlos/Dropbox/FAO TO GO/LOSSES")

  ## Load the libraries
  library(reshape2)
  library(RJDBC)
  library(data.table)
  library(FAOSTAT)

# Set parameters for SUA data query

  options(java.parameters = "-Xmx3000m")
  lapply(dir("../codes/", full.names = TRUE), FUN = source)
  
  ## Connect to the database
  drv = JDBC(driverClass = "oracle.jdbc.driver.OracleDriver",
             classPath = "./R scipts/program/ojdbc14.jar")
  
  conn = dbConnect(drv, "jdbc:oracle:thin:@lprdbwo1:3310:fstp",
                   user = "demo", password = "demo")
  


#Just download data from 2011
SUA.QUERY = dbGetQuery(conn, "SELECT AREA, ITEM, ELE, NUM_2011, SYMB_2011 from tsv_ics_work_yr WHERE ele in ('51','61','63','71','91','93','121','141' ,'261', '264') ")

names(SUA.QUERY)[names(SUA.QUERY) == 'NUM_2011'] <- 'NUM'
names(SUA.QUERY)[names(SUA.QUERY) == 'SYMB_2011'] <- 'SYMB'
names(SUA.QUERY)[names(SUA.QUERY) == 'AREA'] <- 'areacode'
names(SUA.QUERY)[names(SUA.QUERY) == 'ITEM'] <- 'itemcode'

#Reshape the data
SUA.2011=reshape(SUA.QUERY,
                 timevar="ELE",
                 idvar=c("areacode", "itemcode"),
                 direction="wide")

SUA.2011=subset(SUA.2011, 
                select=c("areacode", "itemcode", "NUM.51", "NUM.121", "SYMB.121", "NUM.61", "NUM.63", 
                                   "NUM.261", "NUM.264", "NUM.91", "NUM.93", "NUM.141", "NUM.71")
                )


# FOLLOWING VARIABLES COULD NOT BE DOWNLOADED:
# 1) areaname
# 2) itemname
# 3) item groups

# Download WB data

Downloaded.Data = getWDItoSYB(indicator = c("NY.GDP.MKTP.PP.KD", "IS.ROD.PAVE.ZS"), 
                              name = c("gdp", "pavedroads"),
                             # startDate = 1990, endDate = format(Sys.Date(), "%Y"),
                              startDate = 2011, endDate = 2011,
                              printURL = FALSE, getMetaData = TRUE,
                              printMetaData = FALSE, saveMetaData = FALSE,
                              outputFormat = c("wide", "long"))


Downloaded.Data = Downloaded.Data$entity
names(Downloaded.Data)[names(Downloaded.Data)=="ISO2_WB_CODE"]<-"countryCode"

Downloaded.Data$Country[Downloaded.Data$Country=="Cabo Verde"]<-"Cape Verde"
Downloaded.Data$Country[Downloaded.Data$Country=="West Bank and Gaza"]<-"Occupied Palestinian Territory"
Downloaded.Data$Country[Downloaded.Data$Country=="Congo, Dem. Rep."]<-"Democratic Republic of the Congo"
Downloaded.Data$Country[Downloaded.Data$Country=="Congo, Rep."]<-"Congo"
Downloaded.Data$Country[Downloaded.Data$Country=="China"]<-"China; mainland"

Downloaded.Data$Country[Downloaded.Data$Country=="Korea, Rep."]<-"Republic of Korea"
Downloaded.Data$Country[Downloaded.Data$Country=="Korea, Dem. Rep."]<-"Democratic People's Republic of Korea"
Downloaded.Data$Country[Downloaded.Data$Country=="Lao PDR"]<-"Lao People's Democratic Republic"
Downloaded.Data$Country[Downloaded.Data$Country=="Micronesia, Fed. Sts"]<-"Micronesia (Federated States of)"
Downloaded.Data$Country[Downloaded.Data$Country=="Slovak Republic"]<-"Slovakia"
Downloaded.Data$Country[Downloaded.Data$Country=="Sudan"]<-"Sudan (former)"


#Assign FAO country codes
Downloaded.Data =fillCountryCode(country ="Country", data = Downloaded.Data,
                                 outCode ="FAOST_CODE")

# Manually assign FAOSTAT codes
Downloaded.Data$FAOST_CODE[Downloaded.Data$Country=="Hong Kong SAR, China"]=96
Downloaded.Data$FAOST_CODE[Downloaded.Data$Country=="Macao SAR, China"]=128
Downloaded.Data$FAOST_CODE[Downloaded.Data$Country=="China; mainland"]=41
Downloaded.Data$FAOST_CODE[Downloaded.Data$Country=="Micronesia (Federated States of)"]=145


#... To be continued...
