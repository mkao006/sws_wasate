# This script prepares the dataset for imputation.
# At the moment old data are used. Needs to be updated. 

#Read the file produced after adding external data (WB data, other FAO data) to the SWS data
PREDICTION2009=subset(read.dta("./raw waste and external data merged_save old.dta"),
             select=c(itemcode, itemname, areacode, areaname, newregion, year, 
                      foodgroup, foodgroupn, unsubregionname, continentname, countrycode,
                      pavedroads, gdp, mean_temp, mean_precip, sd_temp,
                      ratio,  num_51, num_61,num_71,num_91,num_121)
             , year==2009 & is.na(newfbs) & primary==1 &
               areaname!="Yugoslav SFR" & areaname!="USSR" & areaname!="The former Yugoslav Republic of Maced.." & 
               areaname!="Serbia and Montenegro"  & areaname!="Ethiopia PDR" & 
               areaname!="Czechoslovakia" & areaname!="Belgium-Luxembourg"  &
               areaname!="Cocos (Keeling) Islands" & areaname!="Gaza Strip (Palestine)" & areaname!="West Bank" & 
               areaname!="Yemen Dem" & areaname!="Yemen Ar Rp" & areaname!="Netherlands Antilles"
#CHECK if further countries have to be dropped
             )

#Append fish data (from Adam):
fish=subset(read.csv("./csv/NEW/fish_data.csv"), ItemCode!="" & ISO3!="")
fish=reshape(fish, varying=list(names(fish)[3:8]),
        v.names=c("X"), times=2007:2012,
        idvar=c("ItemCode", "ISO3","ElementCode"), timevar=c("year"),
        direction="long")
names(fish)[names(fish) == 'ItemCode'] <- 'itemname'
names(fish)[names(fish) == 'ISO3'] <- 'countrycode'
#treat all kinds of fish like meat
fish$foodgroupn=14
fish$foodgroup="meat and fish"
fish$itemcode=9999
fish$foodgroup2="Animal products"
fish$foodgroup3="primary"
fish$num_51=fish$X
fish$num_61=0
fish$num_71=0
fish$num_91=0
fish$num_121=0

fish=subset(fish,year==2009,select=-X)

#Merge country information to fish data
CountryInfo= subset(PREDICTION2009,select=c(countrycode,areaname, areacode, newregion, unsubregionname, continentname,
                             pavedroads, gdp, mean_temp, mean_precip, sd_temp))
CountryInfo <- CountryInfo[order(CountryInfo$countrycode),] 
CountryInfo$rank <- unlist(lapply(as.numeric(table(CountryInfo$countrycode)), seq_len))
CountryInfo=subset(CountryInfo,select=-c(rank),rank==1)

fish=merge(fish,CountryInfo,by="countrycode",all.x=TRUE)
#for is.na(areaname), no data are availabe
fish=subset(fish,!is.na(areaname))

#Append fish data to FBS
PREDICTION=rbind.fill(PREDICTION2009,fish)

