# This script makes loss estimations and imputations for all primary commodities and areas. 
# It uses a dataset consisting of (1) 'official' loss data from the FBS (empty flag)
# and (2) additional loss data, mostly coming from National FBS found on the web.

# The script "Load dataset for imputation.R" chooses the dataset for which losses
# are imptued.
# At the moment only old data are available (SUA data downloaded in Spring 2013).  
# The data need to be replaced with newer data. The best way would be a automated query
# as in "download SUA data.R"

library(foreign)
library(doBy)
library(plyr)

setwd("C:/Documents and Settings/grunberger/My Documents/LOSSES/FWS Loss Imputations")
#setwd("C:/Users/carlos/FAO/LOSSES/FWS Loss Imputations")


#####################                                 #####################
#            #             # Read Loss Data     #              #          #
#(This dataset is currently generated in Stata. R-translation should be done.)
#####################                                 #####################
#   Read data (at the moment produced in the estimation do-file)
DATA=subset(read.dta("./Dataset for Estimation and Prediction in R.dta"),
            select=c(itemcode, itemname, areacode, areaname, newregion, year, foodgroup, foodgroupn, unsubregionname, continentname,
                     pavedroads, gdp, ratio, num_61, num_51, newfbs))

#Data from external data (this needs to be cleaned at a earlier stage).  
DATA=subset(DATA, areaname!="")
DATA=subset(DATA, itemcode!=-1)

#Construct a matrix with all commodities and countries + country characteristics
source("./Load dataset for imputation.R")

#Future way of loading the data by automated query (work in progress)
#source("./R scipts/Get SUA and WB data.R")

#####################                                 #####################
#                    # Define Regression Covariates #                     #
#####################                                 #####################

#Merge meat and fish (fish data form Fisheries. No loss data available.)
DATA=within(DATA, levels(foodgroup)[levels(foodgroup) == "meat"] <- "meat and fish")
PREDICTION$foodgroup=as.factor(PREDICTION$foodgroup)
PREDICTION=within(PREDICTION, levels(foodgroup)[levels(foodgroup) == "meat"] <- "meat and fish")

# 'newfbs' defines data from external data collection. 
PREDICTION$newfbs=0
DATA$newfbs[is.na(DATA$newfbs)]=0

# Truncate GDP (very rich countries produced outliers).
# Assumes that over 25000 PPP (2005!)$ no relevant changes in losses happen.
PREDICTION$gdp[PREDICTION$gdp>25000]=25000
DATA$gdp[DATA$gdp>25000]=25000

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Classify food groups (including more general food groups)
define.foodgroups=function(DATA.INPUT){
  
  #Very generic group
  DATA.INPUT$foodgroup3[DATA.INPUT$foodgroupn>=1 & DATA.INPUT$foodgroupn<=14]="primary"
  DATA.INPUT$foodgroup3[DATA.INPUT$foodgroupn==15]="live animals"
  DATA.INPUT$foodgroup3[DATA.INPUT$foodgroupn>=16 & DATA.INPUT$foodgroupn<99]="processed"
  DATA.INPUT$foodgroup3[DATA.INPUT$foodgroupn==99]=NA
  
  #Classify primary food into 4 types of perishableness
  DATA.INPUT$foodgroup2[
    DATA.INPUT$foodgroupn==1 | DATA.INPUT$foodgroupn==4 |
      DATA.INPUT$foodgroupn==5 | DATA.INPUT$foodgroupn==6 | DATA.INPUT$foodgroupn==9]="Non perishable"
  
  DATA.INPUT$foodgroup2[
    DATA.INPUT$foodgroupn==2 | DATA.INPUT$foodgroupn==3]="Semi perishable"
  
  DATA.INPUT$foodgroup2[
    DATA.INPUT$foodgroupn==7 | DATA.INPUT$foodgroupn==8]="FFV"
  
  DATA.INPUT$foodgroup2[
    DATA.INPUT$foodgroupn>=10 & DATA.INPUT$foodgroupn<=14]="Animal products"
  
  DATA.INPUT$foodgroup2[is.na(DATA.INPUT$foodgroup2)]=DATA.INPUT$foodgroup3[is.na(DATA.INPUT$foodgroup2)]
  #keep in mind that there are undefined items
  DATA.INPUT
}

DATA=define.foodgroups(DATA.INPUT=DATA)
PREDICTION=define.foodgroups(DATA.INPUT=PREDICTION)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Classify subregions 
# Modyfy UN-subregions
define.region=function(DATA.INPUT){
  
  DATA.INPUT$estimation.region[
    DATA.INPUT$unsubregion=="Northern Africa" | DATA.INPUT$unsubregion=="Western Asia" 
    ]="North Africa & Middle East"
  
  DATA.INPUT$estimation.region[
    DATA.INPUT$unsubregion=="Southern Africa" | DATA.INPUT$unsubregion=="Eastern Africa"
    | DATA.INPUT$unsubregion=="Middle Africa"| DATA.INPUT$unsubregion=="Western Africa"
    | DATA.INPUT$areaname=="Sudan (former)"  | DATA.INPUT$areaname=="South Sudan"     
    ]="SSA"  
  
  DATA.INPUT$estimation.region[
    DATA.INPUT$unsubregion=="Southern Asia" | DATA.INPUT$unsubregion=="Southeastern Asia"
    | DATA.INPUT$unsubregion=="Micronesia"| DATA.INPUT$unsubregion=="Polynesia"
    | DATA.INPUT$unsubregion=="Melanesia" | DATA.INPUT$unsubregion=="Eastern Asia"
    ]="South/-east Asia and Pacific"
    
  DATA.INPUT$estimation.region[
    DATA.INPUT$unsubregion=="Caribbean" | DATA.INPUT$unsubregion=="Central America" 
    | DATA.INPUT$unsubregion=="South America"     
    ]="Latin America"
  
  DATA.INPUT$estimation.region[
    DATA.INPUT$unsubregion=="Central Asia" | DATA.INPUT$areaname=="Iran" |
      DATA.INPUT$areaname=="Afghanistan"| DATA.INPUT$areaname=="Pakistan"     
    ]="Central Asia to Pakistan"  
  
  DATA.INPUT$estimation.region[
    DATA.INPUT$unsubregion=="Western Europe" | DATA.INPUT$unsubregion=="Southern Europe" | DATA.INPUT$unsubregion=="Northern Europe" |
      DATA.INPUT$areaname=="Belgium-Luxembourg" | DATA.INPUT$areaname=="Israel"
    ]="Western Europe and Israel"  
  
  #Albania etc. are allocated to Southern Europe...
  DATA.INPUT$estimation.region[
    DATA.INPUT$unsubregion=="Eastern Europe" | DATA.INPUT$areaname=="Albania"
    | DATA.INPUT$areaname=="Bosnia and Herzegovina"| DATA.INPUT$areaname=="Croatia"
    | DATA.INPUT$areaname=="Montenegro" | DATA.INPUT$areaname=="Serbia"
    | DATA.INPUT$areaname=="Slovenia" | DATA.INPUT$areaname=="The former Yugoslav Republic of Macedonia"
    | DATA.INPUT$areaname=="Serbia and Montenegro" | DATA.INPUT$areaname=="Yugoslav SFR" 
    | DATA.INPUT$areaname=="Czechoslovakia" | DATA.INPUT$areaname=="USSR"
    ]="Eastern Europe"  
  
  DATA.INPUT$estimation.region[
    DATA.INPUT$areaname=="Australia" | DATA.INPUT$areaname=="New Zealand" | 
      DATA.INPUT$areaname=="Republic of Korea" | DATA.INPUT$areaname=="Japan" | 
      DATA.INPUT$unsubregion=="Northern America"  
    ]="AUS,NZ,USA,CAN,JAP,KOR"
  
  DATA.INPUT$estimation.region[is.na(DATA.INPUT$estimation.region)] =DATA.INPUT$unsubregion[is.na(DATA.INPUT$estimation.region)] 
  
  DATA.INPUT  
}

DATA=define.region(DATA.INPUT=DATA)
PREDICTION=define.region(DATA.INPUT=PREDICTION)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Prepare Estimation
# Make some adjustment before the estimation:

pre.imputation=function(DATA.INPUT){
  
  DATA.INPUT$num_61[is.na(DATA.INPUT$num_61)]=0
  DATA.INPUT$num_51[is.na(DATA.INPUT$num_51)]=0
  
  DATA.INPUT$imp_prod=DATA.INPUT$num_61/DATA.INPUT$num_51
  
  
  DATA.INPUT$itemname=as.factor(DATA.INPUT$itemname)
  
  DATA.INPUT$estimation.region=as.factor(DATA.INPUT$estimation.region)
  
  DATA.INPUT$foodgroup2=as.factor(DATA.INPUT$foodgroup2)
  
  DATA.INPUT$time= DATA.INPUT$year-1960
  
  DATA.INPUT  
  
}

DATA=pre.imputation(DATA.INPUT=DATA)
PREDICTION=pre.imputation(DATA.INPUT=PREDICTION)

#####################                                 #####################
#                    # Select the Estimation Sample #                     #
#####################                                 #####################

ESTIMATION.SAMPLE=subset(DATA, year>1969 & 
                           imp_prod<1 &
                           ratio!=0 &
                           areaname!="Peru" &
                           foodgroup3=="primary" &
                           !is.na(gdp & pavedroads)
)
# (Note: For PROCESSED commodities there are only few observation. Max 9 unique country/commodity observation
#  as for Rice milled or  Refined Sugar; 7 obs for Wheat flour.
#  It is not worth to extend analysis to processed commodities.
#  Observations of Peru are dropped because loss ratios are unreasonably high.)

#Merge (again) categories of meat and fish (fish data form Fisheries. No loss data available.)
#(Note: This categorization should be done in the data preparation)
ESTIMATION.SAMPLE=within(ESTIMATION.SAMPLE, levels(foodgroup)[levels(foodgroup) == "meat"] <- "meat and fish")


#####################                                 #####################
#             # Count unique country/commodity observations #             #
#####################                                 #####################

# Count unique country/commodity observations; indepentent of time. Observations are probably repeated
# over time. 
ESTIMATION.SAMPLE$country.item.obs =with(ESTIMATION.SAMPLE, ave(itemcode,itemcode, areacode, FUN = seq_along))
table(ESTIMATION.SAMPLE$itemname[ESTIMATION.SAMPLE$country.item.obs==1])

# Create variable 'sum.country.item.obs' which indicates how many unique country/commodity observations 
# exist for a specific commodity. Results of the first regressions will be taken only for commodities with\
# enough observations
ESTIMATION.SAMPLE=merge(ESTIMATION.SAMPLE,
                        aggregate(country.item.obs ~ itemcode, data=subset(ESTIMATION.SAMPLE,country.item.obs==1), NROW),
                        by="itemcode",y.all=T)
names(ESTIMATION.SAMPLE)[names(ESTIMATION.SAMPLE) == 'country.item.obs.y'] <- 'sum.country.item.obs'
names(ESTIMATION.SAMPLE)[names(ESTIMATION.SAMPLE) == 'country.item.obs.x'] <- 'country.item.obs'
ESTIMATION.SAMPLE= ESTIMATION.SAMPLE[with(ESTIMATION.SAMPLE, order(itemcode, areacode)), ]

#####################                                 #####################
#             #               REGRESSIONS                   #             #
#####################                                 #####################

# REGESSION (1): Item-specific dummies

IMPUTATION.REGRESSION= lm(
  I(log(ratio+0.05)) ~ itemname + estimation.region + time + foodgroup2 + pavedroads + pavedroads:foodgroup2 + 
    I(log(gdp)) + I(log(gdp)^2) + I(log(gdp)):foodgroup2 + I(log(gdp)^2):foodgroup2  + newfbs
  ,data=ESTIMATION.SAMPLE
)

summary(IMPUTATION.REGRESSION)

# REGESSION (2): No item-specific dummies. 
# This regression is performed for impute losses for commodities for which no (or very few) 
# observations are available. 
# Use item group-specific dummies. (Both, item and group dummies, cannot be used at the same time.)

IMPUTATION.REGRESSION.foodgroups= lm(
  I(log(ratio+0.05)) ~ foodgroup + estimation.region + time + foodgroup2 + pavedroads + pavedroads:foodgroup2 + 
    I(log(gdp)) + I(log(gdp)^2) + I(log(gdp)):foodgroup2 + I(log(gdp)^2):foodgroup2 + newfbs
  ,data=ESTIMATION.SAMPLE
)

summary(IMPUTATION.REGRESSION.foodgroups)


# Set minimum requirement of unique country/commodity observation for which loss rates are predicted
# using parameters of regression (1).
MinReq=3
# Rationale of this selection: If for a commodity only observations of only one or two country are available,
# than this observations determine alone the losses for all countries (through the commodity dummy).
# This increased the risk mismeasurement. Of course, also a minimun requriremnt of 3 is set arbitrarily. 

# Finally make prediction just for commodities for which data are available. 
# For the rest, estimate a regression without commodity dummies.

#Define items which satisfy the MinReq:
fac.data=as.data.frame(
  levels(    factor(ESTIMATION.SAMPLE$itemcode[ESTIMATION.SAMPLE$sum.country.item.obs>=MinReq])))
names(fac.data)<-c("itemcode")
fac.data$item.estimated=TRUE


#Define items which do not satisfy the MinReq:

#First make a list of all primary items
all.primary.items=as.data.frame((
  levels(factor(PREDICTION$itemcode[PREDICTION$foodgroup3=="primary"]) )))
names(all.primary.items)<-c("itemcode")
all.primary.items$item.is.estimated=FALSE3
#Check which items have not satified the MinReq
item.list=merge(all.primary.items,fac.data, by="itemcode", all=TRUE)
items.not.in.sample=subset(item.list,is.na(item.estimated))
items.not.in.sample=subset(items.not.in.sample,select=-item.estimated)
colnames(items.not.in.sample)[2]="item.estimated"

#####################                                 #####################
# For items satisfying MinReq, impute losses with parameters of regression (1)
IMPUTATIONS=predict(IMPUTATION.REGRESSION, newdata=merge(PREDICTION,fac.data, by="itemcode"))
# For items not satisfying MinReq, impute losses with parameters of regression (2)
IMPUTATIONS2=predict(IMPUTATION.REGRESSION.foodgroups, newdata=merge(PREDICTION,items.not.in.sample, by="itemcode"))

#De-logarithmize losses by using Duan Smear factor
IMPUTATIONS=cbind(exp(IMPUTATIONS)*mean(exp(IMPUTATION.REGRESSION$residuals)),
                  merge(PREDICTION,fac.data, by="itemcode"))

IMPUTATIONS2=cbind(exp(IMPUTATIONS2)*mean(exp(IMPUTATION.REGRESSION.foodgroups$residuals)),
                   merge(PREDICTION,items.not.in.sample, by="itemcode"))

names(IMPUTATIONS)[1]="Losses"
names(IMPUTATIONS2)[1]="Losses"
IMPUTATIONS=rbind(IMPUTATIONS,IMPUTATIONS2)
names(IMPUTATIONS)[1]="Losses"
IMPUTATIONS$Losses=round(IMPUTATIONS$Losses,digit=2)

#####################                                 #####################
#                          # Outlier correction #                         #
#####################                                 #####################

# FUNCTION:
IntQuant <- function(INPUT){
  Q1 <- quantile(INPUT$Losses,probs=0.25,names = F,na.rm = T)
  Q3 <- quantile(INPUT$Losses,probs=0.75,names = F,na.rm = T)
  IQR <- Q3 - Q1
  INPUT$Losses.IQR.corr=INPUT$Losses
  INPUT$Losses.IQR.corr[INPUT$Losses<Q1 - 1.5*IQR]=Q1 - 1.5*IQR
  INPUT$Losses.IQR.corr[INPUT$Losses>Q3 + 1.5*IQR]=Q3 + 1.5*IQR    
  INPUT
}
# (The previous method was to truncate at the 98th item-specific percentile)
IMPUTATIONS <- ddply(IMPUTATIONS, .(itemcode),.fun =function(IMPUTATIONS) IntQuant(IMPUTATIONS))

#####################                                 #####################
#                          # ............. #                         #
#####################                                 #####################


REAL.FBS.LOSSES=IMPUTATIONS
REAL.FBS.LOSSES <- ddply(REAL.FBS.LOSSES, .(itemcode),.fun =function(REAL.FBS.LOSSES) IntQuant(REAL.FBS.LOSSES))
