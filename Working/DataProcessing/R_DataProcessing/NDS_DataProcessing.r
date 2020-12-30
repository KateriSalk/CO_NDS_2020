
###################################*
#### Colorado - Nutrient Diffusing Substrate (NDS) ####
###################################*


###################################*
#### Contact Info ####
###################################*

# Mark Fernandez (mark.fernandez@tetratech.com)





###################################.
#### Citations ####
###################################.

# R Citation
citation()
# R Core Team. 2020. R: A language and environment for statistical computing. R Foundation for Statistical Computing,Vienna,Austria. URL https://www.R-project.org/.




###################################*
#### Acronyms ####
###################################*

# NDS - Nutrient Diffusing Substrate
# MMI - Macroinvertebrate Multimetric Index




###################################*
#### Load Packages ####
###################################*

# Load packages
# options(digits=7L)
par(mar=c(4.5,4.5,1,1),bty='l')
library(openxlsx)    # read.xlsx()
library(data.table)  # [
library(ggplot2)     # ggplot()
library(viridisLite) # 
# library(visdat)      # vis_miss()
# detach('package:aaa',unload=T)



# Clean workspace
rm(list=ls()); gc()

# Settings
na.strings=c(NA,'NA','N/A','#N/A','','None','<Null>','.','Not Analyzed')
seed=27709L
pointsize=9L
txtSize=9L


#### Source functions
source('../../R_Functions/labelsPretty.r')
source('../../R_Functions/inclusive.r')
source('../../R_Functions/invLogit.r')
# source('../../R_Functions/mode.r')





###################################*
#### Data Notes ####
###################################*

# Note: "Date" for chla is "Harvest Date", not "Deployment Date".



###################################*
#### Load 2012 Data ####
###################################*

#### Sites Data
# Note that "Chla" tab has duplicate lat/longs for SiteID 5577 and 5580. Use "Chem Data" tab for sites.
sites2012=read.xlsx('../../../Source/Received/2012 NDS Pilot Study Data Summary_20150416.xlsx',sheet='Chem Data',startRow=1,check.names=T,na.strings=na.strings)
setDT(sites2012)
dim(sites2012) # 19 xx

# Filter fields
tmp=c('Waterbody','StationID','Location','Lat','Long')
sites2012=sites2012[,..tmp]

# Rename fields
tmp=c('StreamID','SiteID','SiteDescription','Lat','Long')
cbind(names(sites2012),tmp)
names(sites2012)=tmp

# Unique
sites2012=unique(sites2012)
dim(sites2012)            # 3 xx
uniqueN(sites2012$SiteID) # 3 - MUST MATCH ABOVE

# Add Year
sites2012[,Year:=2012]



#### Load chem data
chem2012=read.xlsx('../../../Source/Received/2012 NDS Pilot Study Data Summary_20150416.xlsx',sheet='Chem Data',startRow=1,check.names=T,na.strings=na.strings)
setDT(chem2012)
dim(chem2012) # 19 xx

# Filter fields
tmp=c('StationID','Date','Sample.Type',
		'pH','SpC','temp','DO'
		# See other file for 2012 nutrient data
		)
chem2012=chem2012[,..tmp]

# Rename fields
tmp=c('SiteID','Date','SampleType',
		'pH','SpecCond_uScm','WaterTemp_C','DO_mgL')
cbind(names(chem2012),tmp)
names(chem2012)=tmp

# Convert to date
chem2012[,Date:=as.IDate(Date,origin='1899-12-30',tz='UTC')]
head(chem2012$Date)

# Other datasets have 2 dates: Deployment and Harvest. This dataset has 4 dates.
# Chla harvest date was 9/07/2012 (see email from Blake on 12/16/2020).
# Use 8/30 and 9/07 dates.
chem2012[,.N,by=Date]
#          Date N
# 1: 2012-08-17 3
# 2: 2012-08-23 3
# 3: 2012-08-30 3
# 4: 2012-09-07 3
# 5: 2012-10-31 5
# 6:       <NA> 2
chem2012=chem2012[!is.na(Date),]
chem2012=chem2012[Date %in% as.Date(c('2012-08-30','2012-09-07')),]
dim(chem2012) # 6 xx

# Remove Dups/Blanks
chem2012[,.N,by=SampleType]
chem2012=chem2012[SampleType=='Routine',]
dim(chem2012) # 6 xx




#### Nutrient 2012 data
# Note: The "Chem Data" tab is missing nutrient data. Use this Excel file instead.
nutr2012=read.xlsx('../../../Source/Received/2012 Pilot NDS Study/Data/Chem Extracts_20130109.xlsx',sheet='Sheet1',startRow=1,check.names=T,na.strings=na.strings)
setDT(nutr2012)
dim(nutr2012) # 37 xx

# Keep first 19 rows
nutr2012=nutr2012[1:19,]
dim(nutr2012) # 19 xx

# Filter fields
tmp=c('StationID','X_CollDate',
		'X.TN',
		'Nitrogen..Kjeldahl..Total',
		'Nitrogen..Nitrate.Nitrite',
		'Nitrogen..Ammonia',
		'Phosphorus..Phosphate.Total')
nutr2012=nutr2012[,..tmp]

# Rename fields
tmp=c('SiteID','Date',
		'TN_mgL',
		'TKN_mgL',
		'NO23_mgL',
		'NH3_mgL',
		'TP_mgL')
cbind(names(nutr2012),tmp)
names(nutr2012)=tmp

# Convert to date
nutr2012[,Date:=substr(Date,1,10)]
nutr2012[,Date:=as.IDate(Date,format='%m/%d/%Y',tz='UTC')]
head(nutr2012$Date)

# Other datasets have 2 dates: Deployment and Harvest. This dataset has 4 dates.
# Chla harvest date was 9/07/2012 (see email from Blake on 12/16/2020).
# Use 8/30 and 9/07 dates.
nutr2012[,.N,by=Date]
# Date N
# 1: 2012-08-17 3
# 2: 2012-08-23 3
# 3: 2012-08-30 3
# 4: 2012-09-07 3
# 5: 2012-10-31 2
# 6:       <NA> 3
# 7: 2013-01-10 2
nutr2012=nutr2012[!is.na(Date),]
nutr2012=nutr2012[Date %in% as.Date(c('2012-08-30','2012-09-07')),]
dim(nutr2012) # 6 xx


#### Join chem and nutrients
intersect(names(chem2012),names(nutr2012)) # SiteID Date
chem2012=merge(chem2012,nutr2012,by=c('SiteID','Date'),all.x=T) # Left outer join
rm(nutr2012)

# Unpivot
chem2012=melt(chem2012,id.vars=c('SiteID','Date','SampleType'),variable.factor=F,na.rm=T) # Will convert to numeric later
chem2012[,.N,by=variable]
dim(chem2012) # 54 xx

# Add result
chem2012[,result:=value]

# Add ND
chem2012[,ND:=0L]
head(sort(unique(chem2012$value)),50)
tail(sort(unique(chem2012$value)),50)
chem2012[grep('<',value),ND:=1]
chem2012[,.N,by=ND]

# Convert to numeric
chem2012[,value:=sub('^<','',value)]
chem2012[,value:=as.numeric(value)]

# Set NDs to 1/2 MDL
# Note: No MDL for this dataset. So set to 1/2 reported value.
chem2012[ND==1,.N,by=variable]
#    variable N
# 1:  TKN_mgL 3
chem2012[ND==1 & variable=='TN_mgL'  ,value:=value/2]
chem2012[ND==1 & variable=='TKN_mgL' ,value:=value/2]
chem2012[ND==1 & variable=='NO23_mgL',value:=value/2]
chem2012[ND==1 & variable=='NH3_mgL' ,value:=value/2]
chem2012[ND==1 & variable=='TP_mgL'  ,value:=value/2]





#### Chla Data
# Chla values updated in "*_MJP.xlsx" file. See email from Mike to the client on 12/16/2020.
# See "/Source/Received/2020 Pilot NDS Study/Data/_NDS Chl20131002_MJP_20201216.xlsx" for pre-processing work.
chla2012=read.xlsx('../../../Source/Received/2012 NDS Pilot Study Data Summary_20150416_MJP_20201216.xlsx',sheet='Chla',startRow=1,check.names=T,na.strings=na.strings)
setDT(chla2012)
dim(chla2012) # 60 xx

# Filter fields
tmp=c('StationID','X_RecvDt','Group','Vial','Chla..mg.m2.')
chla2012=chla2012[,..tmp]

# Rename fields
tmp=c('SiteID','Date','Treatment','VialID','Chla_mgM2')
cbind(names(chla2012),tmp)
names(chla2012)=tmp

# Convert to date
chla2012[,Date:=substr(Date,1,10)]
chla2012[,Date:=as.IDate(Date,format='%m/%d/%Y',tz='UTC')]
head(chla2012$Date)

# Unpivot
chla2012[,variable:='Chla_mgM2']
setnames(chla2012,'Chla_mgM2','value')





###################################*
#### Load 2013 Data ####
###################################*

#### Sites Data
sites2013=read.xlsx('../../../Source/Received/2013 NDS Study Data Summary_20150416.xlsx',sheet='Chla Data',startRow=1,check.names=T,na.strings=na.strings)
setDT(sites2013)
dim(sites2013) # 287 xx

# Filter fields
tmp=c('Waterbody','StationID','Description','Lat','Long')
sites2013=sites2013[,..tmp]

# Rename fields
tmp=c('StreamID','SiteID','SiteDescription','Lat','Long')
cbind(names(sites2013),tmp)
names(sites2013)=tmp

# Unique
sites2013=unique(sites2013)
dim(sites2013)            # 15 xx
uniqueN(sites2013$SiteID) # 15 - MUST MATCH ABOVE

# Add Year
sites2013[,Year:=2013]

# Load SiteType LUT
lut=fread('SiteSiteTypeLUT.csv',sep=',',header=T,skip=0,na.strings=na.strings)
lut[,.N,by=SiteType]
# Note: SiteID "12809" should NOT have a SiteType.
dim(lut) # 15 xx

# Join
intersect(names(sites2013),names(lut)) # SiteID
sites2013=merge(sites2013,lut,by=c('SiteID'),all.x=T)
rm(lut)




#### Load chem data
chem2013=read.xlsx('../../../Source/Received/2013 NDS Study Data Summary_20150416.xlsx',sheet='Chem Data and Summary',startRow=1,check.names=T,na.strings=na.strings)
setDT(chem2013)
dim(chem2013) # 39 xx

# Remove last row (blank)
chem2013=chem2013[1:38,]
dim(chem2013) # 38 xx

# Filter fields
tmp=c('StationID','Date','Sample.Type',
		'pH','SpC','Temp','DO',
		'Total.Nitrogen.Calculated',
		'Nitrogen..Kjeldahl..Total',
		'Nitrogen..Nitrate.Nitrite',
		'Nitrogen..Ammonia','Phosphorus..Phosphate.Total')
chem2013=chem2013[,..tmp]

# Rename fields
tmp=c('SiteID','Date','SampleType',
		'pH','SpecCond_uScm','WaterTemp_C','DO_mgL',
		'TN_mgL','TKN_mgL','NO23_mgL','NH3_mgL','TP_mgL')
cbind(names(chem2013),tmp)
names(chem2013)=tmp

# Convert to date
chem2013[,Date:=as.IDate(Date,origin='1899-12-30',tz='UTC')]
head(chem2013$Date)

# Remove Dups/Blanks
chem2013[,.N,by=SampleType]
chem2013=chem2013[SampleType=='Routine',]
dim(chem2013) # 34 xx

# Unpivot
chem2013=melt(chem2013,id.vars=c('SiteID','Date','SampleType'),variable.factor=F,na.rm=T) # Will convert to numeric later
dim(chem2013) # 306 xx

# Add result
chem2013[,result:=value]

# Add ND
chem2013[,ND:=0L]
head(sort(unique(chem2013$value)),50)
tail(sort(unique(chem2013$value)),50)
chem2013[grep('<',value),ND:=1]
chem2013[,.N,by=ND]

# Convert to numeric
chem2013[,value:=sub('^<','',value)]
chem2013[,value:=as.numeric(value)]

# Set NDs to 1/2 MDL
# Note: No MDL for this dataset. So set to 1/2 reported value.
chem2013[ND==1,.N,by=variable]
#    variable  N
# 1:  TKN_mgL  6
# 2: NO23_mgL 15
# 3:  NH3_mgL 10
# 4:   TP_mgL  2
chem2013[ND==1 & variable=='TN_mgL'  ,value:=value/2]
chem2013[ND==1 & variable=='TKN_mgL' ,value:=value/2]
chem2013[ND==1 & variable=='NO23_mgL',value:=value/2]
chem2013[ND==1 & variable=='NH3_mgL' ,value:=value/2]
chem2013[ND==1 & variable=='TP_mgL'  ,value:=value/2]

# Drop fields
tmp=c('SiteID','Date','SampleType','variable','result','value','ND')
chem2013=chem2013[,..tmp]





#### Chla Data
chla2013=read.xlsx('../../../Source/Received/2013 NDS Study Data Summary_20150416.xlsx',sheet='Chla Data',startRow=1,check.names=T,na.strings=na.strings)
setDT(chla2013)
dim(chla2013) # 287 xx

# Filter fields
tmp=c('StationID','Date_Harvest','Group','Vial','Chla..mg.m2.')
chla2013=chla2013[,..tmp]

# Rename fields
tmp=c('SiteID','Date','Treatment','VialID','Chla_mgM2')
cbind(names(chla2013),tmp)
names(chla2013)=tmp

# Convert to date
chla2013[,Date:=as.IDate(Date,origin='1899-12-30',tz='UTC')]
head(chla2013$Date)

# Unpivot
chla2013[,variable:='Chla_mgM2']
setnames(chla2013,'Chla_mgM2','value')

# Fix typo (see email from Blake on 11/27/2020)
chla2013[SiteID=='12554A' & VialID=='N92' & value>100,.N] # 1 typo updated
chla2013[SiteID=='12554A' & VialID=='N92' & value>100,VialID:='NP92']




###################################*
#### Load 2014 Data ####
###################################*

#### Sites Data
sites2014=read.xlsx('../../../Source/Received/2014 NDS Study Data Summary_20150416.xlsx',sheet='Stations',startRow=2,check.names=T,na.strings=na.strings) # Start at row 2!
setDT(sites2014)
dim(sites2014) # 21 xx

# Filter fields
tmp=c('Waterbody.1','StationID','Description.1','Lat.1','Long.1')
sites2014=sites2014[,..tmp]

# Rename fields
tmp=c('StreamID','SiteID','SiteDescription','Lat','Long')
cbind(names(sites2014),tmp)
names(sites2014)=tmp

# Unique
sites2014=unique(sites2014)
dim(sites2014)            # 21 xx
uniqueN(sites2014$SiteID) # 21 - MUST MATCH ABOVE

# Add Year
sites2014[,Year:=2014]



#### Load chem data
chem2014=read.xlsx('../../../Source/Received/2014 NDS Study Data Summary_20150416.xlsx',sheet='Chem and Summary',startRow=1,check.names=T,na.strings=na.strings)
setDT(chem2014)
dim(chem2014) # 50 xx

# Filter fields
tmp=c('StationID','X_CollDate','Sample.Type',
		'pH','Conductivity','Temp','Diss.Oxygen',
		'Nitrogen..Total..Subcontracted.','X_Units.23','X_Qualifier.23','X_MDL.23',
		'Nitrogen..Nitrate.Nitrite','X_Units.24','X_Qualifier.24','X_MDL.24',
		'Nitrogen..Ammonia','X_Units.19','X_Qualifier.19','X_MDL.19',
		'Phosphorus..Phosphate.Total','X_Units.20','X_Qualifier.20','X_MDL.20')
chem2014=chem2014[,..tmp]

# Rename fields
tmp=c('SiteID','Date','SampleType',
		'pH','SpecCond_uScm','WaterTemp_C','DO_mgL',
		'TN_mgL','Unit_TN','ND_Code_TN','MDL_TN',
		'NO23_mgL','Unit_NO23','ND_Code_NO23','MDL_NO23',
		'NH3_mgL','Unit_NH3','ND_Code_NH3','MDL_NH3',
		'TP_mgL','Unit_TP','ND_Code_TP','MDL_TP')
cbind(names(chem2014),tmp)
names(chem2014)=tmp

# Convert to date
chem2014[,Date:=as.IDate(Date,origin='1899-12-30',tz='UTC')]
head(chem2014$Date)

# Remove Dups/Blanks
chem2014[,.N,by=SampleType]
chem2014=chem2014[SampleType=='Routine',]
dim(chem2014) # 42 xx

# Unpivot
tmp=c('SiteID','Date','SampleType',
		'Unit_TN','ND_Code_TN','MDL_TN',
		'Unit_NO23','ND_Code_NO23','MDL_NO23',
		'Unit_NH3','ND_Code_NH3','MDL_NH3',
		'Unit_TP','ND_Code_TP','MDL_TP')
chem2014=melt(chem2014,id.vars=tmp,variable.factor=F,na.rm=T) # Will convert to numeric later
dim(chem2014) # 332 xx

# Add result
chem2014[,result:=value]

# Prep value
chem2014[,value:=gsub('\u00A0',' ',value,fixed=T)] # Replace non-breaking spaces.
chem2014[,value:=trimws(value)]
chem2014[SiteID=='10324' & variable=='pH' & value=='8-80',.N] # 1 typo
chem2014[SiteID=='10324' & variable=='pH' & value=='8-80',value:='8.80']
head(sort(unique(chem2014$value)),50)
tail(sort(unique(chem2014$value)),50)

# Add ND
chem2014[,ND:=0L]
chem2014[grep('<',value),ND:=1]
chem2014[variable=='TN_mgL'   & grepl('BDL',ND_Code_TN),ND:=1]
chem2014[variable=='NH3_mgL'  & grepl('BDL',ND_Code_NH3),ND:=1]
chem2014[variable=='NO23_mgL' & grepl('BDL',ND_Code_NO23),ND:=1]
chem2014[variable=='TP_mgL'   & grepl('BDL',ND_Code_TP),ND:=1]
chem2014[,.N,by=ND]

# Convert to numeric
chem2014[,value:=sub('^<','',value)]
chem2014[,value:=as.numeric(value)]

# Check that MDLs are all numeric
sapply(chem2014[,.(MDL_NH3,MDL_NO23,MDL_TN,MDL_TP)],class)

# Set NDs to 1/2 MDL
chem2014[ND==1,.N,by=variable]
#    variable  N
# 1: NO23_mgL 33
# 2:  NH3_mgL  1
# 3:   TP_mgL  2
chem2014[ND==1 & variable=='NO23_mgL' & !is.na(MDL_NO23),value:=MDL_NO23/2]
chem2014[ND==1 & variable=='NH3_mgL' & !is.na(MDL_NH3),value:=MDL_NH3/2]
chem2014[ND==1 & variable=='TP_mgL' & !is.na(MDL_TP),value:=MDL_TP/2]

# Drop fields
tmp=c('SiteID','Date','SampleType','variable','result','value','ND')
chem2014=chem2014[,..tmp]



#### Chla Data
chla2014=read.xlsx('../../../Source/Received/2014 NDS Study Data Summary_20150416.xlsx',sheet='2014 NDS Vials Chla',startRow=1,check.names=T,na.strings=na.strings)
setDT(chla2014)
dim(chla2014) # 459 xx

# Filter fields
tmp=c('StationID','HarvestDate','Group','Rack','Vial','Chla..mg.m2.')
chla2014=chla2014[,..tmp]

# Rename fields
tmp=c('SiteID','Date','Treatment','RackNum','VialID','Chla_mgM2')
cbind(names(chla2014),tmp)
names(chla2014)=tmp

# Remove Chla NAs
chla2014=chla2014[!is.na(Chla_mgM2)]
dim(chla2014) # 377 xx

# Convert to date
chla2014[,Date:=as.IDate(Date,origin='1899-12-30',tz='UTC')]
head(chla2014$Date)

# Unpivot
chla2014[,variable:='Chla_mgM2']
setnames(chla2014,'Chla_mgM2','value')

# Remove extra rack 17 (see email from Blake on 11/27/2020)
chla2014[SiteID=='9240' & RackNum==17,.N] # 19 removed
chla2014[SiteID=='9240' & RackNum==17,value:=NA]
chla2014=chla2014[!is.na(value),]
chla2014[,RackNum:=NULL]
dim(chla2014) # 358 xx

# Note: SiteID = 9717 has 2 racks (field dup). Chla values are similar. Average in "SiteYear" code.




###################################*
#### *** GrabLong *** ####
###################################*

# Append data
GrabLong=rbindlist(list(chem2012,chem2013,chem2014,
								chla2012,chla2013,chla2014),use.names=T,fill=T)
dim(GrabLong) # 1397 xx

# Update ND
GrabLong[is.na(ND),ND:=0L]

# Add Year, Month, and DOY
GrabLong[,Year:=year(Date)]
GrabLong[,Month:=month(Date)]
GrabLong[,DOY:=yday(Date)]

# Check fields
GrabLong[,.N,by=SiteID]
GrabLong[,.N,by=SampleType]
GrabLong[,.N,by=variable]
GrabLong[,.N,by=Treatment] # "NAs" are chem data
GrabLong[,.N,by=ND]





###################################*
#### Zero/Negative Values ####
###################################*

# Check for value <0
GrabLong[value<0,.N,by=variable] # Empty

# Set impossible negative values to NA
GrabLong[value<0 & variable=='aaa',value:=NA]

# Check for value=0
GrabLong[value==0,.N,by=variable] # Empty

# Check min non-zero values
tmp=GrabLong[value>0,min(value),by=variable]

# Set zero values to min/2
GrabLong[value==0 & variable=='aaa',value:=tmp[variable=='aaa',V1]/2]

# Remove value=NA
sum(is.na(GrabLong$value)) # 0 removed
GrabLong=GrabLong[!is.na(value),]
dim(GrabLong) # 1397 xx




###################################*
#### Explore Data ####
###################################*

# Boxplot - Raw
tmp=c('pH','WaterTemp_C','DO_mgL')
png('Explore/Boxplot_Raw.png',width=6.5,height=3.0,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(GrabLong[variable %in% tmp,],aes(x='',y=value))+
	geom_jitter(aes(color=as.factor(Year)),position=position_jitter(w=0.2,h=0))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	scale_y_continuous(trans='identity',labels=labelsPretty)+
	scale_color_viridis_d()+
	facet_wrap(~variable,ncol=3,scales='free_y')+
	labs(y='Value',x=NULL,title=NULL,color='Year')+
	theme_bw()+
	theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
	theme(text=element_text(size=txtSize))+
	theme(legend.position='bottom')
dev.off()

# Boxplot - Log
png('Explore/Boxplot_Log.png',width=6.5,height=8,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(GrabLong[!variable %in% tmp,],aes(x='',y=value))+
	geom_jitter(aes(color=as.factor(Year)),position=position_jitter(w=0.2,h=0))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	scale_y_continuous(trans='log10',labels=labelsPretty)+
	scale_color_viridis_d()+
	facet_wrap(~variable,ncol=3,scales='free_y')+
	labs(y='Value',x=NULL,title=NULL,color='Year')+
	theme_bw()+
	theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
	theme(text=element_text(size=txtSize))+
	theme(legend.position='bottom')
dev.off()

# No extreme values noted.

# Drop NAs
sum(is.na(GrabLong$value)) # 0 removed
GrabLong=GrabLong[!is.na(value),]
dim(GrabLong) # 1397 xx





###################################*
#### *** SiteYear *** ####
###################################*

# Note: There is no "SiteDate" for this project.

# Append Chla
chlaAll=rbindlist(list(chla2012,chla2013,chla2014),use.names=T,fill=T)
rm(chla2012,chla2013,chla2014)
chlaAll[,Year:=year(Date)]
dim(chlaAll) # 705 xx

# Pivot
chlaAll=dcast(chlaAll,SiteID+Year+Treatment+VialID~variable,
				  fun.aggregate=mean,value.var='value',fill=NA)
dim(chlaAll) # 705 xx

# Save chlaAvg (for calculating Response Ratios)
chlaAvg=copy(chlaAll)
chlaAvg[,variable:=paste0('Chla_',Treatment,'_Avg_mgM2')]
chlaAvg=dcast(chlaAvg,SiteID+Year~variable,fun.aggregate=mean,value.var='Chla_mgM2',fill=NA)
dim(chlaAvg) # 38 xx

# Append chem
chemAll=rbindlist(list(chem2012,chem2013,chem2014),use.names=T,fill=T)
rm(chem2012,chem2013,chem2014)
chemAll[,Year:=year(Date)]
dim(chemAll) # 692 xx

# Pivot
chemAll=dcast(chemAll,SiteID+Year~variable,
				  fun.aggregate=mean,value.var='value',fill=NA)
dim(chemAll) # 41 xx

# Join chem on to Chla
intersect(names(chlaAll),names(chemAll)) # SiteID Year
SiteYear=merge(chlaAll,chemAll,by=c('SiteID','Year'),all.x=T)
rm(chlaAll,chemAll)
dim(SiteYear) # 705 xx

# View missing data
# visdat::vis_miss(SiteYear,cluster=T)
# TKN data is missing for year 2014.




###################################*
#### Sites ####
###################################*

# Append sites
sitesPre=rbindlist(list(sites2012,sites2013,sites2014),use.names=T,fill=T)
rm(sites2012,sites2013,sites2014)
dim(sitesPre) # 39 xx

# Check for dups
sitesPre=unique(sitesPre)
dim(sitesPre)            # 39 xx
uniqueN(sitesPre$SiteID) # 39 - MUST match above!

# Check Year
sitesPre[,.N,by=Year]

# Fix non-breaking space
sitesPre[,StreamID:=gsub('\u00A0',' ',StreamID,fixed=T)] # Replace non-breaking spaces.
sitesPre[,StreamID:=trimws(StreamID)]
sitesPre[,sort(unique(StreamID))]

# Export to csv
setorder(sitesPre,-StreamID) # Makes SiteID alpha-numeric
fwrite(sitesPre,'../../GIS/SitesPre.csv')


#### Process in GIS





###################################*
#### Calculated Fields ####
###################################*

# Calculate response ratios (RRs)
chlaAvg[,NRR:=log(Chla_N_Avg_mgM2)/log(Chla_C_Avg_mgM2)]
chlaAvg[,PRR:=log(Chla_P_Avg_mgM2)/log(Chla_C_Avg_mgM2)]

# Join
intersect(names(SiteYear),names(chlaAvg)) # SiteID Year
SiteYear=merge(SiteYear,chlaAvg[,.(SiteID,Year,NRR,PRR)],by=c('SiteID','Year'),all.x=T) # Left outer join





###################################*
#### Join GIS ####
###################################*

# Load GIS LUTs
sites=copy(sitesPre)
sites[,':='(SiteDescription=NULL,Year=NULL)]
dim(sites) # 39 xx
sitesHUC08=fread('../../GIS/CSVs/SitesHUC08.csv',sep=',',header=T,skip=0,na.strings=na.strings,colClasses=c('character','character'))
dim(sitesHUC08) # 39 xx
sitesEco3=fread('../../GIS/CSVs/SitesEco3.csv',sep=',',header=T,skip=0,na.strings=na.strings)
dim(sitesEco3) # 39 xx

# Join to sites
sites=merge(sites,sitesHUC08,by=c('SiteID'),all.x=T) # Left outer join
sites=merge(sites,sitesEco3,by=c('SiteID'),all.x=T) # Left outer join

# Check for NAs
as.matrix(sapply(sites,function(x) sum(is.na(x))))
#          [,1]
# SiteID      0
# StreamID    0
# Lat         0
# Long        0
# SiteType   25 - OK
# HUC08       0
# Eco3ID      0

# Cleanup
rm(sitesHUC08,sitesEco3)



#### Join GIS
intersect(names(SiteYear),names(sites)) # SiteID
SiteYear=merge(SiteYear,sites,by='SiteID',all.x=T) # Left outer join




###################################*
#### Join StreamStats ####
###################################*

#### Load basinStats
basinStats=fread('../R_StreamStats/CSVs/BasinStats.csv',sep=',',header=T,skip=0,na.strings=na.strings)
dim(basinStats) # 1365 xx

# Filter fields
tmp=c('DRNAREA','BSLDEM10M','CSL1085LFP',
		'PRECIP','BASSTRMORD',
		'ELEV','MINBELEV','OUTLETELEV','ELEVMAX')
basinStats=basinStats[variable %in% tmp,]

# Rename variables
basinStats[,.N,by=variable]
basinStats[variable=='DRNAREA',    variable:='CatchmentArea_sqKm']          # Actual units are sqMi
basinStats[variable=='BSLDEM10M', variable:='Slope_AvgBasinwide_pct']       # Not modeled
basinStats[variable=='CSL1085LFP',variable:='Slope_Streamflow_ftMi']        # Yes modeled
basinStats[variable=='PRECIP',    variable:='Precip_AnnualAvgBasinwide_in'] # Yes modeled
basinStats[variable=='BASSTRMORD',variable:='StreamOrder']                  # Too many missing values
basinStats[variable=='ELEV',      variable:='Elev_AvgBasinwide_ft']         # Not modeled
basinStats[variable=='MINBELEV',  variable:='Elev_MinBasinwide_ft']         # Not modeled
basinStats[variable=='OUTLETELEV',variable:='Elev_Outlet_ft']               # Yes modeled
basinStats[variable=='ELEVMAX',   variable:='Elev_MaxBasinwide_ft']         # Not modeled
basinStats[,.N,by=variable]

# Pivot
basinStats=dcast(basinStats,SiteID~variable,fun.aggregate=mean,value.var='value',fill=NA)
dim(basinStats) # 39 xx

# Convert from sqMi to sqKm
basinStats[,CatchmentArea_sqKm:= CatchmentArea_sqKm*2.58999]

# Join
intersect(names(SiteYear),names(basinStats)) # SiteID
SiteYear=merge(SiteYear,basinStats,by=c('SiteID'),all.x=T) # Left outer join
rm(basinStats)



#### Load flowStats
flowStats=fread('../R_StreamStats/CSVs/FlowStats.csv',sep=',',header=T,skip=0,na.strings=na.strings)
dim(flowStats) # 1248 xx

# Filter fields
tmp=c('QA','M7D10Y','V7D10Y','PK10','PK100')
flowStats=flowStats[variable %in% tmp,]

# Rename variables
flowStats[,.N,by=variable]
flowStats[variable=='QA',    variable:='Flow_AnnualAvg_cfs']    # Yes modeled
flowStats[variable=='M7D10Y',variable:='Flow_7dayMin_10yr_cfs'] # Not modeled
flowStats[variable=='V7D10Y',variable:='Flow_7dayMax_10yr_cfs'] # Not modeled
flowStats[variable=='PK10',  variable:='Flow_Peak_10yr_cfs']    # Not modeled
flowStats[variable=='PK100', variable:='Flow_Peak_100yr_cfs']   # Not modeled
flowStats[,.N,by=variable]

# Pivot
flowStats=dcast(flowStats,SiteID~variable,fun.aggregate=mean,value.var='value',fill=NA)
dim(flowStats) # 39 xx

# Join
intersect(names(SiteYear),names(flowStats)) # SiteID
SiteYear=merge(SiteYear,flowStats,by=c('SiteID'),all.x=T) # Left outer join
rm(flowStats)





###################################*
#### Join Hemispherical Photos ####
###################################*

# See "DataProcessing/R_Light/Light_DataProcessing.rmd" for processing info.
# Use Canopy Openness, not leaf area index (LAI).
# 4 exposures were used. All 4 produced similar results. HDR exposure was selected.

# Load data
photos=fread('../R_Light/Output/HemiPhotoOutput.csv',sep=',',header=T,skip=0,na.strings=na.strings)
dim(photos) # 84 xx

# Add SiteID
photos[,SiteID:=sub('^.+, ','',Site)]

# Rename field
setnames(photos,'CanOpen','CanOpen_frac')

# Plot
png('Explore/Photos.png',width=5,height=4.0,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(photos,aes(x=Replicate,y=CanOpen_frac,color=Replicate))+
	geom_jitter(position=position_jitter(w=0.2,h=0))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	scale_y_continuous(trans=invLogit,labels=labelsPretty)+
	scale_color_viridis_d()+
	labs(y='Canopy Openness (frac)',x='Exposure',title=NULL)+
	theme_classic()+
	theme(text=element_text(size=txtSize))+
	theme(legend.position='bottom')
dev.off()
# Conclusion: Use HDR exposure.

# Filter for HDR
photos=photos[grep('HDR',Replicate),]
dim(photos) # 21 xx

# Filter fields
tmp=c('SiteID','CanOpen_frac')
photos=photos[,..tmp]

# Join
intersect(names(SiteYear),names(photos)) # SiteID
SiteYear=merge(SiteYear,photos,by=c('SiteID'),all.x=T) # Left outer join
rm(photos)




###################################*
#### Standardize Streams ####
###################################*

# Not needed




###################################*
#### Maps ####
###################################*

# Map
# library(magrittr)
map=leaflet::leaflet(sites) %>%
	setView(lng= -105.7,lat=38.9,zoom=7) %>%
	addProviderTiles(providers$Esri.WorldTopoMap) %>% # OpenStreetMap Esri.WorldImagery Esri.WorldTopoMap
	addMarkers(~Long,~Lat,popup=~SiteID,label=~SiteID)
map

# Export to png
mapview::mapshot(map,file='Maps/Map_Sites.png',vwidth=800,vheight=580)





###################################*
#### *** Aggregate *** ####
###################################*

# Note: There is no "Lake*" or "Stream*" for this project. 

# SiteYear_Avg (for SiteYearLong)
intersect(names(SiteYear),names(chlaAvg)) # SiteID Year
SiteYear_Avg=merge(SiteYear,chlaAvg[,.(SiteID,Year,
													Chla_C_Avg_mgM2,Chla_N_Avg_mgM2,Chla_P_Avg_mgM2,Chla_NP_Avg_mgM2)],
						 by=c('SiteID','Year'),all.x=T) # Left outer join

# Make SiteYear_Avg Unique
SiteYear_Avg[,':='(Treatment=NULL,VialID=NULL,Chla_mgM2=NULL)]
SiteYear_Avg=unique(SiteYear_Avg)
dim(SiteYear_Avg) # 38 xx

# SiteYearLong
SiteYearLong=melt(SiteYear_Avg,
						id.vars=c('SiteID','SiteType','StreamID','HUC08',
									 'Year'),
						na.rm=T,variable.factor=F)
SiteYearLong[,.N,by=variable]
dim(SiteYearLong) # 1162 xx

# Load varLong
varLong=fread('VariableLong.csv',sep=',',header=T,skip=0,na.strings=na.strings)
dim(varLong) # 32 xx

# Join varLong
intersect(names(SiteYearLong),names(varLong)) # variable
SiteYearLong=merge(SiteYearLong,varLong,by=c('variable'),all.x=T) # Left outer join
rm(varLong)





###################################*
#### Save ####
###################################*

# Sort
setorder(chlaAvg,SiteID,Year)
setorder(SiteYear,SiteID,Year)
setorder(SiteYear_Avg,SiteID,Year)
setorder(SiteYearLong,SiteID,Year,variable)

# Export to csv
fwrite(chlaAvg,'CSVs/ChlaAvg.csv')
fwrite(SiteYear,'CSVs/SiteYear.csv')
fwrite(SiteYear_Avg,'CSVs/SiteYear_Avg.csv')
fwrite(SiteYearLong,'CSVs/SiteYearLong.csv')


















