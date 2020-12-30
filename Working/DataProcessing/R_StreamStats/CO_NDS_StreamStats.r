
###################################*
#### StreamStats Processing ####
###################################*



###################################*
#### Contact Info ####
###################################*

# Mark Fernandez (mark.fernandez@tetratech.com)



###################################.
#### Citations ####
###################################.

citation('streamstats')
# Mark Hagemann. 2020. streamstats: R bindings to the USGS Streamstats API. R package version 0.0.2.
## GitHub page
# https://github.com/markwh/streamstats

## USGS StreamStats
# U.S. Geological Survey. 2016. The StreamStats program, online at https://streamstats.usgs.gov/ss/, accessed on Jan 1, 20XX.

## StreamStats URL:
# https://streamstats.usgs.gov/ss/                                - WEBSITE
# https://streamstatsags.cr.usgs.gov/ss_defs/basin_char_defs.aspx - BASIN VARIBLE LIST
# https://streamstatsags.cr.usgs.gov/ss_defs/flow_stat_defs.aspx  - FLOW VARIBLE LIST





###################################*
#### Acronyms ####
###################################*

# CRS - Coordinate Reference System




###################################*
#### Load Packages ####
###################################*

# Load packages
# options(digits=7L)
par(mar=c(4.5,4.5,1,1),bty='l')
# devtools::install_github('markwh/streamstats')
library(dplyr)       # computeFlowStats() requires dplyr :(
library(streamstats) # delineateWatershed()
library(data.table)  # [
# detach('package:aaa',unload=T)



# Clear workspace
rm(list=ls()); gc()


# Settings
na.strings=c(NA,'NA','N/A','#N/A','','None','<Null>','NULL')
crs=4326   # WGS84 = 4326
rcode='CO' # State abbr
setTimeout(60*2)




###################################*
#### *** Load Sites *** ####
###################################*

#### Load lat/longs
sites=fread('../../GIS/SitesPre.csv',sep=',',header=T,skip=0,na.strings=na.strings)
setorder(sites,SiteID)
dim(sites) # 39 xx

# Filter fields
tmp=c('SiteID','Lat','Long')
sites=sites[,..tmp]

# Check for dup SiteIDs
dim(sites)            # 39 xx
uniqueN(sites$SiteID) # 39 - MUST match above!

## Edit lat/longs that are near a confluence and require adjusting.
sites[SiteID=='9240',]
#    SiteID      Lat      Long
# 1:   9240 37.35361 -107.3246
sites[SiteID=='9240',':='(Lat=37.35367,Long= -107.32462)]
#
sites[SiteID=='12555',]
#    SiteID      Lat      Long
# 1:  12555 39.64167 -106.3067
sites[SiteID=='12555',':='(Lat=39.6416,Long= -106.3053)]
#
sites[SiteID=='10815',]
#    SiteID      Lat      Long
# 1:  10815 37.94972 -107.8686
sites[SiteID=='10815',':='(Lat=37.9486,Long= -107.8691)]
#
sites[SiteID=='12783B',]
#    SiteID      Lat      Long
# 1: 12783B 39.21797 -106.8546
sites[SiteID=='12783B',':='(Lat=39.2178,Long= -106.8549)]

# Check
which(sites$SiteID=='12783B') # 21



###################################*
#### Delineate Watershed ####
###################################*

# Note: Must disconnect VPN to access APIs.

# Initialize list
# ws=vector(mode='list',length=nrow(sites))
# names(ws)=sites$SiteID
length(ws) # 39
errors=rep(F,times=length(ws))

# Loop - Delineate Watersheds
tock=Sys.time()
i=21
for(i in 1:nrow(sites)){ # 1:nrow(sites)
	cat('\n',i,sites$SiteID[i],'\n')
	errors[i]=F
	tryCatch({
		ws[[sites[i,SiteID]]]=delineateWatershed(xlocation=sites[i,Long],ylocation=sites[i,Lat],crs=crs,includeparameters='true',includeflowtypes='true',includefeatures='true',simplify=T)
	},error=function(e) {errors[i]=T; cat('\nERROR: ',i,'\n')}
	) # END tryCatch
} # END for loop
tick=Sys.time()
tick-tock # 40 minutes for 39 watersheds

# Spot check watersheds
i=21
sites[i,]
leafletWatershed(ws[[i]])

# Check for errors - This code may not be working as intended.
sum(errors) # SHOULD be 0
isTRUE(errors)       # These indices were not delineated.
sites[errors,SiteID] # These SiteIDs were not delineated.

# Archive ws
# save(ws,file='ws.RData')

# Save workspaceIDs
tmp=sapply(ws,function(x) x[['workspaceID']])
df=data.table(SiteID=NA_character_,workspaceID=tmp)
df[,SiteID:=names(tmp)]

# Join to sites
sites[,workspaceID:=NULL]
intersect(names(sites),names(df)) # SiteID
sites=merge(sites,df,by=c('SiteID'),all.x=T)
sum(is.na(sites$workspaceID)) # MUST be 0

# Check to see if there are already .shp files present
tmp=list.files('../../GIS/SHPs',pattern='.shp')
length(tmp) # aaa

# Loop - Export shps
errors=rep(F,times=length(ws))
tock=Sys.time()
i=1L
for(i in 1:nrow(sites)){
	cat('\n',i,sites$SiteID[i],'\n')
	tryCatch({
		writeShapefile(ws[[sites[i,SiteID]]],layer=sites[i,SiteID],dir='../../GIS/SHPs',what='boundary')
	},error=function(e) {errors[i]=T; cat('\nERROR: ',i,'\n')}
	) # END tryCatch
} # END loop
tick=Sys.time()
tick-tock # 2 minutes for 39 watersheds

# Check for errors - This code may not be working as intended.
sum(errors) # SHOULD be 0
isTRUE(errors)       # These indices were not delineated.
sites[errors,SiteID] # These SiteIDs were not delineated.

# Check for missing shps
tmp=list.files('../../GIS/SHPs',pattern='.shp')
length(tmp) # SHOULD be 39
tmp1=sub('.shp$','',tmp)
tmp2=setdiff(sites$SiteID,tmp1)
which(sites$SiteID %in% tmp2) # Empty

# Spot check watersheds
i=1
leafletWatershed(ws[[i]])


###################################*
#### GIS Processing ####
###################################*

# GIS Processing Steps
# ====================.
# Add New Feature Dataset, "tmp"
# Load shp files into Data/tmp/
# MERGE into Data/StreamStats polygon
# Add Field - "SiteID" (text)
# Manually populate SiteID
# Data Driven Pages - Check watersheds
# Delete Data/tmp (but KEEP /SHPs folder)
# Export attribute table - "StreamStats.txt"
# Convert to csv





###################################*
#### Watershed Notes ####
###################################*

# SiteID Notes
# ============.
# 10118  - 
# 10231  - 
# 10232  - 
# 10240  - 
# 10267A - 
# 10268B - 
# 10283  - 
# 10322  - 
# 10324  - 
# 10329  - 
# 10767B - 
# 10769  - 
# 10770A - 
# 10814  - 
# 10815  - Upstream of Prospect Creek confluence
# 10818  - 
# 120    - 
# 12554A - 
# 12554B - 
# 12555  - 
# 12783B - 
# 12786B - 
# 12805B - 
# 12809  - 
# 12811  - 
# 12815  - 
# 12940  - 
# 12946  - 
# 138A   - 
# 5574   - 
# 5577   - 
# 5580   - 
# 9240   -  
# 9245   -  
# 9274   - 
# 9372   - 
# 9717   - 
# 9853   - 
# 9862   - 


###################################*
#### Basin Stats ####
###################################*

# Initialize list
# bs=vector(mode='list',length=nrow(sites))
# names(bs)=sites$SiteID
length(bs) # 39

# Loop - Basin Stats
tock=Sys.time()
i=1L
for(i in 1:nrow(sites)){
	cat('\n',i,sites$SiteID[i],'\n')
	tryCatch({
		bs[[sites[i,SiteID]]]=computeChars(sites[i,workspaceID],rcode=rcode,includeparameters=T)
	},error=function(e) {cat('\nERROR: ',i,'\n')}
	) # END tryCatch
} # END Loop
tick=Sys.time()
tick-tock # 19 minutes for 39 watersheds

# Check for nulls
tmp=lengths(basinStats)==0
sum(tmp)      # MUST BE 0! Else, go back into loop.
which(tmp==T) # These indices did not get processed.

# Archive bs
# save(bs,file='bs.RData')

# Process
basinStats=lapply(bs,function(x) x[['parameters']])
basinStats=lapply(basinStats,setDT)
basinStats=lapply(basinStats,function(x) x[,ID:=NULL])
names(basinStats)=sites$SiteID
dim(basinStats[[1]]) # 35 xx  # Is NOT supposed to be 39

# Append
basinStats=rbindlist(basinStats,use.names=T,fill=T,idcol=sites$SiteID)
tmp=c('SiteID','ParamName','ParamDescription','ParamCode','Unit','value')
cbind(names(basinStats),tmp)
names(basinStats)=tmp
dim(basinStats) # 1365 xx

# Rename fields
setnames(basinStats,'ParamCode','variable')

# Export to csv
fwrite(basinStats,'CSVs/BasinStats.csv')




###################################*
#### Flow Stats ####
###################################*

# NOTE: Currently not used. Need to find/create a variable LUT for these flow stats.

# Initialize list
# fs=vector(mode='list',length=nrow(sites))
# names(fs)=sites$SiteID
length(bs) # 39

# Loop - Flow Stats xxx
tock=Sys.time()
i=1L
for(i in 1:nrow(sites)){
	cat('\n',i,sites$SiteID[i],'\n')
	tryCatch({
		fs[[sites[i,SiteID]]]=computeFlowStats(sites[i,workspaceID],rcode=rcode,simplify=T)
	},error=function(e) {cat('\nERROR: ',i,'\n')}
	) # END tryCatch
	
} # END Loop
tick=Sys.time()
tick-tock # 21 minutes for 39 watersheds

# Check for nulls
tmp=lengths(fs)==0
sum(tmp)      # MUST BE 0! Else, go back into loop.
which(tmp==T) # These indices were not able to be processed.

# Archive fs
# save(fs,file='fs.RData')

# Process
flowStats=lapply(fs,setDT)
flowStats=lapply(flowStats,function(x) x[,group_id:=NULL])
dim(flowStats[[1]]) # 32 xx  # Is NOT supposed to be 39

# Append
flowStats=rbindlist(flowStats,use.names=T,fill=T,idcol=sites$SiteID)
tmp=c('SiteID','ParamName','ParamCode','ParamDescription','value','ParamGroup')
cbind(names(flowStats),tmp)
names(flowStats)=tmp
dim(flowStats) # 1248 xx

# Rename fields
setnames(flowStats,'ParamCode','variable')

# Export to csv
fwrite(flowStats,'CSVs/FlowStats.csv')



