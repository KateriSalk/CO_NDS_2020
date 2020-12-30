
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
# R Core Team. 2019. R: A language and environment for statistical computing. R Foundation for Statistical Computing,Vienna,Austria. URL https://www.R-project.org/.

citation('lme4')
# Douglas Bates, Martin Maechler, Ben Bolker, Steve Walker (2015). Fitting Linear Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1), 1-48. doi:10.18637/jss.v067.i01.

citation('emmeans')
# Russell Lenth. 2020. emmeans: Estimated Marginal Means, aka Least-Squares Means. R package version 1.5.2-1. https://CRAN.R-project.org/package=emmeans

# citation('multcomp') - Not used
# Torsten Hothorn, Frank Bretz and Peter Westfall. 2008. Simultaneous Inference in General Parametric Models. Biometrical Journal 50(3), 346--363.

## K-R Degrees of Freedom
# Kenward, M.G. and Roger, J.H. 1997. Small sample inference for fixed effects from restricted maximum likelihood. Biometrics, 983-997.

## Logit Transformation - [0,1]
# M. Smithson and J. Verkuilen. 2006. A Better Lemon Squeezer? Maximum-Likelihood Regression With Beta-Distributed Dependent Variables. Psychological Methods 11(1), 54-71. DOI: 10.1037/1082-989X.11.1.54.


## Post-Hoc Multiple Comparisons Resources
# https://stats.stackexchange.com/questions/237512/how-to-perform-post-hoc-test-on-lmer-model

## FDR (BH)
# Benjamini, Y., and Hochberg, Y. 1995. Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society Series B, 57, 289–300. http://www.jstor.org/stable/2346101.

## Holm
# Holm, S. 1979. A simple sequentially rejective multiple test procedure. Scandinavian Journal of Statistics, 6, 65–70. http://www.jstor.org/stable/4615733.

## Dunnett
# Charles W. Dunnett. 1955. A multiple comparison procedure for comparing several treatments with a control. Journal of the American Statistical Association, 50(272):1096–1121.

## Tukey HSD
# Tukey, John. 1949. "Comparing Individual Means in the Analysis of Variance". Biometrics. 5 (2): 99–114. JSTOR 3001913.





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
# library(openxlsx)    # read.xlsx()
library(data.table)   # [
library(ggplot2)      # ggplot()
library(viridisLite)  # 
library(lattice)      # dotplot()
library(lme4)         # lmer()
library(lmerTest)     # lmer()
library(RLRsim)       # exactRLRT()
# library(multcomp)     # cld()
library(emmeans)      # emmeans()
# library(latex2exp)    # TeX()
# detach('package:aaa',unload=T)


# Clean workspace
rm(list=ls()); gc()

# Set contrasts - Not used
# options(contrasts=c('contr.treatment','contr.poly')) # R default
# options(contrasts=c('contr.sum','contr.poly'))       # Not Used
# Since there is a "reference" level ("Control"), do NOT set to "contr.sum".

# Number of cores
parallel::detectCores()  # 12 cores
future::availableCores() # 12 cores
ncpus=6L

# Settings
na.strings=c(NA,'NA','N/A','#N/A','','None','<Null>','.','Not Analyzed')
seed=27709L
pointsize=9L
txtSize=9L
end=0.8 # viridis
xTicks=yTicks=c(0.1,0.3,1,3,10,30,100,300,1000,3000,10000,30000,100000)
xMinorTicks=yMinorTicks=c(seq(0.2,0.9,by=0.1),
								  seq(2,9,by=1),
								  seq(20,90,by=10),
								  seq(200,900,by=100),
								  seq(2000,9000,by=1000),
								  seq(20000,90000,by=1000))



#### Source functions
source('../R_Functions/labelsPretty.r')
# source('../R_Functions/inclusive.r')
# source('../R_Functions/invLogit.r')
# source('../R_Functions/mode.r')




###################################*
#### Load Data ####
###################################*

# SiteYear_Avg (for plotting)
SiteYear_Avg=fread('../DataProcessing/R_DataProcessing/CSVs/SiteYear_Avg.csv',sep=',',skip=0,header=T,check.names=T,na.strings=na.strings)
dim(SiteYear_Avg) # 38 xx

# SiteYear
SiteYear=fread('../DataProcessing/R_DataProcessing/CSVs/SiteYear.csv',sep=',',skip=0,header=T,check.names=T,na.strings=na.strings)
dim(SiteYear) # 705 xx

# SiteYearLong
SiteYearLong=fread('../DataProcessing/R_DataProcessing/CSVs/SiteYearLong.csv',sep=',',skip=0,header=T,check.names=T,na.strings=na.strings)
dim(SiteYearLong) # 1236 xx

# Create SiteYearLong_Chla (for plotting)
SiteYearLong_Chla=melt(SiteYear,
							  id.vars=c('SiteID','SiteType','VialID',
							  			 'StreamID','HUC08',
							  			 'Treatment','Chla_mgM2',
							  			 'Year'),
							  na.rm=T,variable.factor=F)
SiteYearLong_Chla[,.N,by=variable]
dim(SiteYearLong_Chla) # 20055 xx

# Add TreatmentLong - Not done
# SiteYear[,TreatmentLong:=NA_character_]
# SiteYear[Treatment=='C', TreatmentLong:='Control']
# SiteYear[Treatment=='N', TreatmentLong:='Nitrogen']
# SiteYear[Treatment=='P', TreatmentLong:='Phosphorus']
# SiteYear[Treatment=='NP',TreatmentLong:='Nitro + Phosp']



#### Convert to factors
# SiteID
SiteYear[,SiteID:=factor(SiteID)]
# Treatment - SiteYear
SiteYear[,Treatment:=factor(Treatment,levels=c('C','N','P','NP'))]
SiteYear[,Treatment:=relevel(Treatment,'C')]
# Treatment - SiteYearLong
SiteYearLong_Chla[,Treatment:=factor(Treatment,levels=c('C','N','P','NP'))]
SiteYearLong_Chla[,Treatment:=relevel(Treatment,'C')]
# SiteType - SiteYear
SiteYear[,SiteType:=factor(SiteType,levels=c('Undeveloped','Upstream','Downstream'),ordered=F)] # Do NOT set to ordered
SiteYear[,SiteType:=relevel(SiteType,'Undeveloped')]
# SiteType - SiteYearLong
SiteYearLong_Chla[,SiteType:=factor(SiteType,levels=c('Undeveloped','Upstream','Downstream'),ordered=F)]
SiteYearLong_Chla[,SiteType:=relevel(SiteType,'Undeveloped')]

# Check for zeros
as.matrix(sapply(SiteYear,function(x) sum(x==0,na.rm=T)))

# Transform variables
SiteYear[,logChla_mgM2:=log(Chla_mgM2)]
SiteYear[,logCatchmentArea_sqKm:=log(CatchmentArea_sqKm)]
# SiteYear[,logSlope_AvgBasinwide_pct:=log(Slope_AvgBasinwide_pct)] # Not needed
SiteYear[,logElev_Outlet_ft:=log(Elev_Outlet_ft)]
SiteYear[,logTN_mgL:=log(TN_mgL)]
SiteYear[,logTP_mgL:=log(TP_mgL)]
# SiteYear[,logPrecip_AnnualAvgBasinwide_in:=log(Precip_AnnualAvgBasinwide_in)] # Not needed
SiteYear[,logFlow_AnnualAvg_cfs:=log(Flow_AnnualAvg_cfs)]
SiteYear[,logNRR:=log(NRR)]
SiteYear[,logPRR:=log(PRR)]
# SiteYear[,logitCanOpen_frac:=qlogis(CanOpen_frac)] # Not needed
SiteYear[,logNP_massRatio:=log(NP_massRatio)]
SiteYear[,logNP_molarRatio:=log(NP_molarRatio)]




###################################*
### Summary Stats ####
###################################*

# Sites (Table 2)
tmp=SiteYear[,.N,by=.(Year,StreamID,SiteID,SiteType,Lat,Long)]
tmp=unique(tmp)
dim(tmp) # 38 xx
setorder(tmp,Year,StreamID,SiteID)
fwrite(tmp,'SumStats/Sites.csv')


# Variables of interest
vars=c('Chla_C_Avg_mgM2','Chla_NP_Avg_mgM2','Chla_N_Avg_mgM2','Chla_P_Avg_mgM2', # Chla
		 'CanOpen_frac','WaterTemp_C','TN_mgL','TP_mgL','NRR','PRR','NP_massRatio','NP_molarRatio', # Other
		 # StreamStats:
		 'CatchmentArea_sqKm','Elev_Outlet_ft','Flow_AnnualAvg_cfs','Slope_AvgBasinwide_pct')

# Summary Stats - Overall
tmp=SiteYearLong[variable %in% vars,
					  .(Count=.N,
					    Min=min(value),
					    P10=quantile(value,probs=0.10),
					    P25=quantile(value,probs=0.25),
					    Median=median(value),
					    GeoMean=psych::geometric.mean(value),
					    SD=sd(value),
					    P75=quantile(value,probs=0.75),
					    P90=quantile(value,probs=0.90),
					    Max=max(value)),
					  by=.(varLong)]
dim(tmp) # 16 xx
setorder(tmp,varLong)
fwrite(tmp,'SumStats/SumStats.csv')





###################################*
### Scatterplots ####
###################################*

# Compare Chla
png('Scatterplots/Chla_Pairs.png',width=6.5,height=6.5,units='in',res=300,type='cairo',pointsize=pointsize)
par(mar=c(4.5,4.5,2,1))
pairs(SiteYear_Avg[,.(Chla_C_Avg_mgM2,Chla_N_Avg_mgM2,Chla_P_Avg_mgM2,Chla_NP_Avg_mgM2)],log='xy',main='Compare Chlorophyll (Site-Treatment Avgs)')
par(mfrow=c(1,1))
dev.off()

# Compare Slopes
png('Scatterplots/Slopes.png',width=5,height=4,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(SiteYear,aes(x=Slope_AvgBasinwide_pct,y=Slope_Streamflow_ftMi))+
	geom_point()+
	geom_smooth(method='lm',se=F,color=4)+
	scale_y_continuous(trans='log10',labels=labelsPretty)+
	scale_x_continuous(trans='identity',labels=labelsPretty)+
	labs(title='Compare Slopes')+
	theme_classic()
dev.off()
# Decision: Use "Slope_Streamflow_ftMi" as it more reflects the stream.

# Compare Elevations
png('Scatterplots/Elevation_Pairs.png',width=5,height=5,units='in',res=300,type='cairo',pointsize=pointsize)
par(mar=c(4.5,4.5,2,1))
pairs(SiteYear_Avg[,.(Elev_AvgBasinwide_ft,Elev_Outlet_ft,Elev_MaxBasinwide_ft)],log='xy',main='Compare Elevations')
par(mfrow=c(1,1))
dev.off()
# Use "Elev_Outlet_ft" as that represents the SiteID

# Compare Flows
png('Scatterplots/Flows_Pairs.png',width=6.5,height=6.5,units='in',res=300,type='cairo',pointsize=pointsize)
par(mar=c(4.5,4.5,2,1))
pairs(SiteYear[,.(Flow_AnnualAvg_cfs,Flow_7dayMin_10yr_cfs,Flow_7dayMax_10yr_cfs,Flow_Peak_10yr_cfs,Flow_Peak_100yr_cfs)],log='xy',main='Compare Flows')
dev.off()
# Decision: Use "Flow_AnnualAvg_cfs"

# Compare Nutrients
png('Scatterplots/Nutrients_Pairs.png',width=6.5,height=6.5,units='in',res=300,type='cairo',pointsize=pointsize)
par(mar=c(4.5,4.5,2,1))
pairs(SiteYear[,.(TN_mgL,TP_mgL,NRR,PRR)],log='xy',main='Compare Nutrients')
dev.off()

# Compare StreamStats
png('Scatterplots/StreamStats.png',width=6.5,height=6.5,units='in',res=300,type='cairo',pointsize=pointsize)
par(mar=c(4.5,4.5,2,1))
pairs(SiteYear[,.(CatchmentArea_sqKm,Slope_Streamflow_ftMi,Slope_AvgBasinwide_pct,Precip_AnnualAvgBasinwide_in,Flow_AnnualAvg_cfs,Elev_Outlet_ft)],log='xy',main='StreamStats')
dev.off()
# "CatchmentArea_sqKm" is a good proxy for slope, flow, and elev, and ok for precip.
# We have missing slope_Stream, flow, and precip data. Use Catchment to increase sample size.

# Precip vs Flow
png('Scatterplots/PrecipFlow.png',width=5,height=4,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(SiteYear,aes(x=Flow_AnnualAvg_cfs,y=Precip_AnnualAvgBasinwide_in))+
	geom_point()+
	geom_smooth(method='lm',se=F,color=4)+
	scale_y_continuous(trans='identity',labels=labelsPretty)+
	scale_x_continuous(trans='log10',labels=labelsPretty)+
	labs(y='Avg Annual Precip (in)',x='Avg Annual Flow (cfs)',title=NULL)+
	theme_classic()
dev.off()


# Scatterplot - Colored by Treatment
tmp1=c('CatchmentArea_sqKm','Slope_Streamflow_ftMi','Elev_Outlet_ft','WaterTemp_C','Flow_AnnualAvg_cfs')
png('Scatterplots/Scatterplots_1.png',width=5,height=6.5,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(SiteYearLong_Chla[variable %in% tmp1,],aes(x=value,y=Chla_mgM2))+
	geom_point(aes(color=as.factor(Year)))+
	geom_smooth(method='lm',se=F,color=1)+
	facet_wrap(~variable,ncol=2,scales='free_x')+
	scale_y_continuous(trans='log10',labels=labelsPretty)+
	scale_x_continuous(trans='log10',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y=expression(Chlorophyll~~mg/m^2),x='Value',title=NULL,color='Year')+
	theme_bw()+
	theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
	theme(text=element_text(size=txtSize))+
	theme(legend.position='bottom')
dev.off()

tmp2=c('TN_mgL','TP_mgL','NRR','PRR','CanOpen_frac')
png('Scatterplots/Scatterplots_2.png',width=5,height=4.5,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(SiteYearLong_Chla[variable %in% tmp2,],aes(x=value,y=Chla_mgM2))+
	geom_point(aes(color=as.factor(Year)))+
	geom_smooth(method='lm',se=F,color=1)+
	facet_wrap(~variable,ncol=2,scales='free_x')+
	scale_y_continuous(trans='log10',labels=labelsPretty)+
	scale_x_continuous(trans='log10',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y=expression(Chlorophyll~~mg/m^2),x='Value',title=NULL,color='Year')+
	theme_bw()+
	theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
	theme(text=element_text(size=txtSize))+
	theme(legend.position='bottom')
dev.off()

# Boxplot, by SiteID
tmp=sort(unique(SiteYear$SiteID))
tmp=tmp[1:20]
ylim=range(SiteYear$Chla_mgM2)
labels=c(1,3,10,30,100,300)

# 1
png('Scatterplots/Boxplot_SiteIDs_1.png',width=8,height=10.5,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(SiteYear[SiteID %in% tmp,],aes(x=Treatment,y=Chla_mgM2,color=Treatment))+
	geom_jitter(position=position_jitter(w=0.2,h=0))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	facet_wrap(~SiteID,ncol=4,scales='fixed')+
	scale_y_continuous(trans='log10',breaks=labels,labels=labels,limits=ylim)+
	scale_color_viridis_d(end=end)+
	labs(y=expression(Chlorophyll~~mg/m^2),x='Treatments',title=NULL,color='Treatment')+
	theme_bw()+
	theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
	theme(text=element_text(size=9))+
	theme(legend.position='bottom')
dev.off()

# 2
png('Scatterplots/Boxplot_SiteIDs_2.png',width=8,height=10.5,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(SiteYear[!SiteID %in% tmp,],aes(x=Treatment,y=Chla_mgM2,color=Treatment))+
	geom_jitter(position=position_jitter(w=0.2,h=0))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	facet_wrap(~SiteID,ncol=4,scales='fixed')+
	scale_y_continuous(trans='log10',breaks=labels,labels=labels,limits=ylim)+
	scale_color_viridis_d(end=end)+
	labs(y=expression(Chlorophyll~~mg/m^2),x='Treatments',title=NULL,color='Treatment')+
	theme_bw()+
	theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
	theme(text=element_text(size=9))+
	theme(legend.position='bottom')
dev.off()




###################################*
#### Map ####
###################################*

# Check map
# library(magrittr)
leaflet::leaflet(SiteYear[,.(SiteID,Lat,Long)]) %>%
	setView(lng= -105.7,lat=38.9,zoom=7) %>%
	addProviderTiles(providers$Esri.WorldTopoMap) %>% # OpenStreetMap Esri.WorldImagery Esri.WorldTopoMap
	addMarkers(~Long,~Lat,popup=~SiteID,label=~SiteID)




###################################*
#### *** Text *** ####
###################################*

# 4 Models were run:
# ===================================.
# Main Model            - All years & "!SiteType %in% Downstream"
# Longitudinal Model    - 2013 & "!is.na(SiteType)"
# Light Model           - 2014
# Response Ratio Models - All years


## Hierarchical Models were used instead of glm/ANOVA models for these reasons:
#   - To account for unbalanced design
#   - To account for repeated measures (replicates)
# See https://en.wikipedia.org/wiki/Multilevel_modeling_for_repeated_measures#Multilevel_Modeling_versus_RM-ANOVA




###################################*
#### Post-Hoc Text ####
###################################*

# When testing multiple hypotheses as in a post-hoc ANOVA analysis, we want to control the error of rejecting a true null hypothesis (Type I error; alpha=0.05) for all of the hypotheses combined. This is known as controling the family-wise error rate (FWER). Applying  separate t-tests to compare sampling methods does not control for the family-wise error rate. Therefore the resulting pvalues would be artifically low, increasing the ikelihood of rejecting a true null hypothesis (ie committing a Type I error).

# Various "multiple comparison" tests that control for the FWER have been proposed. Two common ones are the Tukey-Kramer Honest Significant Differences (HSD) test and Dunnett's test. The Tukey-Kramer test is appropriate when all pairwise comparisons are of interest. Dunnett's test is appropriate when there is a baseline or "gold-standard" method that is being compared against. Hommell (cite) is often used when neither a baseline or all pairwise comparisions are of interest.

# lmerTest::difflsmeans() can NOT adjust for multiple comparisons. DO NOT USE!

# multicomp::glht() must have ddf set MANUALLY Otherwise glht() assumes asymptotic z-score. Could be an issue on smaller datasets.

#### emmeans contrast coding resource:
# https://aosmith.rbind.io/2019/04/15/custom-contrasts-emmeans/#the-contrast-function-for-custom-comparisons

# emmeans settings:
# ================.
# "trt.vs.ctrl" => Dunnett comparisons
# "pairwise"    => Tukey comparisons

# Check P-adjustment methods:
# ?p.adjust

#### Rough order of p-value adjustments:
# Most Conservative  ---->  Least Conservative
# bonferroni, sidak >> holm, hochberg, hommel >> fdr >> none
# Use "hommel" as it is considered more powerful than holm and hockberg. See Shaffer 1995.





###################################*
#### Sum of Squares Text ####
###################################*

#### ANOVA Sum of Squares (SS):
#### fit = aov(Y ~ A + B + A:B)

### Type I SS:
# SS(A)
# SS(B)
# SS(A:B|A,B)

### Type II SS:
# SS(A|B)
# SS(B|A)
# SS(A:B|A,B)

### Type III SS:
# SS(A|B,A:B)
# SS(B|A,A:B)
# SS(A:B|A,B)

# See https://stats.stackexchange.com/a/20455/26327


# Highlights
# ===========.
# Pvalue for interaction term is same, regardless of SS Type or dummy coding.
# Pvalues for Type II and Type III are same for models w/o interaction terms.
# Only Type III is affected by dummy coding scheme :(
# Type I isn't what anyone wants.
# Type III makes sense if the interaction is significant. But main effect Pvalues will also depend on dummy coding used :(  Instead, use graphics describing what is going on if the interaction is significant.
# Type II makes sense if the interaction is NS. But instead, just re-run the model as main effects only.
# R aov() and lm() don't care if the predictor is a factor or character. Post-hoc tests do, however.


# SS Defaults
# =======================.
# anova()      # Type I
# car::Anova() # Type II
# SAS          # Type III




###################################*
### *** Main Model *** ####
###################################*

# Main Model
# =========.
# Use all available years.
# Remove Downstream 2013 sites.


#### Dataset
# Subset Data
df=copy(SiteYear[!SiteType %in% 'Downstream',])
df[,.N,by=SiteType]
dim(df) # 626 xx

# TEST to see if these sites influence the significant predictors. They do NOT.
# tmp=c('12811','5580','9245') # Not used
# df=df[!SiteID %in% tmp,]     # Not used

# Drop levels
droplevels(df$SiteID)

# Check for missing data
# visdat::vis_miss(df,cluster=T)

# Fields with missing data
as.matrix(sapply(df,function(x) sum(is.na(x))))



#### Model
library(lmerTest)                   # Load/Unload as needed
# detach('package:lmerTest',unload=T) # Load/Unload as needed
rm(fit)
fit=lmer(logChla_mgM2~1+
				Treatment+               # Retain
				WaterTemp_C+             # Significant 
				# logTN_mgL+             # Remove 2
				# logTP_mgL+             # Remove 4
				## StreamStat
				# logCatchmentArea_sqKm+ # Remove 3
				Slope_AvgBasinwide_pct+  # Significant 
				# logElev_Outlet_ft+     # Remove 1
				# logFlow_AnnualAvg_cfs+ # Remove 5
				## Random Effects:
				(1|SiteID),              # Retain
			data=df,REML=T)
summary(fit,ddf='Kenward-Roger')
isSingular(fit) # SHOULD BE FALSE

# Test if "SiteID" is significant
exactRLRT(m=fit,nsim=10000,ncpus=ncpus,parallel='multicore',seed=seed)
# RLRT = 185.04, p-value < 2.2e-16
# SiteID IS significant.

# Variance Summary
VarCorr(fit)
# Groups   Name        Std.Dev.
# SiteID   (Intercept) 0.47839 
# Residual             0.63037 

# ANOVA Summary
out1=anova(fit,type='2',ddf='Kenward-Roger') # Kenward-Roger Satterthwaite
out1
#                         Sum Sq Mean Sq NumDF  DenDF  F value  Pr(>F)    
# Treatment              201.673  67.224     3 589.73 169.1764 < 2e-16 ***
# WaterTemp_C              2.010   2.010     1  30.66   5.0593 0.03182 *  
# Slope_AvgBasinwide_pct   1.736   1.736     1  30.95   4.3683 0.04492 * 

# Coefficients Summary
fit # n=626; SiteID=34
out=summary(fit,ddf='Kenward-Roger') # Kenward-Roger Satterthwaite
out
# Random effects:
# Groups   Name        Variance Std.Dev.
# SiteID   (Intercept) 0.2289   0.4784  
# Residual             0.3974   0.6304  
# Number of obs: 626, groups:  SiteID, 34
# 
# Fixed effects:
#                          Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)              2.652643   0.663111  31.030571   4.000 0.000365 ***
# TreatmentN               0.856319   0.071430 589.681423  11.988  < 2e-16 ***
# TreatmentP              -0.427099   0.071393 590.024942  -5.982 3.82e-09 ***
# TreatmentNP              0.898912   0.072042 589.398722  12.478  < 2e-16 ***
# WaterTemp_C              0.066480   0.029556  30.663783   2.249 0.031822 *  
# Slope_AvgBasinwide_pct  -0.016831   0.008053  30.948100  -2.090 0.044923 * 

# Export to csv
write.csv(out$coefficients,file='Model_Main/Coefs_Pvalues.csv')
write.csv(as.data.frame(out1),file='Model_Main/ANOVA_Pvalues.csv')

# Yhat, resids, R2
df[,yhat:=predict(fit,type='response',re.form=NULL)] # Include ALL random effects
df[,resids:=residuals(fit,type='deviance',scaled=F)]
df[,stdResids:=residuals(fit,type='deviance',scaled=T)]
df[,leverage:=cooks.distance(fit)] # influence(fit) takes way too long
R2=cor(df$logChla_mgM2,df$yhat,method='pearson')^2
R2 # 0.6558243

# Diagnostic Plots
png('Model_Main/Diagnostics.png',width=6.5,height=5.5,units='in',res=300,type='cairo',pointsize=pointsize)
par(mfrow=c(2,2),mar=c(4.5,4.5,2,1))
# Resids vs Fitted Plot
scatter.smooth(df$resids~df$yhat,span=0.7,lpars=list(col=4,lwd=2),xlab='Linear Predictor',ylab='Residuals',main='Residuals vs Fitted')
abline(h=0)
# Model
qqnorm(df$stdResids,ylab='Std. Residuals',main='Normal Q-Q')
qqline(df$stdResids) # Slight right-skew
# Location-Scale Plot
scatter.smooth(sqrt(abs(df$stdResids))~df$yhat,span=0.7,lpars=list(col=4,lwd=2),xlab='Linear Predictor',ylab='sqrt(abs(Std. Residuals))',main='Scale-Location')
# Leverage Plot - Possible, but not intuitive
par(mfrow=c(1,1))
dev.off()

# Explore extreme values
qqnorm(df$stdResids)
# identify(qqnorm(df$stdResids))

plot(logChla_mgM2~Treatment,data=df)
# identify(df$logChla_mgM2~df$Treatment)
tmp=c(490,531,532,533) # NP boxplot outliers
tmp1=c(371,526)        # N boxplot outliers
tmp=c(tmp,tmp1)
df[tmp,.N,by=SiteID]    # 12811 5580  9245
df[tmp,.N,by=HUC08]     # 10190005 14050001 14080102
df[tmp,.N,by=SiteType]  # NA Upstream
# Note: These three sites showed up as extreme values in the boxplots: 12811, 5580, 9245.
# However, they did not influence which parameters were significant. Therefore they were retained.
# END Explore extreme values

# Residuals by Boxplot
png('Model_Main/ResidualsByTreatment.png',width=5,height=4.3,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(df,aes(x=Treatment,y=resids))+
	geom_hline(yintercept=0,color='grey')+
	geom_jitter(aes(color=as.factor(Year)),position=position_jitter(w=0.2,h=0))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	scale_y_continuous(trans='identity',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y='Residuals',x=NULL,title=NULL,color='Year')+
	theme_classic()+
	theme(legend.position='bottom')
dev.off()

# Random intercepts (plus SE - NOT CIs!) 
tmp=ranef(fit,condVar=T)
png('Model_Main/RE_SiteID.png',width=5,height=7.0,units='in',res=300,type='cairo',pointsize=pointsize) # Doesn't respect pointsize
p=dotplot(tmp,main=F,
			 scales=list(y=list(cex=1),x=list(cex=1)),
			 par.strip.text=list(cex=1))
print(p$`SiteID`)
dev.off()

# 1:1 plot
xlim=ylim=range(c(df$logChla_mgM2,df$yhat))
png('Model_Main/_1to1.png',width=4,height=4, units='in',res=300,type='cairo',pointsize=pointsize)
par(pty='s',mar=c(4.5,4.5,1,1))
plot(logChla_mgM2~yhat,data=df,axes=F,ylab=expression(Observed~Chlorophyll~~mg/m^2),xlab=expression(Observed~Chlorophyll~~mg/m^2),xlim=xlim,ylim=ylim,main=NULL)
rug(log(xMinorTicks),ticksize= -0.01,col='darkgrey')
axis(1,at=log(xTicks),labels=xTicks)
rug(log(xMinorTicks),side=2,ticksize= -0.01,col='darkgrey')
axis(2,at=log(xTicks),labels=xTicks)
abline(0,1)
legend('topleft',legend=sprintf('R2 = %0.2f',R2),pch=NA,inset=0.01,bty='o',merge=F) 
box()
par(pty='m')
dev.off()





###################################*
### Post-Hoc Analysis ####
###################################*

# Load contrasts
contrast_M=fread('Contrasts_Main.csv',sep=',',skip=0,header=T,check.names=T,na.strings=na.strings)
dim(contrast_M) # 4 xx

# Estimated Marginal Means
rm(postHoc_Means)
postHoc_Means=emmeans(fit,specs= ~Treatment,lmer.df='kenward-roger',adjust='tukey')
postHoc_Means
# Treatment emmean     SE   df lower.CL upper.CL
# C           3.18 0.0970 49.1     2.93     3.43
# N           4.04 0.0966 48.3     3.79     4.29
# P           2.76 0.0964 48.0     2.51     3.01
# NP          4.08 0.0970 49.2     3.83     4.33
#
contrast_M[,.N,by=Level] # The order MUST match emmeans output above!

# Post-Hoc Contrasts
rm(postHoc_pValues)
postHoc_pValues=contrast(postHoc_Means,method=list(
	'N - C' =contrast_M$N -contrast_M$C,
	'P - C' =contrast_M$P -contrast_M$C,
	'NP - C'=contrast_M$NP-contrast_M$C,
	'NP - N'=contrast_M$NP-contrast_M$N,
	'NP - P'=contrast_M$NP-contrast_M$P,
	'N - P' =contrast_M$N -contrast_M$P),
	lmer.df='kenward-roger',adjust='tukey')
postHoc_pValues
# contrast estimate     SE  df t.ratio p.value
# N - C      0.8563 0.0714 590 11.988  <.0001 
# P - C     -0.4271 0.0714 590 -5.982  <.0001 
# NP - C     0.8989 0.0720 589 12.478  <.0001 
# NP - N     0.0426 0.0716 590  0.595  0.9919 
# NP - P     1.3260 0.0714 590 18.562  <.0001 
# N - P      1.2834 0.0707 590 18.150  <.0001
#
# Actual pvalues using Tukey:
pairs(postHoc_Means)

# Letters
tmp=multcomp::cld(postHoc_Means,Letter=letters)
tmp
# Treatment emmean     SE   df lower.CL upper.CL .group
# P           2.76 0.0964 48.0     2.51     3.01  a    
# C           3.18 0.0970 49.1     2.93     3.43   b   
# N           4.04 0.0966 48.3     3.79     4.29    c  
# NP          4.08 0.0970 49.2     3.83     4.33    c 
#
groups=c('b','a','c','a') # Ordered for Figure

# Export
fwrite(tmp,'Model_Main/PostHoc_Groups.csv')

# Matrix
pwpm(postHoc_Means)
#          C       N       P     NP
# C   [3.18]  <.0001  <.0001 <.0001
# N  -0.8563  [4.04]  <.0001 0.9336
# P   0.4271  1.2834  [2.76] <.0001
# NP -0.8989 -0.0426 -1.3260 [4.08]

# CIs - Do NOT use CIs to determine significance. Use p.value instead.
# set.seed(seed)
postHoc_CIs=confint(postHoc_pValues,level=0.95,adjust='tukey') # mvt bonferroni # "hommel" is not an option for CIs.
postHoc_CIs
# contrast estimate     SE  df lower.CL upper.CL
# N - C      0.8563 0.0714 590    0.668    1.045
# P - C     -0.4271 0.0714 590   -0.616   -0.239
# NP - C     0.8989 0.0720 589    0.709    1.089
# NP - N     0.0426 0.0716 590   -0.146    0.232
# NP - P     1.3260 0.0714 590    1.137    1.515
# N - P      1.2834 0.0707 590    1.097    1.470

# Export
fwrite(as.data.frame(postHoc_pValues),'Model_Main/PostHoc_Table.csv')
fwrite(as.data.frame(pairs(postHoc_Means)),'Model_Main/PostHoc_Table_Tukey.csv')
fwrite(as.data.frame(postHoc_CIs),'Model_Main/PostHoc_CIs.csv')

# Plot CIs
png('Model_Main/PostHoc_Plot.png',width=5,height=4.0,units='in',res=300,type='cairo',pointsize=pointsize)
plot(postHoc_CIs)+
	geom_vline(xintercept=0,color='grey',size=1,linetype='dashed')+
	labs(y='Treatment Comparisons',x='Log Treatment Difference (mg/m2)',title='Post-Hoc Treatment Comparisons (95% CI)')+
	theme_classic()+
	theme(text=element_text(size=txtSize))
dev.off()

# Boxplots with Post-Hoc groupings xxx
png('Model_Main/Treatments.png',width=5,height=4.3,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(SiteYear[!SiteType %in% 'Downstream',],aes(x=Treatment,y=Chla_mgM2))+
	geom_jitter(aes(color=as.factor(Year)),position=position_jitter(w=0.2,h=0))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	# Extra. SiteID 9245 "Stollsteimer Creek" had high turbidity, relative to the other sites (see email from Blake, 12/29/2020)
	# geom_point(data=SiteYear[SiteID=='9245',],fill='orange',shape=23,size=3)+
	scale_y_continuous(trans='log10',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y=expression(Chlorophyll~~mg/m^2),x=NULL,title=NULL,color='Year')+
	theme_classic()+
	theme(legend.position='bottom')+
	stat_summary(geom='text',fun=function(x) max(x)+0.1,size=4,label=groups,color=1) 
dev.off()





###################################*
### *** Longitudinal Model *** ####
###################################*

# Longitudinal Model uses year 2013 only.

#### Dataset
# Subset Data
df=copy(SiteYear[!is.na(SiteType),])
dim(df) # 270 xx

# Drop levels
droplevels(df$SiteID)

# Check SiteType
df[,.N,by=SiteType]
#       SiteType   N
# 1:  Downstream  79
# 2:    Upstream  91
# 3: Undeveloped 100

# Check for missing data
# visdat::vis_miss(df,cluster=T)




#### Model
# Fit
library(lmerTest)                   # Load/Unload as needed
# detach('package:lmerTest',unload=T) # Load/Unload as needed
rm(fit)
fit=lmer(logChla_mgM2~1+
				SiteType+                # Retain
				Treatment+               # Retain
				SiteType:Treatment+      # Significant
				# WaterTemp_C+           # Remove 6
				# logTN_mgL+             # Remove 5
				# logTP_mgL+             # Remove 3
				## StreamStat:
				# logCatchmentArea_sqKm+ # Remove 2
				Slope_AvgBasinwide_pct+  # Significant 
				# logElev_Outlet_ft+     # Remove 1
				# logFlow_AnnualAvg_cfs+ # Remove 4
				## Random Effect:
				(1|SiteID),              # Retain
			data=df,REML=T)
anova(fit,type='2',ddf='Kenward-Roger')
isSingular(fit) # SHOULD BE FALSE

# Test if "SiteID" is significant
exactRLRT(fit,nsim=10000,ncpus=ncpus,parallel='multicore',seed=seed)
# RLRT = 78.958, p-value < 2.2e-16
# SiteID IS significant.

# Variance Summary
VarCorr(fit)
# Groups   Name        Std.Dev.
# SiteID   (Intercept) 0.46287 
# Residual             0.55657

# ANOVA Summary
out1=anova(fit,type='2',ddf='Kenward-Roger') # Kenward-Roger Satterthwaite
out1
#                         Sum Sq Mean Sq NumDF   DenDF F value    Pr(>F)    
# Treatment              60.678 20.2260     3 247.104 65.2929 < 2.2e-16 ***
# SiteType                3.358  1.6791     2   9.984  5.4203  0.025478 *  
# Slope_AvgBasinwide_pct  4.616  4.6159     1   9.935 14.9010  0.003198 ** 
# Treatment:SiteType     11.257  1.8762     6 247.104  6.0568 6.223e-06 ***

# Coefficients Summary
fit # n=270; SiteID=14
out=summary(fit,ddf='Kenward-Roger') # Kenward-Roger Satterthwaite
out
# Random effects:
# Groups   Name        Variance Std.Dev.
# SiteID   (Intercept) 0.2142   0.4629  
# Residual             0.3098   0.5566  
# Number of obs: 270, groups:  SiteID, 14
# 
# Fixed effects:
#                                  Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)                      3.577290   0.413575  11.100294   8.650 2.90e-06 ***
# TreatmentN                       1.224210   0.157422 247.001694   7.777 2.02e-13 ***
# TreatmentP                       0.031844   0.157422 247.001694   0.202 0.839864    
# TreatmentNP                      1.471519   0.157422 247.001694   9.348  < 2e-16 ***
# SiteTypeUpstream                 1.156887   0.334026  14.417212   3.463 0.003658 ** 
# SiteTypeDownstream               1.675783   0.357691  14.383931   4.685 0.000327 ***
# Slope_AvgBasinwide_pct          -0.034209   0.008862   9.934892  -3.860 0.003198 ** 
# TreatmentN:SiteTypeUpstream     -0.800922   0.226268 247.044029  -3.540 0.000479 ***
# TreatmentP:SiteTypeUpstream     -0.463375   0.229939 247.440491  -2.015 0.044964 *  
# TreatmentNP:SiteTypeUpstream    -0.565092   0.226268 247.044029  -2.497 0.013161 *  
# TreatmentN:SiteTypeDownstream   -1.010837   0.237922 247.018276  -4.249 3.05e-05 ***
# TreatmentP:SiteTypeDownstream   -0.381008   0.237922 247.018276  -1.601 0.110566    
# TreatmentNP:SiteTypeDownstream  -1.169941   0.237922 247.018276  -4.917 1.60e-06 ***

# Export to csv
write.csv(out$coefficients,file='Model_Longitudinal/Coefs_Pvalues.csv')
write.csv(as.data.frame(out1),file='Model_Longitudinal/ANOVA_Pvalues.csv')

# Yhat, resids, R2
df[,yhat:=predict(fit,type='response',re.form=NULL)] # Include ALL random effects
df[,resids:=residuals(fit,type='deviance',scaled=F)]
df[,stdResids:=residuals(fit,type='deviance',scaled=T)]
df[,leverage:=cooks.distance(fit)] # influence(fit) takes way too long
R2=cor(df$logChla_mgM2,df$yhat,method='pearson')^2
R2 # 0.7702466

# Diagnostic Plots
png('Model_Longitudinal/Diagnostics.png',width=6.5,height=5.5,units='in',res=300,type='cairo',pointsize=pointsize)
par(mfrow=c(2,2),mar=c(4.5,4.5,2,1))
# Resids vs Fitted Plot
scatter.smooth(df$resids~df$yhat,span=0.7,lpars=list(col=4,lwd=2),xlab='Linear Predictor',ylab='Residuals',main='Residuals vs Fitted')
abline(h=0)
# Model
qqnorm(df$stdResids,ylab='Std. Residuals',main='Normal Q-Q')
qqline(df$stdResids) # Slight right-skew
# Location-Scale Plot
scatter.smooth(sqrt(abs(df$stdResids))~df$yhat,span=0.7,lpars=list(col=4,lwd=2),xlab='Linear Predictor',ylab='sqrt(abs(Std. Residuals))',main='Scale-Location')
# Leverage Plot - Possible, but not intuitive
par(mfrow=c(1,1))
dev.off()

# Residuals by Boxplot
png('Model_Longitudinal/ResidualsByTreatment.png',width=5,height=4.3,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(df,aes(x=Treatment,y=resids))+
	geom_hline(yintercept=0,color='grey')+
	geom_jitter(aes(color=SiteType),position=position_jitter(w=0.2,h=0))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	scale_y_continuous(trans='identity',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y='Residuals',x=NULL,title=NULL,color='Site Type')+
	theme_classic()+
	theme(legend.position='bottom')
dev.off()

# Random intercepts (plus SE - NOT CIs!)
tmp=ranef(fit,condVar=T)
png('Model_Longitudinal/RE_SiteID.png',width=5,height=7.0,units='in',res=300,type='cairo',pointsize=pointsize) # Doesn't respect pointsize
p=dotplot(tmp,main=F,
			 scales=list(y=list(cex=1),x=list(cex=1)),
			 par.strip.text=list(cex=1))
print(p$`SiteID`)
dev.off()

# 1:1 plot
xlim=ylim=range(c(df$logChla_mgM2,df$yhat))
png('Model_Longitudinal/_1to1.png',width=4,height=4, units='in',res=300,type='cairo',pointsize=pointsize)
par(pty='s',mar=c(4.5,4.5,1,1))
# plot(logChla_mgM2~yhat,data=df)
plot(logChla_mgM2~yhat,data=df,axes=F,ylab=expression(Observed~Chlorophyll~~mg/m^2),xlab=expression(Fitted~Chlorophyll~~mg/m^2),xlim=xlim,ylim=ylim,main=NULL)
rug(log(xMinorTicks),ticksize= -0.01,col='darkgrey')
axis(1,at=log(xTicks),labels=xTicks)
rug(log(xMinorTicks),side=2,ticksize= -0.01,col='darkgrey')
axis(2,at=log(xTicks),labels=xTicks)
abline(0,1)
legend('topleft',legend=sprintf('R2 = %0.2f',R2),pch=NA,inset=0.01,bty='o',merge=F) 
box()
par(pty='m')
dev.off()

# Boxplots - SiteType
png('Model_Longitudinal/SiteType.png',width=5,height=4.3,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(SiteYear[!is.na(SiteType),],aes(x=SiteType,y=Chla_mgM2))+
	geom_jitter(aes(color=Treatment),position=position_jitter(w=0.2,h=0))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	scale_y_continuous(trans='log10',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y=expression(Chlorophyll~~mg/m^2),x='Site Location',title=NULL,color='Treatment')+
	theme_classic()+
	theme(legend.position='bottom')
dev.off()





###################################*
### Post-Hoc Analysis ####
###################################*

# Load contrasts
contrast_L=fread('Contrasts_Longitudinal.csv',sep=',',skip=0,header=T,check.names=T,na.strings=na.strings)
dim(contrast_L) # 12 xx

# Estimated Marginal Means
rm(postHoc_Means)
postHoc_Means=emmeans(fit,specs= ~Treatment:SiteType,lmer.df='kenward-roger') # Do NOT switch order of "Treatment:SiteType"!
postHoc_Means
# Treatment SiteType    emmean    SE   df lower.CL upper.CL
# C         Undeveloped   2.36 0.236 14.2     1.86     2.87
# N         Undeveloped   3.59 0.236 14.2     3.08     4.09
# P         Undeveloped   2.39 0.236 14.2     1.89     2.90
# NP        Undeveloped   3.83 0.236 14.2     3.33     4.34
# C         Upstream      3.52 0.236 14.6     3.01     4.02
# N         Upstream      3.94 0.238 14.9     3.44     4.45
# P         Upstream      3.09 0.241 15.8     2.58     3.60
# NP        Upstream      4.43 0.238 14.9     3.92     4.93
# C         Downstream    4.04 0.266 14.6     3.47     4.61
# N         Downstream    4.25 0.264 14.2     3.69     4.82
# P         Downstream    3.69 0.264 14.2     3.12     4.25
# NP        Downstream    4.34 0.264 14.2     3.77     4.91
#
contrast_L[,.N,by=Level] # The order MUST match emmeans output above!

# Letters
tmp=multcomp::cld(postHoc_Means,Letter=letters)
tmp
# SiteType    Treatment emmean    SE   df lower.CL upper.CL .group       
# Undeveloped C           2.36 0.236 14.2     1.86     2.87  a      
# Undeveloped P           2.39 0.236 14.2     1.89     2.90  a      
# Upstream    P           3.09 0.241 15.8     2.58     3.60  abc    
# Upstream    C           3.52 0.236 14.6     3.01     4.02  abcde  
# Undeveloped N           3.59 0.236 14.2     3.08     4.09   bcdefg
# Downstream  P           3.69 0.264 14.2     3.12     4.25  ab d f 
# Undeveloped NP          3.83 0.236 14.2     3.33     4.34   bcdefg
# Upstream    N           3.94 0.238 14.9     3.44     4.45     defg
# Downstream  C           4.04 0.266 14.6     3.47     4.61   bcdefg
# Downstream  N           4.25 0.264 14.2     3.69     4.82   bcdefg
# Downstream  NP          4.34 0.264 14.2     3.77     4.91    c e g
# Upstream    NP          4.43 0.238 14.9     3.92     4.93       fg

# Export
fwrite(tmp,'Model_Longitudinal/PostHoc_Groups.csv')
groups=c('aaa')

# Post-Hoc Contrasts
rm(postHoc_pValues)
postHoc_pValues=contrast(postHoc_Means,method=list(
	# Undeveloped
	'Undeveloped:N - Undeveloped:C' =contrast_L$Undeveloped_N -contrast_L$Undeveloped_C,
	'Undeveloped:P - Undeveloped:C' =contrast_L$Undeveloped_P -contrast_L$Undeveloped_C,
	'Undeveloped:NP - Undeveloped:C'=contrast_L$Undeveloped_NP-contrast_L$Undeveloped_C,
	# Upstream
	'Upstream:N - Upstream:C' =contrast_L$Upstream_N -contrast_L$Upstream_C,
	'Upstream:P - Upstream:C' =contrast_L$Upstream_P -contrast_L$Upstream_C,
	'Upstream:NP - Upstream:C'=contrast_L$Upstream_NP-contrast_L$Upstream_C,
	# Downstream
	'Downstream:N - Downstream:C' =contrast_L$Downstream_N -contrast_L$Downstream_C,
	'Downstream:P - Downstream:C' =contrast_L$Downstream_P -contrast_L$Downstream_C,
	'Downstream:NP - Downstream:C'=contrast_L$Downstream_NP-contrast_L$Downstream_C,
	# Control
	'Upstream:C - Undeveloped:C'  =contrast_L$Upstream_C  -contrast_L$Undeveloped_C,
	'Downstream:C - Undeveloped:C'=contrast_L$Downstream_C-contrast_L$Undeveloped_C,
	'Downstream:C - Upstream:C'   =contrast_L$Downstream_C-contrast_L$Upstream_C),
	lmer.df='kenward-roger',adjust='hommel') # Not Tukey here! All pairwise comparisons are NOT being done.
postHoc_pValues
# contrast                       estimate    SE    df t.ratio p.value
# Undeveloped:N - Undeveloped:C    1.2242 0.157 247.0  7.777  <.0001 
# Undeveloped:P - Undeveloped:C    0.0318 0.157 247.0  0.202  0.8399 
# Undeveloped:NP - Undeveloped:C   1.4715 0.157 247.0  9.348  <.0001 
# Upstream:N - Upstream:C          0.4233 0.163 247.1  2.604  0.0586 
# Upstream:P - Upstream:C         -0.4315 0.168 247.8 -2.575  0.0637 
# Upstream:NP - Upstream:C         0.9064 0.163 247.1  5.577  <.0001 
# Downstream:N - Downstream:C      0.2134 0.178 247.0  1.196  0.4656 
# Downstream:P - Downstream:C     -0.3492 0.178 247.0 -1.957  0.2305 
# Downstream:NP - Downstream:C     0.3016 0.178 247.0  1.690  0.3104 
# Upstream:C - Undeveloped:C       1.1569 0.334  14.4  3.463  0.0283 
# Downstream:C - Undeveloped:C     1.6758 0.358  14.4  4.685  0.0029 
# Downstream:C - Upstream:C        0.5189 0.356  14.6  1.459  0.3492 

# Actual pvalues using Tukey - Not needed here

# Matrix
pwpm(postHoc_Means)
# Too much to paste.

# CIs - Do NOT use CIs to determine significance. Use p.value instead.
# set.seed(seed)
postHoc_CIs=confint(postHoc_pValues,level=0.95,adjust='hommel') # "hommel" not accepted for confint()
postHoc_CIs
# contrast                       estimate    SE    df lower.CL upper.CL
# Undeveloped:N - Undeveloped:C    1.2242 0.157 247.0   0.7701   1.6783
# Undeveloped:P - Undeveloped:C    0.0318 0.157 247.0  -0.4223   0.4859
# Undeveloped:NP - Undeveloped:C   1.4715 0.157 247.0   1.0174   1.9256
# Upstream:N - Upstream:C          0.4233 0.163 247.1  -0.0455   0.8921
# Upstream:P - Upstream:C         -0.4315 0.168 247.8  -0.9150   0.0519
# Upstream:NP - Upstream:C         0.9064 0.163 247.1   0.4376   1.3753
# Downstream:N - Downstream:C      0.2134 0.178 247.0  -0.3012   0.7280
# Downstream:P - Downstream:C     -0.3492 0.178 247.0  -0.8638   0.1654
# Downstream:NP - Downstream:C     0.3016 0.178 247.0  -0.2130   0.8162
# Upstream:C - Undeveloped:C       1.1569 0.334  14.4   0.0255   2.2883
# Downstream:C - Undeveloped:C     1.6758 0.358  14.4   0.4637   2.8879
# Downstream:C - Upstream:C        0.5189 0.356  14.6  -0.6837   1.7214

# Export
fwrite(as.data.frame(postHoc_pValues),'Model_Longitudinal/PostHoc_Table.csv')
# fwrite(as.data.frame(pairs(postHoc_Means)),'Model_Main/PostHoc_Table_Tukey.csv') # Not needed here
fwrite(as.data.frame(postHoc_CIs),'Model_Longitudinal/PostHoc_CIs.csv')

# Plot CIs
png('Model_Longitudinal/PostHoc_Plot.png',width=6,height=9.0,units='in',res=300,type='cairo',pointsize=pointsize)
plot(postHoc_CIs)+
	geom_vline(xintercept=0,color='grey',size=1,linetype='dashed')+
	labs(y='Treatment Comparisons',x='Log Treatment Difference (mg/m2)',title='Post-Hoc Treatment Comparisons (95% CI)')+
	theme_classic()+
	theme(text=element_text(size=txtSize))
dev.off()

# Boxplots - SiteType 
png('Model_Longitudinal/SiteType.png',width=5,height=4.3,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(SiteYear[!is.na(SiteType),],aes(x=SiteType,y=Chla_mgM2))+
	geom_jitter(position=position_jitter(height=0,width=0.2))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	scale_y_continuous(trans='log10',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y=expression(Chlorophyll~~mg/m^2),x='Site Location',title=NULL)+
	theme_classic()+
	theme(legend.position='bottom')
dev.off()

# Boxplots - SiteType:Treatment
png('Model_Longitudinal/SiteType_byTreatment.png',width=5,height=4.3,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(SiteYear[!is.na(SiteType),],aes(x=SiteType,y=Chla_mgM2,color=Treatment))+
	geom_jitter(position=position_jitterdodge(jitter.height=0,jitter.width=0.2))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	scale_y_continuous(trans='log10',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y=expression(Chlorophyll~~mg/m^2),x='Site Location',title=NULL,color='Treatment')+
	theme_classic()+
	theme(legend.position='bottom')
dev.off()





###################################*
### *** Light Model *** ####
###################################*

# Longitudinal Model uses year 2014 only.

#### Dataset
# Subset Data
df=copy(SiteYear[!is.na(CanOpen_frac),])
dim(df) # 336 xx

# Drop levels
droplevels(df$SiteID)

# Check for missing data
# visdat::vis_miss(df,cluster=T)




#### Model
# Fit
library(lmerTest)                   # Load/Unload as needed
# detach('package:lmerTest',unload=T) # Load/Unload as needed
rm(fit)
fit=lmer(logChla_mgM2~1+
				Treatment+                # Retain
				CanOpen_frac+             # Retain
				Treatment:CanOpen_frac+   # Remove 
				# WaterTemp_C+              # Remove 7
				# logTN_mgL+              # Remove 5
				# logTP_mgL+              # Remove 1
				## StreamStats:
				# logCatchmentArea_sqKm+  # Remove 4
				# Slope_AvgBasinwide_pct+ # Remove 2
				# logElev_Outlet_ft+        # Remove 6
				# logFlow_AnnualAvg_cfs+  # Remove 3
				# # Random Effects:
				(1|SiteID),               # Retain
			data=df,REML=T)
anova(fit,type='2',ddf='Kenward-Roger')
isSingular(fit) # SHOULD BE FALSE

# Test if "SiteID" is significant
exactRLRT(fit,nsim=10000,ncpus=ncpus,parallel='multicore',seed=seed)
# RLRT = 138.19, p-value < 2.2e-16
# SiteID IS significant.

# Variance Summary
VarCorr(fit)
# Groups   Name        Std.Dev.
# SiteID   (Intercept) 0.49721 
# Residual             0.59256 

# ANOVA Summary
out1=anova(fit,type='2',ddf='Kenward-Roger') # Kenward-Roger Satterthwaite
out1
#                        Sum Sq Mean Sq NumDF   DenDF F value  Pr(>F)    
# Treatment              44.759 14.9198     3 312.153 42.4915 < 2e-16 ***
# CanOpen_frac            0.066  0.0656     1  15.712  0.1867 0.67151    
# Treatment:CanOpen_frac  2.826  0.9421     3 312.152  2.6832 0.04684 *  

# Coefficients Summary
fit # n=336; SiteID=18
out=summary(fit,ddf='Kenward-Roger') # Kenward-Roger Satterthwaite
out
# Random effects:
# Groups   Name        Variance Std.Dev.
# SiteID   (Intercept) 0.2472   0.4972  
# Residual             0.3511   0.5926  
# Number of obs: 336, groups:  SiteID, 18
# 
# Fixed effects:
#                          Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)                2.7576     0.2713  22.9905  10.164 5.64e-10 ***
# TreatmentN                 1.3696     0.1822 312.2091   7.516 6.02e-13 ***
# TreatmentP                -0.2418     0.1817 312.2079  -1.331  0.18412    
# TreatmentNP                1.2494     0.1824 312.0926   6.851 3.92e-11 ***
# CanOpen_frac               0.6374     0.4208  22.8819   1.515  0.14356    
# TreatmentN:CanOpen_frac   -0.7261     0.2780 312.1149  -2.612  0.00945 ** 
# TreatmentP:CanOpen_frac   -0.5071     0.2763 312.2637  -1.836  0.06737 .  
# TreatmentNP:CanOpen_frac  -0.6331     0.2788 312.1901  -2.270  0.02387 *  

# Export to csv
write.csv(out$coefficients,file='Model_Light/Coefs_Pvalues.csv')
write.csv(as.data.frame(out1),file='Model_Light/ANOVA_Pvalues.csv')

# Yhat, resids, R2
df[,yhat:=predict(fit,type='response',re.form=NULL)] # Include ALL random effects
df[,resids:=residuals(fit,type='deviance',scaled=F)]
df[,stdResids:=residuals(fit,type='deviance',scaled=T)]
df[,leverage:=cooks.distance(fit)] # influence(fit) takes way too long
R2=cor(df$logChla_mgM2,df$yhat,method='pearson')^2
R2 # 0.6742733

# Diagnostic Plots
png('Model_Light/Diagnostics.png',width=6.5,height=5.5,units='in',res=300,type='cairo',pointsize=pointsize)
par(mfrow=c(2,2),mar=c(4.5,4.5,2,1))
# Resids vs Fitted Plot
scatter.smooth(df$resids~df$yhat,span=0.7,lpars=list(col=4,lwd=2),xlab='Linear Predictor',ylab='Residuals',main='Residuals vs Fitted')
abline(h=0)
# Model
qqnorm(df$stdResids,ylab='Std. Residuals',main='Normal Q-Q')
qqline(df$stdResids) # Slight right-skew
# Location-Scale Plot
scatter.smooth(sqrt(abs(df$stdResids))~df$yhat,span=0.7,lpars=list(col=4,lwd=2),xlab='Linear Predictor',ylab='sqrt(abs(Std. Residuals))',main='Scale-Location')
# Leverage Plot - Possible, but not intuitive
par(mfrow=c(1,1))
dev.off()

# Residuals by Boxplot
png('Model_Light/ResidualsByTreatment.png',width=5,height=4.3,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(df,aes(x=Treatment,y=resids))+
	geom_hline(yintercept=0,color='grey')+
	geom_jitter(aes(color=Treatment),position=position_jitter(w=0.2,h=0))+
	geom_boxplot(outlier.shape=NA,fill=NA)+
	scale_y_continuous(trans='identity',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y='Residuals',x=NULL,title=NULL,color='Treatment')+
	theme_classic()+
	theme(legend.position='bottom')
dev.off()

# Random intercepts (plus SE - NOT CIs!)
tmp=ranef(fit,condVar=T)
png('Model_Light/RE_SiteID.png',width=5,height=7.0,units='in',res=300,type='cairo',pointsize=pointsize) # Doesn't respect pointsize
p=dotplot(tmp,main=F,
			 scales=list(y=list(cex=1),x=list(cex=1)),
			 par.strip.text=list(cex=1))
print(p$`SiteID`)
dev.off()

# 1:1 plot
xlim=ylim=range(c(df$logChla_mgM2,df$yhat))
png('Model_Light/_1to1.png',width=4,height=4, units='in',res=300,type='cairo',pointsize=pointsize)
par(pty='s',mar=c(4.5,4.5,1,1))
# plot(logChla_mgM2~yhat,data=df)
plot(logChla_mgM2~yhat,data=df,axes=F,ylab=expression(Observed~Chlorophyll~~mg/m^2),xlab=expression(Fitted~Chlorophyll~~mg/m^2),xlim=xlim,ylim=ylim,main=NULL)
rug(log(xMinorTicks),ticksize= -0.01,col='darkgrey')
axis(1,at=log(xTicks),labels=xTicks)
rug(log(xMinorTicks),side=2,ticksize= -0.01,col='darkgrey')
axis(2,at=log(xTicks),labels=xTicks)
abline(0,1)
legend('topleft',legend=sprintf('R2 = %0.2f',R2),pch=NA,inset=0.01,bty='o',merge=F) 
box()
par(pty='m')
dev.off()




###################################*
### Post-Hoc Analysis ####
###################################*

# Post-Hoc comparisons
rm(postHoc_Means)
postHoc_Means=emmeans(fit,specs= ~Treatment:CanOpen_frac,lmer.df='kenward-roger')
postHoc_Means
# Treatment CanOpen_frac emmean    SE   df lower.CL upper.CL
# C                0.568   3.12 0.135 24.0     2.84     3.40
# N                0.568   4.08 0.134 23.1     3.80     4.35
# P                0.568   2.59 0.134 22.8     2.31     2.87
# NP               0.568   4.01 0.135 23.6     3.73     4.29
contrast_M[,.N,by=Level] # The order MUST match emmeans output above!

# Letters
tmp=multcomp::cld(postHoc_Means,Letter=letters)
tmp
# Treatment CanOpen_frac emmean    SE   df lower.CL upper.CL .group
# P                0.568   2.59 0.134 22.8     2.31     2.87  a    
# C                0.568   3.12 0.135 24.0     2.84     3.40   b   
# NP               0.568   4.01 0.135 23.6     3.73     4.29    c  
# N                0.568   4.08 0.134 23.1     3.80     4.35    c 
groups=c('aaa')

# Export
fwrite(tmp,'Model_Light/PostHoc_Groups.csv')

# Matrix
pwpm(postHoc_Means)

# Post-Hoc Contrasts
rm(postHoc_pValues)
postHoc_pValues=contrast(postHoc_Means,method=list(
	'N - C' =contrast_M$N -contrast_M$C,
	'P - C' =contrast_M$P -contrast_M$C,
	'NP - C'=contrast_M$NP-contrast_M$C,
	'NP - N'=contrast_M$NP-contrast_M$N,
	'NP - P'=contrast_M$NP-contrast_M$P,
	'N - P' =contrast_M$N -contrast_M$P),
	lmer.df='kenward-roger',adjust='tukey') # Not respecting Tukey
postHoc_pValues
# contrast estimate     SE  df t.ratio p.value
# N - C      0.9572 0.0923 312 10.372  <.0001 
# P - C     -0.5299 0.0918 312 -5.770  <.0001 
# NP - C     0.8899 0.0933 312  9.538  <.0001 
# NP - N    -0.0674 0.0918 313 -0.734  0.9762 
# NP - P     1.4197 0.0912 312 15.575  <.0001 
# N - P      1.4871 0.0899 312 16.535  <.0001
#
# Actual pvalues using Tukey:
pairs(postHoc_Means)

# CIs - Do NOT use CIs to determine significance. Use p.value instead.
postHoc_CIs=confint(postHoc_pValues,level=0.95)
postHoc_CIs
# contrast estimate     SE  df lower.CL upper.CL
# N - C      0.9572 0.0923 312    0.713    1.202
# P - C     -0.5299 0.0918 312   -0.773   -0.287
# NP - C     0.8899 0.0933 312    0.643    1.137
# NP - N    -0.0674 0.0918 313   -0.310    0.176
# NP - P     1.4197 0.0912 312    1.178    1.661
# N - P      1.4871 0.0899 312    1.249    1.725

# Export Tables - Not needed here

# Chla ~ Light
png('Model_Light/Light.png',width=5,height=4.3,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(SiteYear[!is.na(CanOpen_frac),],aes(x=CanOpen_frac,y=Chla_mgM2,color=Treatment))+
	geom_point()+
	geom_smooth(method='lm',se=F)+ # Interaction was significant
	scale_y_continuous(trans='log10',labels=labelsPretty)+
	scale_x_continuous(trans='identity',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y=expression(Chlorophyll~~mg/m^2),x='Canopy Openness (frac)',title=NULL,color='Treatment')+
	theme_classic()+
	theme(text=element_text(size=txtSize))+
	theme(legend.position='bottom')
dev.off()





###################################*
### *** Site-Specific Models *** ####
###################################*

# Use regular ANOVA on each site

# Initialize lists
sites=sort(unique(SiteYear$SiteID))
length(sites) # 38 sites
outCoefs=vector(mode='list',length=length(sites))
outANOVA=vector(mode='list',length=length(sites))
outPostHoc=vector(mode='list',length=length(sites))
names(outCoefs)=names(outANOVA)=names(outPostHoc)=sites

# LOOP
plots=F
i=1L
for(i in 1:length(sites)){ # Loop across sites
	# Countup
	cat(i,' ')
	rm(df,fit,R2)
	
	# Subset Data
	df=copy(SiteYear[SiteID==sites[i],])
	dim(df) # 20 xx
	
	# Fit
	fit=lm(logChla_mgM2~Treatment,data=df)
	outCoefs[[sites[i]]]=as.data.frame(summary(fit)$coefficients)
	outCoefs[[sites[i]]]$Variable=row.names(outCoefs[[sites[i]]]) # Add rownames
	# outCoefs[[sites[i]]]
	#              Estimate Std. Error   t value     Pr(>|t|)
	# (Intercept) 2.7098594  0.3055507 8.8687717 1.420114e-07
	# TreatmentN  0.3174798  0.4321139 0.7347131 4.731460e-01
	# TreatmentP  0.4063409  0.4321139 0.9403558 3.610164e-01
	# TreatmentNP 0.9500775  0.4321139 2.1986736 4.295674e-02
	
	# ANOVA
	outANOVA[[sites[i]]]=as.data.frame(anova(fit))
	outANOVA[[sites[i]]]$Variable=row.names(outANOVA[[sites[i]]]) # Add rownames
	# outANOVA[[sites[i]]]
	#           Df Sum Sq Mean Sq F value Pr(>F)
	# Treatment  3 2.3403 0.78012  1.6712 0.2131
	# Residuals 16 7.4689 0.46681
	
	# Yhat, resids, R2
	df[,yhat:=predict(fit,type='response')]
	df[,resids:=residuals(fit)]
	df[,stdResids:=residuals(fit)/sd(resids)]
	df[,leverage:=cooks.distance(fit)] # influence(fit) takes way too long
	R2=cor(df$logChla_mgM2,df$yhat,method='pearson')^2
	# R2 # 0.238586
	
	if(plots){
		# Diagnostic Plots
		png(paste0('Model_SiteSpecific/Diagnostics_',sites[i],'.png'),width=6.5,height=5.5,units='in',res=300,type='cairo',pointsize=pointsize)
		par(mfrow=c(2,2),mar=c(4.5,4.5,2,1))
		plot(fit)
		par(mfrow=c(1,1))
		dev.off()
		
		# Residuals by Boxplot
		png(paste0('Model_SiteSpecific/ResidualsByTreatment_',sites[i],'.png'),width=5,height=4.0,units='in',res=300,type='cairo',pointsize=pointsize)
		ggplot(df,aes(x=Treatment,y=resids))+
			geom_hline(yintercept=0,color='grey')+
			geom_jitter(position=position_jitter(w=0.2,h=0))+
			geom_boxplot(outlier.shape=NA,fill=NA)+
			scale_y_continuous(trans='identity',labels=labelsPretty)+
			scale_color_viridis_d(end=end)+
			labs(y='Residuals',x=NULL,title=NULL)+
			theme_classic()
		dev.off()
		
		# 1:1 plot
		xlim=ylim=range(c(df$logChla_mgM2,df$yhat))
		png(paste0('Model_SiteSpecific/1to1_',sites[i],'.png'),width=4,height=4, units='in',res=300,type='cairo',pointsize=pointsize)
		par(pty='s',mar=c(4.5,4.5,1,1))
		plot(logChla_mgM2~yhat,data=df,axes=F,ylab=expression(Observed~Chlorophyll~~mg/m^2),xlab=expression(Observed~Chlorophyll~~mg/m^2),xlim=xlim,ylim=ylim,main=NULL)
		rug(log(xMinorTicks),ticksize= -0.01,col='darkgrey')
		axis(1,at=log(xTicks),labels=xTicks)
		rug(log(xMinorTicks),side=2,ticksize= -0.01,col='darkgrey')
		axis(2,at=log(xTicks),labels=xTicks)
		abline(0,1)
		legend('topleft',legend=sprintf('R2 = %0.2f',R2),pch=NA,inset=0.01,bty='o',merge=F) 
		box()
		par(pty='m')
		dev.off()
	} # END if(plots)
	
	### Post-Hoc Analysis ####
	# Check ANOVA
	tmp=outANOVA[[sites[i]]]$`Pr(>F)`[1]
	tmp # 0.2130934
	if(tmp<0.05){
		# Tukey HSD
		outPostHoc[[sites[i]]]=TukeyHSD(aov(logChla_mgM2~Treatment,data=df),conf.level=0.95)
		outPostHoc[[sites[i]]]$Treatment
		#            diff        lwr      upr     p adj
		# N-C  0.31747976 -0.9188068 1.553766 0.8817105
		# P-C  0.40634085 -0.8299457 1.642627 0.7839507
		# NP-C 0.95007751 -0.2862090 2.186364 0.1658050
		# P-N  0.08886109 -1.1474255 1.325148 0.9967934
		# NP-N 0.63259775 -0.6036888 1.868884 0.4805020
		# NP-P 0.54373666 -0.6925499 1.780023 0.6008163
		
		# Plot
		png(paste0('Model_SiteSpecific/PostHoc_',sites[i],'.png'),width=5,height=4.5,units='in',res=300,type='cairo',pointsize=pointsize)
		par(mar=c(4.5,4.0,2.5,1))
		plot(outPostHoc[[sites[i]]])
		abline(v=0,lty=2,lwd=1,col='grey')
		dev.off()
		
		# Add rownames
		outPostHoc[[sites[i]]]=as.data.frame(outPostHoc[[sites[i]]]$Treatment)
		outPostHoc[[sites[i]]]$Contrast=row.names(outPostHoc[[sites[i]]]) # Add rownames
	} # END if(Trt is significant)
} # END Loop
warnings()

# Append outCoefs
tmp1=rbindlist(outCoefs,idcol='SiteID',use.names=T,fill=T)
names(tmp1)=c('SiteID','Estimate','StdError','tValue','Pvalue','Variable')
dim(tmp1) # 152 xx

# Append outANOVA
tmp2=rbindlist(outANOVA,idcol='SiteID',use.names=T,fill=T)
names(tmp2)=c('SiteID','df','SS','MSS','Fvalue','Pvalue','Variable')
dim(tmp2) # 76 xx

# Append outPostHoc
tmp3=rbindlist(outPostHoc,idcol='SiteID',use.names=T,fill=T)
names(tmp3)=c('SiteID','Difference','lwr','upr','AdjPvalue','Contrast')
dim(tmp3) # 180 xx

# Add Significant
tmp3[,Significant:=0L]
tmp3[AdjPvalue<0.05,Significant:=1]

# Create sites
sites=copy(SiteYear[,.(SiteID,Year)])
sites=unique(sites)
dim(sites) # 38 xx

# Join to ANOVA (for Year)
tmp2=merge(tmp2,sites,by=c('SiteID'),all.x=T) # Left outer join

# Check ANOVA results
png('Model_SiteSpecific/_ANOVA_Results.png',width=6.5,height=5,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(tmp2[Variable=='Treatment',],aes(y=Pvalue,x=SiteID))+
	geom_point(aes(color=as.factor(Year)),size=2)+
	geom_hline(yintercept=0.05,color=2,linetype=2)+
	# scale_y_continuous(trans='identity',limits=c(0,1))+
	scale_color_viridis_d(end=end)+
	labs(y='ANOVA P-value',x=NULL,title=NULL,color='Year')+
	theme_classic()+
	theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
	theme(text=element_text(size=txtSize))+
	theme(legend.position='bottom')
dev.off()

# Percentage significant
tmp2[Variable=='Treatment',.N,by=Pvalue<0.05]
#    Pvalue  N
# 1:  FALSE  8
# 2:   TRUE 30 - Significant
30/38*100 # 78.94737% significant

# Post-Hoc summary table
tmp4=tmp3[,.N,by=Contrast]
names(tmp4)=c('Contrast','nTotal')
tmp5=tmp3[Significant==1,.N,by=Contrast]
names(tmp5)=c('Contrast','nSignificant')
tmp4=merge(tmp4,tmp5,by=c('Contrast'))
rm(tmp5)
tmp4[,PctSignificant:=nSignificant/nTotal*100]

# Export to csv
fwrite(tmp1,'Model_SiteSpecific/_Coefs_Pvalues.csv')
fwrite(tmp2,'Model_SiteSpecific/_ANOVA_Pvalues.csv')
fwrite(tmp3,'Model_SiteSpecific/_PostHoc_Pvalues.csv')
fwrite(tmp4,'Model_SiteSpecific/_PostHoc_Summary.csv')





###################################*
#### *** Response Ratios *** ####
###################################*

# Plot settings
width=5
height=4.5

# Subset Data
df=copy(SiteYear[,.(Year,SiteID,logNRR,logPRR,logTN_mgL,logTP_mgL)])
df=unique(df,by='SiteID')
dim(df)                         # 38 xx
length(unique(SiteYear$SiteID)) # 38  MUST MATCH!
ylim=range(SiteYear[,.(NRR,PRR)])



#### Nitrogen
# Fit
rm(tmp)
tmp=lm(logNRR~logTN_mgL,data=df)
# Note: "1/NRR~logTN" was a slightly better fit. But there is no theoretical reason to model inverse, so don't use.
tmp1=summary(tmp)
tmp1$r.squared # 0.3329745
tmp1
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.005171   0.061322   0.084 0.933267    
# logTN_mgL   -0.170037   0.040111  -4.239 0.000149 ***
write.csv(tmp1$coefficients,'Model_ResponseRatios/N_Pvalues.csv')

# Diagnostics
png('Model_ResponseRatios/N_Diagnostics.png',width=6.5,height=5.5,units='in',res=300,type='cairo',pointsize=pointsize)
par(mfrow=c(2,2))
plot(tmp)
par(mfrow=c(1,1))
dev.off()

# Plot
png('Model_ResponseRatios/N_Plot.png',width=width,height=height,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(unique(SiteYear,by='SiteID'),aes(y=NRR,x=TN_mgL))+
	geom_point(aes(color=as.factor(Year)))+
	geom_smooth(method='lm',se=F,color=1)+
	# geom_smooth(method='loess',se=F,color=1)+
	scale_y_continuous(trans='log10',labels=labelsPretty,limits=ylim)+
	scale_x_continuous(trans='log10',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y='Nitrogen Response Ratio',x='TN (mg/L)',title=NULL,color='Year')+
	theme_bw()+
	theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
	theme(text=element_text(size=txtSize))+
	theme(legend.position='bottom')
dev.off()




#### Phosphorus
# Potential extreme value: SiteID=="138A", Year = 2014
plot(logPRR~logTP_mgL,data=df)
# identify(df$logPRR~df$logTP_mgL)
df[SiteID=='138A',.N,by=.(SiteID,Year)]
#    SiteID Year  N
# 1:   138A 2014 1
# Note: 1 site in the lower-left corner removed (in fit and ggplot)

## Fit
rm(tmp)
tmp=lm(logPRR~logTP_mgL,data=df[SiteID!='138A']) # Removed 1 extreme value. This SiteID did not influence any other models.
tmp1=summary(tmp)
tmp1$r.squared # 0.1469315  (R2 = 0.03053258 WITH extreme value)
tmp1
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -0.29789    0.07932  -3.756 0.000629 ***
# logTP_mgL   -0.05482    0.02233  -2.455 0.019189 * 
write.csv(tmp1$coefficients,'Model_ResponseRatios/P_Pvalues.csv')

# Diagnostics
png('Model_ResponseRatios/P_Diagnostics.png',width=6.5,height=5.5,units='in',res=300,type='cairo',pointsize=pointsize)
par(mfrow=c(2,2))
plot(tmp)
par(mfrow=c(1,1))
dev.off()

# Plot
png('Model_ResponseRatios/P_Plot.png',width=width,height=height,units='in',res=300,type='cairo',pointsize=pointsize)
ggplot(unique(SiteYear[SiteID!='138A'],by='SiteID'),aes(y=PRR,x=TP_mgL))+
	geom_point(aes(color=as.factor(Year)))+
	geom_smooth(method='lm',se=F,color=1)+
	# geom_smooth(method='loess',se=F,color=1)+
	scale_y_continuous(trans='log10',labels=labelsPretty,limits=ylim)+
	scale_x_continuous(trans='log10',labels=labelsPretty)+
	scale_color_viridis_d(end=end)+
	labs(y='Phosphorus Response Ratio',x='TP (mg/L)',title=NULL,color='Year')+
	theme_bw()+
	theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
	theme(text=element_text(size=txtSize))+
	theme(legend.position='bottom')
dev.off()

















