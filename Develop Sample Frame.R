# in SD3 branch
# setwd("P:/Projects/NEEA22 (CBSA Phase 1 Planning)/_04 Assessment of Alternative Designs/Tool/FaithSD3")

# install.packages('plyr')
library(plyr)
#install.packages("data.table")
library(data.table)
#install.packages("openxlsx")
library(openxlsx)
# install.packages("stringi")
library(stringi)

## load Mike's NEEA frame)------
# fname.frame <- './qryFrame_13_FrameExport-PopSim.csv'
# frame.0 <- read.csv(fname.frame,colClasses=c(rep('factor',6),rep('numeric',2),'character',rep('factor',5),'numeric'))
# frame.0$SBW_Frame_Floor_Area <- as.numeric(frame.0$SBW_Frame_Floor_Area)
# save(frame.0,file='frame_0.rda')
# rm(frame.0)
frame.0 <- data.table(get(load('frame_0.rda')))
# str(frame.0)
frame.0[,Census.Group.ID:=stri_sub(Census.ID,from=1,to=13)]
frame.0$Census.ID <- as.factor(frame.0$Census.ID)
frame.0$Census.Group.ID <- as.factor(frame.0$Census.Group.ID)
frame.0[,Census.Block.ID:=Census.ID]

# cb.smry.0 <- frame.0[,.(rec.ct=length(RecordID)),by='Census.ID']
# sum(cb.smry.0$rec.ct)/nrow(cb.smry.0)
# mean(cb.smry.0$rec.ct)

frame.0[SBW_Frame_Floor_Area<16,SBW_Frame_Floor_Area:=NA]
# head(frame.0)
# tail(frame.0)
# summary(frame.0)
excl.bldg.types <- c('Residential','Manufacturing/Industrial','Agricultural')
frame.1 <- copy(frame.0[!((BLD_TYP_2009.CBSA %in% c('','Unknown') & is.na(SBW_Frame_Floor_Area))|(BLD_TYP_2009.CBSA %in% excl.bldg.types))])
frame.1 <- frame.1[!(nchar(as.character(Address_Final))==0 & is.na(ZIP_Final) & BLD_TYP_2009.CBSA=='Other' & is.na(SBW_Frame_Floor_Area))]
frame.1[,FD.BLDG.TYPE:=BLD_TYP_2009.CBSA]
frame.1[BLD_TYP_2009.CBSA %in% c('Unknown',''),FD.BLDG.TYPE:='UNKNOWN']
## make sure Coeur D Alene is spelled consistently
frame.1[grepl('Coeur D\'Alene',City_Final),City_Final:='Coeur D Alene']
# frame.1[grepl('Alene',City_Final),.(length(RecordID)),by='City_Final']
## make capitalization consistent for City_Final
frame.1[,City_Final:=gsub("(\\w)(\\w*)", "\\U\\1\\L\\2", City_Final, perl=TRUE)]
## remove duplicates--------
## collapse records with no floor area to census block, address
frame.no.sf <- unique(frame.1[is.na(SBW_Frame_Floor_Area)],by=c('Census.ID','Address_Final'))
## collapse records with floor area to census block, floor area
frame.rm.dup <- unique(frame.1[!is.na(SBW_Frame_Floor_Area)],by=c('Census.ID','SBW_Frame_Floor_Area'))
## combine these two data sets back together
frame.2 <- rbind(frame.rm.dup,frame.no.sf)
frame.2[,bldg.type:=FD.BLDG.TYPE]
# sum(as.numeric(frame.2$SBW_Frame_Floor_Area),na.rm=T)
# head(frame.2)

## load Mike's NEEA utility assignment data ----------
fname.util <- './qryNEEA_09a_CBSAAreaUtilAllocation-PopSim.csv'
zip.util.0 <- data.table(read.csv(fname.util))#,colClasses = c('numeric',rep('factor',3),rep('numeric',2),rep('factor',5))))
# tail(zip.util.0)
# str(zip.util.0)
# zip.util.0[RevisedAllocation!=1 & OverSampleUtility %in% c('Bonneville Power Administration','Energy Trust of Oregon'),.(cnt=length(ZIP5)),by='OverSampleUtility'][cnt>1]
zip.util.1 <- zip.util.0[,.(max.alloc=max(RevisedAllocation)),by=c('ZIP5','OverSampleUtilityRevised')]
zip.util.2 <- unique(zip.util.1,by='ZIP5')
# zip.util.2$ZIP5 <- as.numeric(zip.util.2$ZIP5)
colnames(zip.util.2)[1:2] <- c('ZIP_Final','OverSampleUtility')
# str(zip.util.2)
# head (zip.util.2)

## load data collection cost data---------
fname.cost <- './CBSA Data Collection Cost.xlsx'
## building survey costs
dc.cost <- data.table(read.xlsx(fname.cost,sheet='for R - bldg survey',colNames = T))

## building size classifications by building type
dc.cost.bldg.sz.0 <- data.table(read.xlsx(fname.cost,sheet='Input - Hours',colNames = T))
dc.cost.bldg.sz.0[,X1:=NULL]
dc.cost.bldg.sz.0[UboundSF=='Max',UboundSF:=Inf]
dc.cost.bldg.sz.0$UboundSF <- as.numeric(dc.cost.bldg.sz.0$UboundSF)
dc.cost.bldg.sz <- dc.cost.bldg.sz.0[complete.cases(dc.cost.bldg.sz.0)][Building.Type != c('Assembly','Mixed'),c('CBSA.Building.Type','Building.Size','LboundSF','UboundSF'),with=F]
colnames(dc.cost.bldg.sz)[1:2] <- c('bldg.type','bldg.sz')

## load census block survey costs
cb.cost <- data.table(read.xlsx(fname.cost,sheet='for R - census block survey',colNames = T))

## load "costing zones" - Seattle/Portland metro areas more expensive than Other
cost.zones.0 <- data.table(read.xlsx(fname.cost,sheet='SBWCostingZone',colNames = T))
cost.zones <- copy(cost.zones.0[,c('stabbr','cty_name','Costing.Zone'),with=F])

## load Mike's CBSA data -------------
fname.cbsa <- './qryCBSA_01_UrbanRuralFIPS_PopSim.csv'
cbsa.0 <- data.table(read.csv(fname.cbsa))
# head(cbsa.0)
# summary(cbsa.0)
cbsa.1 <- copy(cbsa.0)
cbsa.1$CBSA.Rec.ID <- 1:nrow(cbsa.1)
setcolorder(cbsa.1,c(ncol(cbsa.1),1:(ncol(cbsa.1)-1)))
cbsa.1[,Gas.EUI:=as.numeric(as.character(Gas.EUI))]
cbsa.1[is.na(Therm_SBWRevised),Gas.EUI:=NA]
# cbsa.1[,.(length(BLD_SF)),by='BLD_TYP_2009_SBWRevised']
cbsa.1$campus <- 'N'
setorder(cbsa.1,-kWh_SBWRevised)
## assign top energy consumers as campuses
cbsa.1[1:7,campus:='Y']
setorder(cbsa.1,CBSA.Rec.ID)
# cbsa.1[campus=='Y']
# cbsa.1[,.(length(BLD_SF)),by='BLD_TYP_2009_SBWRevised']

## get unknown building types in frame from CBSA based on frame square footage -------
frame.uk.bt <- copy(frame.2[FD.BLDG.TYPE=='UNKNOWN'])
frame.uk.bt[,BLD_SF:=SBW_Frame_Floor_Area]
setorder(frame.uk.bt,cty_name,BLD_SF)
setkey(frame.uk.bt,BLD_SF)
# head(frame.uk.bt)
cbsa.sf.bt <- copy(cbsa.1[campus=='N',c('CBSA.Rec.ID','cty_name','BLD_TYP_2009_SBWRevised','campus','BLD_SF','Vintage_SBWRevised','Elect.EUI','Gas.EUI','kWh_SBWRevised','Therm_SBWRevised'),with=F])
setorder(cbsa.sf.bt,cty_name,BLD_SF)
setkey(cbsa.sf.bt,BLD_SF)
## join frame and cbsa grabbing CBSA values based on nearest SF (bldg type, vintage, EUIs, energy)
frame.uk.bt.assign <- cbsa.sf.bt[frame.uk.bt,roll='nearest',on=c('cty_name','BLD_SF')]
# frame.uk.bt.assign[,.(length(RecordID)),by='Density']
frame.uk.bt.combo <- unique(frame.uk.bt.assign,by=c('RecordID','Density'))
# frame.uk.bt.combo[,.(length(RecordID)),by='Density']

## catch any records that didn't get building type assigned (cuz the county didn't have match, do at Density level)
frame.uk.bt.1 <- copy(frame.2[RecordID %in% frame.uk.bt.assign[is.na(BLD_TYP_2009_SBWRevised)]$RecordID])
frame.uk.bt.1[,BLD_SF:=SBW_Frame_Floor_Area]
setorder(frame.uk.bt.1,Density,BLD_SF)
setkey(frame.uk.bt.1,BLD_SF)
# head(frame.uk.bt.1)
cbsa.sf.bt.1 <- copy(cbsa.1[campus=='N',c('CBSA.Rec.ID','Density','BLD_TYP_2009_SBWRevised','campus','BLD_SF','Vintage_SBWRevised','Elect.EUI','Gas.EUI','kWh_SBWRevised','Therm_SBWRevised'),with=F])
setorder(cbsa.sf.bt.1,Density,BLD_SF)
setkey(cbsa.sf.bt.1,BLD_SF)
# head(cbsa.sf.bt.1)
frame.uk.bt.assign.1 <- cbsa.sf.bt.1[frame.uk.bt.1,roll='nearest',on=c('Density','BLD_SF')]
# summary(frame.uk.bt.assign.1)
frame.uk.bt.combo.1 <- unique(frame.uk.bt.assign.1,by=c('RecordID','Density'))
# summary(frame.uk.bt.combo.1)
# frame.uk.bt.combo.1[,.(length(RecordID)),by='Density']

## line up columns and combine all records which bldg types have been assigned to
base.cnames <- colnames(frame.2)
frame.uk.bt.cnames <- colnames(frame.uk.bt.combo)
setcolorder(frame.uk.bt.combo,c(base.cnames,setdiff(frame.uk.bt.cnames,base.cnames)))
frame.uk.bt.1.cnames <- colnames(frame.uk.bt.combo.1)
setcolorder(frame.uk.bt.combo.1,c(base.cnames,setdiff(frame.uk.bt.1.cnames,base.cnames)))
# colnames(frame.uk.bt.combo)==colnames(frame.uk.bt.combo.1)
frame.assigned.bt <- rbind(frame.uk.bt.combo[!is.na(BLD_TYP_2009_SBWRevised)],frame.uk.bt.combo.1)
frame.assigned.bt[,bldg.type:=BLD_TYP_2009_SBWRevised]

## get unknown SF in frame from CBSA based on frame bldg type------
frame.uk.sf <- frame.2[is.na(SBW_Frame_Floor_Area)]
frame.uk.sf$SBW_Frame_Floor_Area <- as.numeric(frame.uk.sf$SBW_Frame_Floor_Area)
# frame.uk.sf[,descr.combo:=paste(stabbr,cty_name,Density,BLD_TYP_2009.CBSA,sep='_')]
frame.uk.sf[,bldg.type:=FD.BLDG.TYPE]
cbsa.1[,bldg.type:=BLD_TYP_2009_SBWRevised]
# cbsa.1[,SBW_Frame_Floor_Area:=BLD_SF]
# head(cbsa.1)

# frame.uk.sf[,.(length(RecordID)),by='bldg.type'][cbsa.1[campus=='N',.(length(stabbr)),by='bldg.type'],on='bldg.type']

set.seed(555)
frame.agg.dens.bt <- frame.uk.sf[,.(smpl.sz=length(RecordID)),by=c('Density','bldg.type')]
 # r<-1
## randomly select SF values from CBSA for each Density+BldgType combo------
system.time({
  tmp.smpl <- sapply(1:nrow(frame.agg.dens.bt),function(r) {
    cbsa.smpl <- sample(cbsa.1[campus=='N' & as.character(bldg.type)==as.character(frame.agg.dens.bt[r]$bldg.type) & 
                    as.character(Density)==as.character(frame.agg.dens.bt[r]$Density)]$CBSA.Rec.ID,size=frame.agg.dens.bt[r]$smpl.sz,replace=T)
    return(frame.uk.sf[bldg.type==frame.agg.dens.bt[r]$bldg.type & Density==frame.agg.dens.bt[r]$Density,CBSA.Rec.ID:=cbsa.smpl])
  })
  rm(tmp.smpl)
})
# head(frame.uk.sf)
# head(cbsa.1)

## merge frame and cbsa for frame records that originally had no SF entry
frame.assigned.sf <- merge(frame.uk.sf,cbsa.1[,c('CBSA.Rec.ID','Density','BLD_TYP_2009_SBWRevised','bldg.type','campus','BLD_SF','Vintage_SBWRevised','Elect.EUI','Gas.EUI','kWh_SBWRevised','Therm_SBWRevised'),with=F],
                                 by=c('CBSA.Rec.ID','Density','bldg.type'),all.x=T)
# head(frame.assigned.sf)
# frame.assigned.sf[,.(avg.sf=mean(SBW_Frame_Floor_Area),sd.sf=sd(SBW_Frame_Floor_Area)),by=c('Density','bldg.type')][cbsa.1[,.(avg.sf=mean(BLD_SF),sd.sf=sd(BLD_SF)),by=c('Density','bldg.type')],on=c('Density','bldg.type')]

## for remaining frame records (those that already had bldg type and floor area), get vintage, EUIs, consumption from CBSA
##    based on nearest SF for given density and bldg type
frame.2.remaining <- copy(frame.2)
frame.2.remaining[,bldg.type:=BLD_TYP_2009.CBSA]
frame.2.remaining[,BLD_SF:=SBW_Frame_Floor_Area]
frame.2.remaining <- cbsa.1[,c('CBSA.Rec.ID','stabbr','Density','BLD_TYP_2009_SBWRevised','bldg.type','campus','BLD_SF','Vintage_SBWRevised',
                               'Elect.EUI','Gas.EUI','kWh_SBWRevised','Therm_SBWRevised'),with=F][frame.2.remaining[!is.na(SBW_Frame_Floor_Area) & FD.BLDG.TYPE!='UNKNOWN'],
                                roll='nearest',on=c('stabbr','Density','bldg.type','BLD_SF')]
# frame.2.remaining[,.(length(RecordID)),by='bldg.type']
frame.2.remaining <- unique(frame.2.remaining,by='RecordID')

## line up columns and combine all records which bldg types have been assigned to
base.cnames <- colnames(frame.2)
frame.remaining.cnames <- colnames(frame.2.remaining)
setcolorder(frame.2.remaining,c(base.cnames,setdiff(frame.remaining.cnames,base.cnames)))
frame.bt.cnames <- colnames(frame.assigned.bt)
setcolorder(frame.assigned.bt,c(frame.remaining.cnames,setdiff(frame.bt.cnames,frame.remaining.cnames)))
frame.sf.cnames <- colnames(frame.assigned.sf)
setcolorder(frame.assigned.sf,c(frame.remaining.cnames,setdiff(frame.sf.cnames,frame.remaining.cnames)))
# colnames(frame.assigned.sf)==colnames(frame.assigned.bt)
# match(colnames(frame.2.remaining),colnames(frame.assigned.sf))
# frame.assigned.bt[,.(length(RecordID)),by='Density']
# frame.uk.sf.combo.1[,.(length(RecordID)),by='Density']
# frame.2.remaining[,.(length(RecordID)),by='Density']
sim.frame <- data.table(rbind(frame.assigned.bt,frame.assigned.sf,frame.2.remaining))
# str(sim.frame)
# sim.frame$ZIP_Final <- as.factor(sim.frame$ZIP_Final)

## assign utilities to simulated frame -------------
## first need to find frame records with no zips and assign zips as possible based on census block
cb.missing.zip <- unique(sim.frame[is.na(ZIP_Final)]$Census.ID)
# head(cb.missing.zip)
get.missing.zips <- unique(sim.frame[!is.na(ZIP_Final) & Census.ID %in% cb.missing.zip,c('Census.ID','ZIP_Final'),with=F])
assign.zip <- unique(sim.frame[is.na(ZIP_Final),colnames(sim.frame)[!grepl('ZIP_Final',colnames(sim.frame))],with=F][get.missing.zips,on='Census.ID'],
                     by='RecordID')
setcolorder(assign.zip,c(1:3,ncol(assign.zip),4:(ncol(assign.zip)-1)))
sim.frame <- rbind(sim.frame[!(RecordID %in% assign.zip$RecordID)],assign.zip)

## assign Census.ID to census block group ID
sim.frame[,Census.ID:=Census.Group.ID]
uniqueN(sim.frame,by=c('Census.Group.ID','ZIP_Final'))

## ensure one zip code assignment per census block group
cens.grp.zip.0 <- sim.frame[!is.na(ZIP_Final),.(bldg.cnt=length(RecordID)),by=c('Census.ID','ZIP_Final')]
setorder(cens.grp.zip.0,Census.ID,-bldg.cnt)
cens.grp.zip.1 <- cens.grp.zip.0[,.(ZIP_Final=head(ZIP_Final,1)),by='Census.ID']
sim.frame <- merge(sim.frame[,colnames(sim.frame)[!grepl('ZIP_Final',colnames(sim.frame))],with=F],cens.grp.zip.1[,c('Census.ID','ZIP_Final'),with=F],by='Census.ID',all.x=T)

# nrow(frame.2[is.na(ZIP_Final) & is.na(SBW_Frame_Floor_Area)])
# str(sim.frame)
sim.frame <- merge(sim.frame,zip.util.2[,1:2,with=F],by='ZIP_Final',all.x=T)
# sim.frame[is.na(OverSampleUtility),.(length(RecordID)),by='bldg.type']
## now need to find remaining frame records with no utility assignments and assign utilities as possible based on city
city.missing.util <- unique(sim.frame[is.na(OverSampleUtility) & nchar(as.character(City_Final))>0 & nchar(as.character(stabbr))>0,c('City_Final','stabbr'),with=F],
                           by=c('City_Final','stabbr'))
get.missing.util <- unique(sim.frame[!is.na(OverSampleUtility) & nchar(as.character(City_Final))>0 & nchar(as.character(stabbr))>0 & 
                            (City_Final %in% city.missing.util$City_Final & stabbr %in% city.missing.util$stabbr),
                          c('City_Final','stabbr','OverSampleUtility'),with=F])
get.missing.util <- get.missing.util[,.(OverSampleUtility=head(OverSampleUtility,1)),by=c('City_Final','stabbr')]
assign.util <- unique(sim.frame[is.na(OverSampleUtility) & nchar(as.character(City_Final))>0 & nchar(as.character(stabbr))>0,
                    colnames(sim.frame)[!grepl('OverSampleUtility',colnames(sim.frame))],with=F][get.missing.util,
                    on=c('City_Final','stabbr')],by='RecordID')
sim.frame <- rbind(sim.frame[!(RecordID %in% assign.util$RecordID)],assign.util)
## now need to find remaining frame records with no utility assignments and assign utilities as possible based on County
county.missing.util <- unique(sim.frame[is.na(OverSampleUtility),c('cty_name','stabbr'),with=F],by=c('cty_name','stabbr'))
get.missing.util.1 <- unique(sim.frame[!is.na(OverSampleUtility) & 
                                         (cty_name %in% county.missing.util$cty_name & stabbr %in% county.missing.util$stabbr),
                                     c('cty_name','stabbr','OverSampleUtility'),with=F])
get.missing.util.1 <- get.missing.util.1[,.(OverSampleUtility=head(OverSampleUtility,1)),by=c('cty_name','stabbr')]
assign.util.1 <- unique(sim.frame[is.na(OverSampleUtility),colnames(sim.frame)[!grepl('OverSampleUtility',colnames(sim.frame))],with=F]
                        [get.missing.util.1,on=c('cty_name','stabbr')],by='RecordID')[!is.na(RecordID)]
sim.frame <- rbind(sim.frame[!(RecordID %in% assign.util.1$RecordID)],assign.util.1)
# sim.frame[,.(length(RecordID)),by='OverSampleUtility']
# uniqueN(sim.frame,by=c('Census.ID','OverSampleUtility'))
sim.frame$Pub.Priv <- 'Public'
sim.frame[OverSampleUtility %in% c('Puget Sound Energy','Energy Trust of Oregon','Idaho Power Company','NorthWestern Energy','Avista Utilities','PacifiCorp'),
          Pub.Priv:='Private']
# sim.frame[,.(length(RecordID)),by=c('Pub.Priv','OverSampleUtility')]

## assign data collection cost to simulated frame --------------
## first assign costing zones based on county
sim.frame <- sim.frame[cost.zones,on=c('stabbr','cty_name')]
## next assign building in frame to sample size categories
# r<-1
tmp <- lapply(1:nrow(dc.cost.bldg.sz),function(r) {
  sim.frame[bldg.type==dc.cost.bldg.sz[r]$bldg.type & BLD_SF>= dc.cost.bldg.sz[r]$LboundSF & BLD_SF <= dc.cost.bldg.sz[r]$UboundSF,
            bldg.sz:=dc.cost.bldg.sz[r]$bldg.sz]
})
rm(tmp)
## now assign building survey costs
sim.frame <- merge(sim.frame,dc.cost,by=c('bldg.type','Costing.Zone','bldg.sz'),all.x=T)
## assign census block suvey costs
sim.frame <- sim.frame[cb.cost,on='Costing.Zone']
# sim.frame[,.(bldg.cost=mean(survey.cost),cb.cost=mean(cb.survey.cost)),by=c('bldg.type','Costing.Zone','bldg.sz')]

## disturb the SF and vintage, update kWh and Therms accordingly-------------
set.seed(246)
noise <- runif(nrow(sim.frame),min=0.9,max=1.1)
sim.frame[,sim.sf:=BLD_SF*noise]
sim.frame[,sim.kWh:=Elect.EUI*sim.sf]
sim.frame[,sim.therm:=Gas.EUI*sim.sf]
# sim.frame[,sim.therm:=Therm_SBWRevised*noise]
# sim.frame[,sim.Gas.EUI:=sim.therm/sim.sf]
set.seed(135)
noise.yr <- runif(nrow(sim.frame),min=1,max=1.001)
# sim.frame[,sim.vintage:=Vintage_SBWRevised]
sim.frame[,sim.vintage:=round(Vintage_SBWRevised*noise.yr,0)]
# summary(sim.frame[,c('BLD_SF','sim.sf','kWh_SBWRevised','sim.kWh','Therm_SBWRevised','sim.therm','Vintage_SBWRevised','sim.vintage'),with=FALSE])
# summary(sim.frame[,c('BLD_SF','sim.sf','kWh_SBWRevised','sim.kWh','Therm_SBWRevised','sim.therm','Gas.EUI','sim.Gas.EUI','Vintage_SBWRevised','sim.vintage'),with=FALSE])
# summary(cbsa.1)

# sim.frame[,.(rec.ct=length(RecordID),tot.sf=sum(sim.sf),mean.sf=mean(sim.sf),cv.sf=sd(sim.sf)/mean(sim.sf))]
## tot floor area too high, reduce to 75% to get it closer to 3.6 billion SF
sim.frame[,sim.sf:=0.75*sim.sf]
# sim.frame[,.(rec.ct=length(RecordID),tot.sf=sum(sim.sf),mean.sf=mean(sim.sf),cv.sf=sd(sim.sf)/mean(sim.sf)),]

# saveRDS(sim.frame,file='full_sim_frame.rds')

sim.frame.2.save <- sim.frame[,c('RecordID','stabbr','Density','cty_name','Census.ID','OverSampleUtility','Pub.Priv','Costing.Zone','bldg.type','sim.sf',
                          'sim.vintage','Elect.EUI','Gas.EUI','sim.kWh','sim.therm','survey.cost','cb.survey.cost','cb.travel.cost'),with=F]
colnames(sim.frame.2.save)[grepl('.*EUI$',colnames(sim.frame.2.save))] <- paste0('sim.',colnames(sim.frame.2.save)[grepl('.*EUI$',colnames(sim.frame.2.save))])
# colnames(sim.frame.2.save)[grepl('.*cost$',colnames(sim.frame.2.save))] <- paste0('sim.',colnames(sim.frame.2.save)[grepl('.*cost$',colnames(sim.frame.2.save))])
# nrow(sim.frame[is.na(bldg.sz)])
saveRDS(sim.frame.2.save,file='sim_frame.rds')

# write.csv(sim.frame,file='sim_frame.csv')


## explore frame------------
{
  sim.frame.load <- readRDS('frame.rds')
  # sim.frame[FD.BLDG.TYPE=='UNKNOWN',.(length(RecordID)),by='bldg.type']
# sim.frame[is.na(SBW_Frame_Floor_Area),.(sum(BLD_SF)),by='bldg.type']
# sim.frame[is.na(Therm_SBWRevised),.(length(RecordID)),by='Gas.EUI']
# cbsa.1[is.na(Therm_SBWRevised),.(length(CBSA.Rec.ID)),by='Gas.EUI']

## compare stats of simulated frame to CBSA ------------
agg.cmpr <- sim.frame[,.(rec.cnt=length(RecordID),avg.sf=mean(BLD_SF),cv.sf=sd(BLD_SF)/mean(BLD_SF)),
                      by=c('Density','bldg.type')][cbsa.1[,.(cbsa.rec.cnt=length(CBSA.Rec.ID),cbsa.avg.sf=mean(BLD_SF),
                            cbsa.cv.sf=sd(BLD_SF)/mean(BLD_SF)),by=c('Density','bldg.type')],on=c('Density','bldg.type')]
agg.cmpr[,pct.diff.avg.sf:=signif(abs(1-avg.sf/cbsa.avg.sf)*100,3)]
agg.cmpr[,pct.diff.cv.sf:=signif(abs(1-cv.sf/cbsa.cv.sf)*100,3)]
setorder(agg.cmpr,pct.diff.cv.sf)
# hist(agg.cmpr$pct.diff.avg.sf)
# hist(agg.cmpr$pct.diff.cv.sf)
# hist(cbsa.1[Density=='Urban' & bldg.type=='Office']$BLD_SF)
# hist(sim.frame[Density=='Urban' & bldg.type=='Office']$BLD_SF)
# RoundNear((1-quantile(sim.frame[Density=='Urban' & bldg.type=='Office']$BLD_SF)/quantile(cbsa.1[Density=='Urban' & bldg.type=='Office']$BLD_SF)),0.05)
# RoundNear((1-quantile(sim.frame$BLD_SF)/quantile(cbsa.1$BLD_SF)),0.05)
# RoundNear((1-quantile(sim.frame[Density=='Urban' & bldg.type=='Office']$sim.sf)/quantile(cbsa.1[Density=='Urban' & bldg.type=='Office']$BLD_SF)),0.05)
# RoundNear((1-quantile(sim.frame$sim.sf)/quantile(cbsa.1$BLD_SF)),0.05)
# hist(sim.frame[Density=='Urban' & bldg.type=='Office']$sim.sf)
# hist(cbsa.1[Density=='Urban' & bldg.type=='Office']$BLD_SF)
# 
# 
# ## compare stats of original frame to CBSA -------------
cbsa.2 <- copy(cbsa.1)
cbsa.2[,SBW_Frame_Floor_Area:=BLD_SF]
agg.cmpr.frame <- frame.2[!(is.na(SBW_Frame_Floor_Area)) & FD.BLDG.TYPE!='UNKNOWN',.(rec.cnt=length(RecordID),avg.sf=mean(SBW_Frame_Floor_Area),cv.sf=sd(SBW_Frame_Floor_Area)/mean(SBW_Frame_Floor_Area)),
                      by=c('Density','bldg.type')][cbsa.2[,.(cbsa.rec.cnt=length(CBSA.Rec.ID),cbsa.avg.sf=mean(SBW_Frame_Floor_Area),
                        cbsa.cv.sf=sd(SBW_Frame_Floor_Area)/mean(SBW_Frame_Floor_Area)),by=c('Density','bldg.type')],
                    on=c('Density','bldg.type')]
agg.cmpr.frame[,pct.diff.avg.sf:=signif(abs(1-avg.sf/cbsa.avg.sf)*100,3)]
agg.cmpr.frame[,pct.diff.cv.sf:=signif(abs(1-cv.sf/cbsa.cv.sf)*100,3)]
setorder(agg.cmpr.frame,pct.diff.cv.sf)
# hist(agg.cmpr.frame$pct.diff.avg.sf)
# hist(agg.cmpr.frame$pct.diff.cv.sf)
## >>>>>  Conclude simulated frame stats compare favorably to CBSA stats?

## which CBSA records were not applied in the simulated frame? ----------
cbsa.not.in.frame <- copy(cbsa.1[!(CBSA.Rec.ID %in% sim.frame$CBSA.Rec.ID)])
write.csv(cbsa.not.in.frame,file='cbsa not used in frame.csv')
setorder(cbsa.not.in.frame,-BLD_SF)
head(cbsa.not.in.frame,20)
cmpr.missed.rec <- merge(cbsa.1[,.(rec.cnt=length(CBSA.Rec.ID)),by=c('Density','bldg.type')],cbsa.not.in.frame[,.(rec.miss.cnt=length(CBSA.Rec.ID)),
                                                                              by=c('Density','bldg.type')],by=c('Density','bldg.type'),all.x=T)
cmpr.missed.rec$rec.miss.cnt <- as.character(cmpr.missed.rec$rec.miss.cnt)
cmpr.missed.rec[is.na(rec.miss.cnt),rec.miss.cnt:='all represented']
setorder(cmpr.missed.rec,rec.miss.cnt,rec.cnt)
sum(cbsa.not.in.frame$BLD_SF)/sum(cbsa.1$BLD_SF)

# setorder(sim.frame,-BLD_SF)
# head(sim.frame[cty_name=='KING' & bldg.type=='University'],12)
# setorder(sim.frame,RecordID)
# head(sim.frame)
# cbsa.1[CBSA.Rec.ID==2128]

}