# setwd("~/_SBW/NEEA22 CBSA 04 Assess Alt Designs/Tool")
## working in SD2 branch

#setwd("P:/Projects/NEEA22 (CBSA Phase 1 Planning)/_04 Assessment of Alternative Designs/Tool/FaithSD2")

# install.packages('data.table')
library(data.table)
# install.packages("plyr")
library(plyr)
# <<<<<<< HEAD
# install.packages('XLConnectJars')
# library(XLConnectJars)
# install.packages('XLConnect')
# =======
# >>>>>>> 35a6cf44a3e71da1217f2c397bedbe42e992d55b
# library(XLConnect)

source('Sample Tool.R')

smpl.frame <- readRDS('sim_frame.rds')
# write.csv(smpl.frame,file='sim_frame.csv')
cb.smry <- smpl.frame[,.(rec.ct=length(RecordID)),by='Census.ID']
sum(cb.smry$rec.ct)/nrow(cb.smry)
# P:\Projects\NEEA22 (CBSA Phase 1 Planning)\_04 Assessment of Alternative Designs\Tool\SD2\test_designframe.csv

# set inputs for sample design [THESE NEED TO BE GLOBALLY DEFINED?]--------
# overhead cost
fname.cost <- './CBSA Data Collection Cost.xlsx'
## building survey costs
#oh.cost <- data.table(read.xlsx(fname.cost,sheet='for R - OH costs',colNames = T))
oh.cost <- data.table(readWorksheetFromFile(fname.cost,sheet='for R - OH costs',header = T))

Cost.OH <- 6e5 #oh.cost$base.cost

# define max sampling fraction
max.n.frac <- 0.5


## all this stuff is to design a sample, in lieu of using the web app -------------


## test sample designs--------
{
# get top utilities by largest total floor area in frame
# top.utils <- smpl.frame[,.(rec.cnt=length(RecordID),tot.sf=sum(sim.sf)),by='OverSampleUtility'][tot.sf>50e6]
# # setorder(top.utils,-tot.sf)
# smpl.frame[,Utility:=OverSampleUtility]
# # reassign remaining utilities to BPA
# smpl.frame[!(OverSampleUtility %in% top.utils$OverSampleUtility),Utility:='Bonneville Power Administration']
# smpl.frame[,colnames(smpl.frame)[19:29]:=NULL]
# domain.cols <- c('stabbr','Density','bldg.type')
# smpl.frame[,domain:=paste(get('stabbr'),get('Pub.Priv'),get('Density'),get('bldg.type'),sep='_')]
# # reassign domains, defined as pop < 20, to one domain
# small.domains <- smpl.frame[,.(rec.cnt=length(RecordID)),by='domain'][rec.cnt<50]
# smpl.frame[domain %in% small.domains$domain,domain:='Too Small to Stratify']
# # head(smpl.frame)
# # smpl.frame[,.(length(RecordID)),by='domain']
# domains <- unique(smpl.frame$domain)

  smpl.frame[,domain:=NULL]
smpl.frame[bldg.type=='Office' & Pub.Priv=='Private',.(rec.ct=length(RecordID),tot.sf=sum(sim.sf))]
smpl.frame[,domain:=bldg.type]  
smpl.design <- smpl.frame[,.(Population=length(RecordID),Tot_SF=sum(sim.sf),Avg_SF=mean(sim.sf),CV_SF=sd.pop(sim.sf)/mean(sim.sf)),
                          by='domain']
target.conf <- rep(0.9,nrow(smpl.design))
target.prec <- rep(0.1,nrow(smpl.design))
target.cost <- rep(1e5,nrow(smpl.design))
optim.param <- rep('Cost',nrow(smpl.design))
# head(smpl.design)
smpl.design$targ.conf <- target.conf
smpl.design$opt.param <- optim.param
smpl.design$value <- target.cost
# smpl.design$value <- target.prec


# smpl.design[stabbr %in% c('ID','MT') & domain != 'Too Small to Stratify',opt.param:='cost']
# smpl.design[stabbr %in% c('ID','MT') & domain != 'Too Small to Stratify',value:=27e3]
# smpl.design[domain == 'Too Small to Stratify',opt.param:='cost']
# smpl.design[domain == 'Too Small to Stratify',value:=27e3]
smpl.design[,c('RP_SF','RP_kWh','RP_Thrm','RP_E_EUI','RP_G_EUI','RP_Yr_Built','Cost','SampleSize'):=0]
# smpl.design[Tot_SF<5e6 & opt.param=='precision',targ.conf:=0.8]
# smpl.design[Tot_SF<5e6 & opt.param=='precision',value:=0.2]
# smpl.design[,.(length(domain)),by=c('opt.param','targ.conf','value')]

smpl.frame[,.(rec.ct=length(RecordID),tot.sf=sum(sim.sf),avg.sf=mean(sim.sf),cv.sf=sd.pop(sim.sf)/mean(sim.sf),
              tot.cost=sum(survey.cost),avg.cost=mean(survey.cost),cv.cost=sd.pop(survey.cost)/mean(survey.cost)),by='domain'][,
            .(pct.ct=rec.ct/sum(rec.ct),pct.sf=tot.sf/sum(tot.sf),pct.avg.sf=avg.sf/(sum(tot.sf)/sum(rec.ct)),
              pct.cost=tot.cost/sum(tot.cost),pct.avg.cost=avg.cost/(sum(tot.cost)/sum(rec.ct)))]

### get results of sample design-------
rm(frame,design)
frame <- copy(smpl.frame)
design <- copy(smpl.design)
tail(frame)
tail(design)
system.time(output <- size.stratify(frame=copy(smpl.frame),design=copy(smpl.design[,c('domain','targ.conf','opt.param','value'),with=F])))
output[,RP_sf:=round(signif(RP_sf,3),2)]
output[,RP_vintage:=round(signif(RP_vintage,3),3)]
output[,RP_Elect.EUI:=round(signif(RP_Elect.EUI,3),2)]
output[,RP_Gas.EUI:=round(signif(RP_Gas.EUI,3),2)]
output[,RP_kWh:=round(signif(RP_kWh,3),2)]
output[,RP_therm:=round(signif(RP_therm,3),2)]
output[,.(tot.smpl.sz=sum(SampleSize),tot.cost=sum(Cost))]
output[is.nan(RP_SF)]
output[,.(mean(RP_SF),mean(Cost),max(Cost),mean(Sample_Size),max(Sample_Size)),by=c('targ.conf','opt.param','value')]
summary(output)
}## end of testing sample design

## import design from web app -----------
{
# web.frame <- data.table(read.csv('P:/Projects/NEEA22 (CBSA Phase 1 Planning)/_04 Assessment of Alternative Designs/Tool/SD2/test_designframe.csv'))
# colnames(web.frame)[grepl('BLD_SF',colnames(web.frame))] <- 'sim.sf'
# colnames(web.frame)[grepl('Vintage_SBWRevised',colnames(web.frame))] <- 'sim.vintage'
# colnames(web.frame)[grepl('kWh_SBWRevised',colnames(web.frame))] <- 'sim.kWh'
# colnames(web.frame)[grepl('Therm_SBWRevised',colnames(web.frame))] <- 'sim.therm'
# saveRDS(web.frame,file='test_designframe.rds')

web.frame <- readRDS('test_designframe.rds')
web.design <- data.table(read.csv('test_tmp.csv'))
# web.design <- web.design[web.frame[,.(Population=length(RecordID)),by='domain'],on='domain']
# web.design$Population <- web.design$i.Population
# web.design[,i.Population:=NULL]
# 
# web.frame.1 <- copy(web.frame[,colnames(web.frame)[c(2:6,8:17,22)],with=F])
# web.design.1 <- copy(web.design[,colnames(web.design)[c(21,6,10:19)],with=F])
web.o <- size.stratify(frame=copy(web.frame[,2:22,with=F]),design=copy(web.design[,2:5,with=F]))

frame <- copy(web.frame)
design <- copy(web.design)
}

## oversample design -----------
smpl.design.os <- smpl.frame[,.(Population=length(RecordID),Tot_SF=sum(sim.sf),Avg_SF=mean(sim.sf),CV_SF=sd.pop(sim.sf)/mean(sim.sf)),
                          by=c('OverSampleUtility','bldg.type','domain')]



## create frame for 1st stage of 2 stage design, -----------
## i.e., aggregate to census block level, drop all building-specific info
cb.frame <- smpl.frame[,.(bldg.count=length(RecordID)),by=c('Census.ID','OverSampleUtility','Density','stabbr','cty_name','cb.survey.cost')]
# uniqueN(cb.frame,by='Census.ID')
# sum(cb.frame$bldg.count)
cb.frame[,domain:=OverSampleUtility]
# cb.frame[OverSampleUtility=='Bonneville Power Administration',domain:=paste(get('stabbr'),get('OverSampleUtility'),sep='_')]
# cb.frame[,domain:=paste(get('stabbr'),get('Density'),sep='_')]
# cb.frame[,domain:=stabbr]
# domains.1 <- unique(cb.frame$domain)

# cb.frame.smry <- cb.frame[,.(cb.ct=length(Census.ID),bldg.ct=sum(bldg.count)),by='domain']
# setorder(cb.frame.smry,-cb.ct)

smpl.design.1 <- cb.frame[,.(Population=length(Census.ID),Tot_Bldgs=sum(bldg.count),Avg_SF=mean(bldg.count),CV_SF=sd.pop(bldg.count)/mean(bldg.count)),
                          by=c('domain')]
target.conf.1 <- rep(0.8,nrow(smpl.design.1))
target.prec.1 <- rep(0.2,nrow(smpl.design.1))
# head(smpl.design)
smpl.design.1$targ.conf <- target.conf.1
smpl.design.1$targ.rp <- target.prec.1
# smpl.design.1[grepl('Bonneville Power Administration',domain),targ.conf:=0.8]
# smpl.design.1[grepl('Bonneville Power Administration',domain),targ.prec:=0.2]
smpl.design.1[,c('Cost','Sample_Size'):=0]
# smpl.design.1[,.(length(domain)),by=c('domain','targ.conf','value')]

### get results of 1st stage sample design
# frame.1 <- copy(cb.frame)
# design.1 <- copy(smpl.design.1)

output.1 <- srs(frame.1=copy(cb.frame),design.1=copy(smpl.design.1))
smpl.frame.2 <- copy(smpl.frame)
# colnames(smpl.frame.2)
smpl.frame.2 <- smpl.frame.2[Census.ID %in% output.1]
smpl.design.1 <- smpl.design.1[smpl.frame.2[,.(cb.cost=sum(cb.survey.cost)),by='stabbr'],on='domain']

saveRDS(smpl.frame.2,file='frame_2.rds')
old.cb.frame <- readRDS('frame_2.rds')


smpl.frame.2 <- readRDS('frame_2.rds')
