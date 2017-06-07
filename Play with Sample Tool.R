# setwd("~/_SBW/NEEA22 CBSA 04 Assess Alt Designs/Tool")
## working in SD3 branch

setwd("P:/Projects/NEEA22 (CBSA Phase 1 Planning)/_04 Assessment of Alternative Designs/Tool/FaithSD3")

library(lattice)
# install.packages('data.table')
library(data.table)
# install.packages("plyr")
library(plyr)
# install.packages("ggplot2")
library(ggplot2)
library(openxlsx)
oh.cost <- data.table(read.xlsx('./CBSA Data Collection Cost.xlsx',sheet='for R - OH costs',colNames = T))
# oh.cost <- data.table(readWorksheetFromFile('CBSA Data Collection Cost.xlsx',sheet='for R - OH costs',header = T))
Cost.OH <- oh.cost$base.cost
Cost.OH.1st.stage <- oh.cost$cost.1st.stage
Cost.1.FE <- oh.cost$cost.frm.enhance.1.stage
Cost.2.FE <- oh.cost$cost.frm.enhance.2.stage
# <<<<<<< HEAD
# install.packages('XLConnectJars')
# library(XLConnectJars)
# install.packages('XLConnect')
# =======
# >>>>>>> 35a6cf44a3e71da1217f2c397bedbe42e992d55b
# library(XLConnect)

# source('Sample Tool.R')
# source('clean size.stratify.R')

smpl.frame <- readRDS('sim_frame.rds')
smpl.frame.0 <- copy(smpl.frame)
smpl.frame.1 <- copy(smpl.frame)
smpl.frame.1[,domain:=bldg.type]
smpl.design <- smpl.frame.1[,.(Population=length(RecordID)),by='domain']
smpl.design$targ.conf <- 0.9
smpl.design$opt.param <- 'Precision'
smpl.design$value <- 0.1
smpl.design[,Population:=NULL]

#### Find lowest 2 stage budget----------
budget.1st.stage <- seq(from=1e4,to=2.5e5,by=2.5e4)
b.1 <- budget.1st.stage[6]
min.budget <- lapply(budget.1st.stage,function(b.1) {
  set.seed(444)
  results <- get.adj.factor(copy(smpl.frame.1),copy(smpl.design),b.1)
  # this is a global variable to populate the summary table
  totals <- results[,.(cb.n=head(cb.n,1),n.1.stage=sum(SampleSize),n.srs=head(n.srs,1),var.cost.1.stg=sum(Cost),base.cost.1.stg=Cost.OH,tot.cost.1.stage=sum(Cost)+Cost.OH,
                     frm.enh.1.stg=Cost.1.FE,tot.w.fe.1.stg=sum(Cost)+Cost.OH+Cost.1.FE,n.2.stage=sum(n.2),cost.1st.stg=head(cb.var.cst,1),cost.2nd.stg=sum(cost.2),
                     var.cost.2.stg=sum(cost.2)+head(cb.var.cst,1),base.cost.2.stg=Cost.OH+Cost.OH.1st.stage,
                     tot.cost.2.stage=sum(cost.2)+Cost.OH+Cost.OH.1st.stage,frm.enh.2.stg=Cost.2.FE,tot.w.fe.2.stg=sum(cost.2)+Cost.OH+Cost.OH.1st.stage+Cost.1.FE)]
  totals[,budg.targ:=b.1]
  return(totals[,c('budg.targ','cb.n','n.2.stage','var.cost.2.stg','cost.1st.stg','cost.2nd.stg'),with=F])
})
min.budget.1 <- data.table(rbind.fill.matrix(lapply(min.budget, rbind)))
for (i in 2:ncol(results.2)){
  results.2[,colnames(results.2)[i]:=as.numeric(get(colnames(results.2)[i]))]
}

smpl.frame.1[,domain:='All']
smpl.design <- smpl.frame.1[,.(Population=length(RecordID)),by='domain']
smpl.design$targ.conf <- 0.9
smpl.design$opt.param <- 'Precision'
smpl.design$value <- 0.1
smpl.design[,Population:=NULL]
smpl.1.stage <- size.stratify(copy(smpl.frame.1),copy(smpl.design),smpl.out = T)
smpl.1.stage[,.(N=length(RecordID),mean.sf=mean(sim.sf),sd.sf=sd.pop(sim.sf)),by='stratum']

## create frame for 1st stage of 2 stage design, -----------
## i.e., aggregate to census block level, drop all building-specific info
# smpl.frame <- readRDS('full_sim_frame.rds')

## assign certainty block groups
# fname.cert.cb <- 'P:/Projects/NEEA22 (CBSA Phase 1 Planning)/_04 Assessment of Alternative Designs/Report Chapter/FirstStageCertaintyCriteria.xlsx'
# cert.cb.crit <- data.table(readWorksheetFromFile(fname.cert.cb,sheet='SizeCriteria',header = T))
# cert.cb.crit <- cert.cb.crit[,2:3]
# colnames(cert.cb.crit) <- c('sf.limit','bldg.type')
# smpl.frame <- smpl.frame[cert.cb.crit,on='bldg.type']
# smpl.frame[,cert.cb:=F]
# smpl.frame[sim.sf>sf.limit,cert.cb:=T]
# smpl.frame[cert.cb==T,.(length(RecordID)),by='bldg.type']
# nrow(smpl.frame[cert.cb==T])
# smpl.frame[Census.ID %in% smpl.frame[cert.cb==T]$Census.ID,cert.cb:=T]
# cb.frame <- smpl.frame[,.(bldg.count=length(RecordID),sim.avg.sf=mean(sim.sf)),
#                        by=c('Census.ID','OverSampleUtility','Density','stabbr','cty_name','cert.cb','Costing.Zone','cb.survey.cost')]

## define certainty blocks
cb.frame <- smpl.frame[,.(bldg.count=length(RecordID),sim.avg.sf=mean(sim.sf),survey.cost=sum(cb.survey.cost)),
                       by=c('Census.ID','OverSampleUtility','Density','stabbr','cty_name','Costing.Zone','cb.travel.cost')]
cb.frame[,survey.cost:=survey.cost+cb.travel.cost]

cb.frame[,domain:='All']
# cb.smpl.frame <- copy(cb.frame[cert.cb==F])
cb.smpl.frame <- copy(cb.frame)

# nrow(cb.frame[cert.cb==T])
# nrow(smpl.frame[cert.cb==T])

## set up 1st stage of 2 stage sample design
cb.design <- cb.smpl.frame[,.(pop=length(Census.ID)),by='domain']
# head(smpl.design)
cb.design$targ.conf <- 0.9
cb.design$opt.param <- 'Cost'
cb.design$value <- 1e4

### set up 1 stage sample design
# smpl.frame.1 <- copy(smpl.frame.0)
# smpl.frame.1[,domain:=bldg.type]
# smpl.design <- smpl.frame.1[,.(Population=length(RecordID)),by='domain']
# smpl.design$targ.conf <- 0.9
# smpl.design$opt.param <- 'Precision'
# smpl.design$value <- 0.1
# smpl.design[,Population:=NULL]

num.strata <- 3
ID.field <- 'Census.ID'
sz.meas.param <- 'sim.avg.sf'


### Get Adjustment Factors------------

smpl.frame.0 <- copy(smpl.frame)

## Check that design effect converges to 1  for 'All' for large census block sample (stratified)-----------
smpl.frame.0[,domain:='All']
seed <- 444:643#1443
s <- seed[1]
target.cost <- c(1.8e5,5e5,1e6,2.4e6,4e6)
cost <- target.cost[1]
results.1 <- lapply(seed,function(s){
  results.0 <- lapply(target.cost,function(cost){
    cat(file=stderr(), "*1st stage target cost: ", unlist(cost),", seed: ", unlist(s),".\n")
    cb.design$value <- cost
    
    ## draw stage 1 sample
    set.seed(s)
    cb.smpl <- size.stratify(copy(cb.smpl.frame),copy(cb.design),num.strata,max.n.frac=1,ID.field,sz.meas.param,seed=s,smpl.out = T)
    # cb.smpl <- size.stratify(copy(cb.smpl.frame),copy(cb.design),max.n.frac=1,ID.field=ID.field,sz.meas.param=sz.meas.param,seed=s,smpl.out = F)
    # cb.smpl[,.(length(Census.ID)),by=c('stratum','sampled')]
    nrow(cb.smpl[sampled=='Y'])
    nrow(smpl.frame[Census.ID %in% cb.smpl[sampled=='Y']$Census.ID])
    cb.smpl[,N.cb.strat:=lapply(.SD,length),.SDcols='Census.ID',by=c('domain','stratum')]
    # cb.strat.smry <- cb.smpl[sampled=='Y',.(cb.strat.n=length(Census.ID),cb.strat.mean.sf=mean(sim.avg.sf),cb.strat.sd=sd.pop(sim.avg.sf)),by=c('domain','stratum','N.cb.strat')]
    # nrow(smpl.frame[Census.ID %in% cb.smpl[stratum==3]$Census.ID])
    
    cb.smpl.frame.1 <- merge(smpl.frame.0,cb.smpl[,c('Census.ID','stratum','sampled','RP_avg.sf','z'),with=F],by='Census.ID',all.x=T)
    cb.smpl.frame.1[,N.strat.1:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    cb.smpl.frame.1[,true.mean:=lapply(.SD,mean),.SDcols='sim.sf',by='domain']
    cb.smpl.frame.1 <- cb.smpl.frame.1[sampled=='Y']
    cb.smpl.frame.1[,strat.n.1:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    cb.smpl.frame.1[,strat.wt.1:=N.strat.1/strat.n.1]
    strat.smry.1 <- cb.smpl.frame.1[,.(wtd.sf.1=sum(strat.wt.1*sim.sf)),by=c('domain','z','stratum','N.strat.1','strat.n.1','RP_avg.sf','true.mean')]
    # strat.smry.1 <- strat.smry.1[cb.strat.smry,on=c('domain','stratum')]
    domain.smry.1 <- strat.smry.1[,.(wtd.sf.1=sum(wtd.sf.1),n.1=sum(strat.n.1),N.dom.1=sum(N.strat.1)),by=c('domain','z','RP_avg.sf','true.mean')]
    domain.smry.1[,wtd.avg.sf.1:=wtd.sf.1/N.dom.1]
    setnames(domain.smry.1,'RP_avg.sf','RP.1')
    domain.smry.1[,var.1:=(RP.1*true.mean/z)^2]
    # domain.smry.1$strat.3.n <- strat.smry.1[stratum==3]$strat.n.1
    domain.smry.1[,z:=NULL]
    
    ## draw stage 2 sample
    # Extract sampled census blocks from simulated frame
    smpl.frame.2 <- copy(smpl.frame.0[Census.ID %in% cb.smpl[sampled=='Y']$Census.ID])
    # now set up design - domains are building types
    smpl.design.2 <- smpl.frame.2[,.(Population=length(RecordID)),by='domain']
    smpl.design.2$targ.conf <- 0.9
    smpl.design.2$opt.param <- 'Precision'
    smpl.design.2$value <- 0.1
    smpl.design.2[,Population:=NULL]
    # now draw 2nd stage  sample
    set.seed(s)
    bldg.smpl <- size.stratify(copy(smpl.frame.2),copy(smpl.design.2),num.strata,seed=s,smpl.out = T)
    bldg.smpl[,N.strat.2:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    bldg.smpl <- bldg.smpl[cb.smpl.frame.1[,c('RecordID','strat.wt.1'),with=F],on='RecordID']
    # bldg.smpl[,.(strat.mean.sf=mean(sim.sf),strat.sd=sd.pop(sim.sf),N=length(RecordID)),by=c('domain','stratum')]
    
    bldg.smpl <- bldg.smpl[sampled=='Y']
    bldg.smpl[,strat.n.2:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    bldg.smpl[,strat.wt.2:=N.strat.2/strat.n.2]
    bldg.smpl[,wtd.sf.2:=strat.wt.1*strat.wt.2*sim.sf]
    strat.smry.2 <- bldg.smpl[,.(wtd.sf.2=sum(wtd.sf.2),unwtd.strat.mean.sf.2=mean(sim.sf),strat.sd.2=sd.pop(sim.sf)),by=c('domain','z','stratum','strat.n.2','N.strat.2','RP_sf')]
    domain.smry.2 <- strat.smry.2[,.(wtd.sf.2=sum(wtd.sf.2),n.2=sum(strat.n.2),N.dom.2=sum(N.strat.2)),by=c('domain','RP_sf','z')]
    for (i in 2:ncol(domain.smry.1)){
      domain.smry.2[,colnames(domain.smry.1)[i]:=domain.smry.1[,get(colnames(domain.smry.1)[i])]]
    }
    # domain.smry.2 <- domain.smry.2[domain.smry.1,on='domain']
    domain.smry.2[,wtd.avg.sf.2:=wtd.sf.2/N.dom.1]
    setnames(domain.smry.2,'RP_sf','RP.2')
    domain.smry.2[,var.2:=(RP.2*true.mean/z)^2]
    # strat.smry <- strat.smry.1[strat.smry.2,on=c('domain','stratum','z')]
    # setnames(strat.smry,c('RP_avg.sf','RP_sf'),c('RP.1','RP.2'))
    # smry.compare <- strat.smry[domain.smry.2[,c('domain','var.1','var.2','wtd.avg.sf.1','wtd.avg.sf.2'),with=F],on='domain']
    smry.compare <- copy(domain.smry.2)
    smry.compare[,cb.n:=nrow(cb.smpl[sampled=='Y'])]
    
    smry.compare[,targ.cost:=cost]
    return(smry.compare)
  })
  tmp.results <- data.table(rbind.fill.matrix(lapply(results.0, rbind)))
  tmp.results[,seed:=s]
  return(tmp.results)
})
results.2 <- data.table(rbind.fill.matrix(lapply(results.1, rbind)))
for (i in 2:ncol(results.2)){
  results.2[,colnames(results.2)[i]:=as.numeric(get(colnames(results.2)[i]))]
}
# write.csv(results.2,'wtd mean and var results from 6000 runs.csv')
results.strat.smry <-results.2[,.(N.cb.strat=round(mean(N.cb.strat),0),N.strat.1=round(mean(N.strat.1),0),cb.strat.n=round(mean(cb.strat.n),0),
                                  strat.n.1=round(mean(strat.n.1),0),cb.strat.mean=mean(cb.strat.mean.sf),cb.strat.sd=mean(cb.strat.sd),
                                  N.strat.2=round(mean(N.strat.2),0),strat.n.2=round(mean(strat.n.2),0),strat.mean.2=mean(unwtd.strat.mean.sf.2),
                                  strat.sd.2=mean(strat.sd.2)),by=c('targ.cost','stratum')]
setorder(results.strat.smry,targ.cost,stratum)
results.strat.smry

results.3 <- results.2[,.(N.cb=sum(N.cb.strat),N.1=sum(N.strat.1),cb.n=sum(cb.strat.n),wtd.sf.1=sum(wtd.sf.1),N.2=sum(N.strat.2),
                          n.2nd=sum(strat.n.2),wtd.sf.2=sum(wtd.sf.2)),by=c('domain','targ.cost','seed','z','var.1','var.2','wtd.avg.sf.1','wtd.avg.sf.2')]
results.smry <-results.3[,.(N.cb=round(mean(N.cb),0),N.1=mean(N.1),cb.n=round(mean(cb.n),0),N.2=round(mean(N.2),0),wtd.mean.1=mean(wtd.avg.sf.1),
                            n.2nd=round(mean(n.2nd),0),wtd.mean.2=mean(wtd.avg.sf.2),Var.2.stage=sd(wtd.avg.sf.1)^2+sd(wtd.avg.sf.2)^2),
                         by=c('domain','targ.cost')]

smry <- smpl.frame.0[,.(true.mean=mean(sim.sf),N=length(RecordID),tot.sf=sum(sim.sf)),by='domain']

smpl.frame.1 <- copy(smpl.frame.0)
smpl.frame.1[,domain:='All']
smpl.design <- smpl.frame.1[,.(Population=length(RecordID)),by='domain']
smpl.design$targ.conf <- 0.9
smpl.design$opt.param <- 'Precision'
smpl.design$value <- 0.1
smpl.design[,Population:=NULL]
smpl.1.stage <- size.stratify(copy(smpl.frame.1),copy(smpl.design),smpl.out = F)
smpl.1.stage[,.(tot.cost=sum(Cost),tot.sample.size=sum(SampleSize))]
smpl.1.stage <- smpl.1.stage[smry,on='domain']
smpl.1.stage[,Var.1.stage.CF:=(RP_sf/1.645*true.mean)^2]
smpl.1.stage.recs <- size.stratify(copy(smpl.frame.1),copy(smpl.design),smpl.out = T)
smpl.1.stage.recs[,.(length(RecordID)),by=c('sampled','stratum')][sampled=='Y']

results.smry <- results.smry[smpl.1.stage[,c('domain','Var.1.stage.CF','RP_sf','true.mean'),with=F],on='domain']
results.smry[,des.eff:=Var.2.stage/Var.1.stage.CF]
results.smry[,first.stg.smpl.frac:=N.2/nrow(smpl.frame.0)]
results.smry[,n.1.stage:=smpl.1.stage$SampleSize]
results.smry[,n.adj.fact:=n.2nd/n.1.stage]
keep.strat.all.des.eff <- copy(results.smry)
# write.csv(keep.strat.all.des.eff,'2 stage sample size adjustment factors.csv')

to.fit <- copy(results.smry[,c('first.stg.smpl.frac','n.adj.fact'),with=F])
colnames(to.fit) <- c('x','y')
fit.log <- nls(y ~ a*log(x)+b,data=to.fit,start=list(a=1,b=1))
a <- coef(fit.log)[1]
b <- coef(fit.log)[2]
x <- seq(from=0.05,to=1,by=0.05)
plot(to.fit,xlab='1st stage sampling fraction',ylab='2 stage sample size adjustment',main=paste0('y = ',a,' *log(x)+',b))
lines(x,a*log(x)+b,col='blue')

histogram(~sim.sf|paste0('stratum=',stratum),data=cb.smpl.frame.1, layout = c(1,3),main='Building Floor Area distribution by Census Block Group stratum')

# fit.power <- nls(y ~ a/(x^b),data=to.fit,start=list(a=1,b=1))
# a <- coef(fit.power)[1]
# b <- coef(fit.power)[2]
# x <- seq(from=0.05,to=1,by=0.05)
# plot(to.fit,xlab='1st stage sampling fraction',ylab='2 stage sample size adjustment')
# lines(x,a/(x^b))

tmp.out <- size.stratify(copy(smpl.frame.1),copy(smpl.design),smpl.out = T)
tmp.out$RecordID <- as.integer(as.character(tmp.out$RecordID))
# write.csv(tmp.out[,c('RecordID','stratum'),with=F][tmp.out[sampled=='Y',.(n=length(RecordID)),by='stratum'],on='stratum'],file='1_stage_size_strat_AllBT.csv')

setnames(cb.smpl,c('stratum','sampled'),c('cb.stratum','cb.sampled'))
tmp.out <- smpl.1.stage[cb.smpl[,c('Census.ID','cb.stratum','cb.sampled'),with=F],on='Census.ID']
colnames(tmp.out)
head(tmp.out)
write.csv(tmp.out[,c(1,5,9:10,19:20,26:27,36:37),with=F],file='frame for Rick 2017-05-08.csv')

## SRS instead of size stratified 'All' ------------
# smpl.frame.0[,domain:=bldg.type]
smpl.frame.0[,domain:='All']
# smpl.frame.0[,.(length(RecordID)),by='domain']
seed <- 444:643
s <- seed[1]
target.cost <- c(3.05e5,8.25e5,2.3e6)
cost <- target.cost[1]
num.strata <- 1
results.1 <- lapply(seed,function(s){
  results.0 <- lapply(target.cost,function(cost){
    cat(file=stderr(), "*1st stage target cost: ", unlist(cost),", seed: ", unlist(s),".\n")
    smpl.design.1$value <- cost
    
    ## draw stage 1 sample
    set.seed(s)
    cb.smpl <- size.stratify(copy(cb.smpl.frame),copy(smpl.design.1),num.strata,ID.field,sz.meas.param,seed=s,smpl.out = T)
    nrow(cb.smpl[sampled=='Y'])
    nrow(smpl.frame[Census.ID %in% cb.smpl[sampled=='Y']$Census.ID])
    
    cb.smpl.frame.1 <- merge(smpl.frame.0,cb.smpl[,c('Census.ID','stratum','sampled','RP_avg.sf','z'),with=F],by='Census.ID',all.x=T)
    # cb.smpl.frame.1[,domain:=bldg.type]
    # cb.smpl.frame.1[,domain:='All']
    cb.smpl.frame.1[,N.strat.1:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    cb.smpl.frame.1[,mean.sf:=lapply(.SD,mean),.SDcols='sim.sf',by='domain']
    cb.smpl.frame.1 <- cb.smpl.frame.1[sampled=='Y']
    cb.smpl.frame.1[,strat.n.1:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    cb.smpl.frame.1[,strat.wt.1:=N.strat.1/strat.n.1]
    strat.smry.1 <- cb.smpl.frame.1[,.(wtd.sf.1=sum(strat.wt.1*sim.sf)),by=c('domain','z','stratum','N.strat.1','strat.n.1','RP_avg.sf','mean.sf')]
    domain.smry.1 <- strat.smry.1[,.(wtd.sf.1=sum(wtd.sf.1),n.1=sum(strat.n.1),N.dom.1=sum(N.strat.1)),by=c('domain','z','RP_avg.sf','mean.sf')]
    domain.smry.1[,wtd.avg.sf.1:=wtd.sf.1/N.dom.1]
    setnames(domain.smry.1,'RP_avg.sf','RP.1')
    domain.smry.1[,var.1:=(RP.1*mean.sf/z)^2]
    domain.smry.1[,z:=NULL]
    
    ## draw stage 2 sample
    # Extract sampled census blocks from simulated frame
    smpl.frame.2 <- copy(smpl.frame.0[Census.ID %in% cb.smpl[sampled=='Y']$Census.ID])
    # now set up design - domains are building types
    # smpl.frame.2[,domain:=bldg.type]
    smpl.frame.2[,domain:='All']
    smpl.design.2 <- smpl.frame.2[,.(Population=length(RecordID)),by='domain']
    smpl.design.2$targ.conf <- 0.9
    smpl.design.2$opt.param <- 'Precision'
    smpl.design.2$value <- 0.1
    smpl.design.2[,Population:=NULL]
    # now draw 2nd stage  sample
    set.seed(s)
    bldg.smpl <- size.stratify(copy(smpl.frame.2),copy(smpl.design.2),num.strata,seed=s,smpl.out = T)
    bldg.smpl[,N.strat.2:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    # bldg.smpl <- bldg.smpl[cb.smpl[,c('Census.ID','strat.wt.1'),with=F],on='Census.ID']
    bldg.smpl <- bldg.smpl[cb.smpl.frame.1[,c('RecordID','strat.wt.1'),with=F],on='RecordID']
    
    bldg.smpl <- bldg.smpl[sampled=='Y']
    bldg.smpl[,strat.n.2:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    bldg.smpl[,strat.wt.2:=N.strat.2/strat.n.2]
    bldg.smpl[,wtd.sf.2:=strat.wt.1*strat.wt.2*sim.sf]
    strat.smry.2 <- bldg.smpl[,.(wtd.sf.2=sum(wtd.sf.2)),by=c('domain','z','stratum','strat.n.2','N.strat.2','RP_sf')]
    domain.smry.2 <- strat.smry.2[,.(wtd.sf.2=sum(wtd.sf.2),n.2=sum(strat.n.2),N.dom.2=sum(N.strat.2)),by=c('domain','RP_sf','z')]
    for (i in 2:ncol(domain.smry.1)){
      domain.smry.2[,colnames(domain.smry.1)[i]:=domain.smry.1[,get(colnames(domain.smry.1)[i])]]
    }
    # domain.smry.2 <- domain.smry.2[domain.smry.1,on='domain']
    domain.smry.2 <- domain.smry.2[smpl.frame.0[,.(sim.pop=length(RecordID)),by='domain'],on='domain']
    domain.smry.2[,wtd.avg.sf.2:=wtd.sf.2/sim.pop]
    setnames(domain.smry.2,'RP_sf','RP.2')
    domain.smry.2[,var.2:=(RP.2*mean.sf/z)^2]
    smry.compare <- copy(domain.smry.2)
    smry.compare[,cb.n:=nrow(cb.smpl[sampled=='Y'])]
    
    
    ## 1 stage design
    # set.seed(s)
    # smpl.1.stage <- size.stratify(copy(smpl.frame.1),copy(smpl.design),seed=s,smpl.out = T)
    # smpl.1.stage[,N.strat:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    # smpl.1.stage <- smpl.1.stage[sampled=='Y']
    # smpl.1.stage[,strat.n:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    # smpl.1.stage[,strat.wt:=N.strat/strat.n]
    # smpl.1.stage[,wtd.sf:=strat.wt*sim.sf]
    # strat.smry <- smpl.1.stage[,.(wtd.sf=sum(wtd.sf)),by=c('domain','z','stratum','strat.n','N.strat','RP_sf')]
    # domain.smry <- strat.smry[,.(wtd.sf=sum(wtd.sf),n=sum(strat.n),N.dom=sum(N.strat)),by=c('domain','RP_sf','z')]
    # domain.smry[,wtd.avg.sf:=wtd.sf/N.dom]
    # setnames(domain.smry,'RP_sf','RP.1.stage')
    # domain.smry[,var.1.stage:=(RP.1.stage*N.dom/z)^2]
    # domain.smry[,z:=NULL]
    # 
    # smry.compare <- smry.compare[domain.smry,on='domain']
    
    smry.compare[,targ.cost:=cost]
    return(smry.compare)
  })
  tmp.results <- data.table(rbind.fill.matrix(lapply(results.0, rbind)))
  tmp.results[,seed:=s]
  return(tmp.results)
})
results.2 <- data.table(rbind.fill.matrix(lapply(results.1, rbind)))
for (i in 2:ncol(results.2)){
  results.2[,colnames(results.2)[i]:=as.numeric(get(colnames(results.2)[i]))]
}

results.smry <-results.2[,.(cb.n=round(mean(cb.n),0),mean.1=mean(wtd.avg.sf.1),avg.n.1=round(mean(n.1),0),N.1.avg=round(mean(N.dom.1),0),
                            mean.2=mean(wtd.avg.sf.2),avg.n.2=round(mean(n.2),0),N.2.avg=round(mean(N.dom.2),0),
                            Var.2.stage=sd(wtd.avg.sf.1)^2+sd(wtd.avg.sf.2)^2),by=c('domain','targ.cost')]

# smpl.frame.0[,domain:=bldg.type]
smpl.frame.0[,domain:='All']
smry <- smpl.frame.0[,.(true.mean=mean(sim.sf),N=length(RecordID),tot.sf=sum(sim.sf)),by='domain']

smpl.frame.1 <- copy(smpl.frame.0)
smpl.frame.1[,domain:='All']
# smpl.frame.1[,domain:=bldg.type]
smpl.design <- smpl.frame.1[,.(Population=length(RecordID)),by='domain']
smpl.design$targ.conf <- 0.9
smpl.design$opt.param <- 'Precision'
smpl.design$value <- 0.1
smpl.design[,Population:=NULL]
smpl.1.stage <- size.stratify(copy(smpl.frame.1),copy(smpl.design),num.strata,smpl.out = F)
smpl.1.stage[,.(tot.cost=sum(Cost),tot.sample.size=sum(SampleSize))]
# smry <- smpl.frame.0[,.(mean.sf=mean(sim.sf)),'bldg.type']
# setnames(smry,'bldg.type','domain')
smpl.1.stage <- smpl.1.stage[smry,on='domain']
smpl.1.stage[,Var.1.stage.CF:=(RP_sf/1.645*true.mean)^2]

results.smry <- results.smry[smpl.1.stage[,c('domain','Var.1.stage.CF','RP_sf','true.mean'),with=F],on='domain']
results.smry[,des.eff:=Var.2.stage/Var.1.stage.CF]
keep.srs.des.eff <- copy(results.smry)

with( results.2[targ.cost==305000,c('wtd.avg.sf.1','wtd.avg.sf.2'),with=FALSE],cov(wtd.avg.sf.1,wtd.avg.sf.2))

## Check that design effect converges to 1  by building type for large census block sample (stratified)-----------
smpl.frame.0[,domain:=bldg.type]
smpl.frame.0[,.(mean.sf=mean(sim.sf),med.sf=median(sim.sf))]
seed <- 444:643
s <- seed[1]
target.cost <- c(3.8e5,1.25e6,3.4e6)
cost <- target.cost[3]
num.strata <- 3
results.1 <- lapply(seed,function(s){
  results.0 <- lapply(target.cost,function(cost){
    cat(file=stderr(), "*1st stage target cost: ", unlist(cost),", seed: ", unlist(s),".\n")
    smpl.design.1$value <- cost
    
    ## draw stage 1 sample
    set.seed(s)
    cb.smpl <- size.stratify(copy(cb.smpl.frame),copy(smpl.design.1),num.strata,ID.field,sz.meas.param,seed=s,smpl.out = T)
    # cb.smpl <- size.stratify(copy(cb.smpl.frame),copy(smpl.design.1),num.strata=1,ID.field,sz.meas.param,seed=s,smpl.out = T)
    nrow(cb.smpl[sampled=='Y'])
    nrow(smpl.frame[Census.ID %in% cb.smpl[sampled=='Y']$Census.ID])
    
    cb.smpl.frame.1 <- merge(smpl.frame.0,cb.smpl[,c('Census.ID','stratum','sampled','RP_avg.sf','z'),with=F],by='Census.ID',all.x=T)
    cb.smpl.frame.1[,N.strat.1:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    cb.smpl.frame.1[,mean.sf:=lapply(.SD,mean),.SDcols='sim.sf',by='domain']
    cb.smpl.frame.1 <- cb.smpl.frame.1[sampled=='Y']
    cb.smpl.frame.1[,strat.n.1:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    cb.smpl.frame.1[,strat.wt.1:=N.strat.1/strat.n.1]
    strat.smry.1 <- cb.smpl.frame.1[,.(wtd.sf.1=sum(strat.wt.1*sim.sf)),by=c('domain','z','stratum','N.strat.1','strat.n.1','RP_avg.sf','mean.sf')]
    domain.smry.1 <- strat.smry.1[,.(wtd.sf.1=sum(wtd.sf.1),n.1=sum(strat.n.1),N.dom.1=sum(N.strat.1)),by=c('domain','z','RP_avg.sf','mean.sf')]
    domain.smry.1[,wtd.avg.sf.1:=wtd.sf.1/N.dom.1]
    setnames(domain.smry.1,'RP_avg.sf','RP.1')
    domain.smry.1[,var.1:=(RP.1*mean.sf/z)^2]
    domain.smry.1[,z:=NULL]
    
    ## draw stage 2 sample
    # Extract sampled census blocks from simulated frame
    smpl.frame.2 <- copy(smpl.frame.0[Census.ID %in% cb.smpl[sampled=='Y']$Census.ID])
    # now set up design - domains are building types
    smpl.design.2 <- smpl.frame.2[,.(Population=length(RecordID)),by='domain']
    smpl.design.2$targ.conf <- 0.9
    smpl.design.2$opt.param <- 'Precision'
    smpl.design.2$value <- 0.1
    smpl.design.2[,Population:=NULL]
    # now draw 2nd stage  sample
    set.seed(s)
    bldg.smpl <- size.stratify(copy(smpl.frame.2),copy(smpl.design.2),num.strata,seed=s,smpl.out = T)
    bldg.smpl[,N.strat.2:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    bldg.smpl <- bldg.smpl[cb.smpl.frame.1[,c('RecordID','strat.wt.1'),with=F],on='RecordID']
    
    bldg.smpl <- bldg.smpl[sampled=='Y']
    bldg.smpl[,strat.n.2:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
    bldg.smpl[,strat.wt.2:=N.strat.2/strat.n.2]
    bldg.smpl[,wtd.sf.2:=strat.wt.1*strat.wt.2*sim.sf]
    strat.smry.2 <- bldg.smpl[,.(wtd.sf.2=sum(wtd.sf.2)),by=c('domain','z','stratum','strat.n.2','N.strat.2','RP_sf')]
    domain.smry.2 <- strat.smry.2[,.(wtd.sf.2=sum(wtd.sf.2),n.2=sum(strat.n.2),N.dom.2=sum(N.strat.2)),by=c('domain','RP_sf','z')]
    domain.smry.2 <- domain.smry.2[domain.smry.1,on='domain']
    domain.smry.2[,wtd.avg.sf.2:=wtd.sf.2/N.dom.1]
    setnames(domain.smry.2,'RP_sf','RP.2')
    domain.smry.2[,var.2:=(RP.2*mean.sf/z)^2]
    smry.compare <- copy(domain.smry.2)
    smry.compare[,N.1.bldgs:=nrow(smpl.frame.2)]
    
    smry.compare[,targ.cost:=cost]
    return(smry.compare)
  })
  tmp.results <- data.table(rbind.fill.matrix(lapply(results.0, rbind)))
  tmp.results[,seed:=s]
  return(tmp.results)
})
results.2 <- data.table(rbind.fill.matrix(lapply(results.1, rbind)))
for (i in 2:ncol(results.2)){
  results.2[,colnames(results.2)[i]:=as.numeric(get(colnames(results.2)[i]))]
}

results.smry <-results.2[,.(mean.1=mean(wtd.avg.sf.1),avg.n.1=mean(n.1),mean.2=mean(wtd.avg.sf.2),avg.n.2=mean(n.2),
                            Var.2.stage=sd(wtd.avg.sf.1)^2+sd(wtd.avg.sf.2)^2),by=c('domain','targ.cost')]


smry <- smpl.frame.0[,.(true.mean=mean(sim.sf),N=length(RecordID),tot.sf=sum(sim.sf)),by='domain']

smpl.frame.1 <- copy(smpl.frame.0)
smpl.frame.1[,domain:=bldg.type]
smpl.design <- smpl.frame.1[,.(Population=length(RecordID)),by='domain']
smpl.design$targ.conf <- 0.9
smpl.design$opt.param <- 'Precision'
smpl.design$value <- 0.1
smpl.design[,Population:=NULL]
smpl.1.stage <- size.stratify(copy(smpl.frame.1),copy(smpl.design),smpl.out = F)
smpl.1.stage[,.(tot.cost=sum(Cost),tot.sample.size=sum(SampleSize))]
smpl.1.stage <- smpl.1.stage[smry,on='domain']
smpl.1.stage[,Var.1.stage.CF:=(RP_sf/1.645*true.mean)^2]

results.smry <- results.smry[smpl.1.stage[,c('domain','Var.1.stage.CF','RP_sf','true.mean'),with=F],on='domain']
results.smry[,des.eff:=Var.2.stage/Var.1.stage.CF]
keep.des.eff.converge <- copy(results.smry)
keep.des.eff.converge[,.(sum(avg.n.2)),by='targ.cost']

keep.des.eff.converge[,c('domain','targ.cost','des.eff'),with=F]

# 
# smpl.1.stage[,N.strat.1:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
# smpl.1.stage <- smpl.1.stage[sampled=='Y']
# smpl.1.stage[,strat.n.1:=lapply(.SD,length),.SDcols='RecordID',by=c('domain','stratum')]
# smpl.1.stage[,strat.wt.1:=N.strat.1/strat.n.1]
# strat.smry.1.stg <- smpl.1.stage[,.(tot.sf=sum(sim.sf),wtd.sf.1=sum(strat.wt.1*sim.sf),strat.n.1=head(strat.n.1,1),N.strat.1=head(N.strat.1,1)),by=c('domain','stratum')]
#                                     # sd.strat.1=sd(sim.sf),V.strat=sum(strat.wt.1*sd(sim.sf))^2/sum(strat.n.1),V.strat.FPC=sum(strat.wt.1*sd(sim.sf)^2))
#                                  
# domain.smry.1.stg <- strat.smry.1.stg[,.(tot.sf=sum(tot.sf),wtd.sf.1=sum(wtd.sf.1),n.1=sum(strat.n.1),N.dom.1=sum(N.strat.1)),
#                                       by='domain']
# domain.smry.1.stg[,wtd.avg.sf.1.stg:=wtd.sf.1/N.dom.1]
# domain.smry.1.stg[,Var.1:=V.strat-V.strat.FPC]
# domain.smry.1.stg[,RP.1:=1.645*sqrt(Var.1)/tot.sf]
# smry[results.2[,.(mean.wtd.avg.1=mean(wtd.avg.sf.1),mean.wtd.avg.2=mean(wtd.avg.sf.2)),by='domain'],on='domain'][domain.smry.1.stg[,c('domain','wtd.avg.sf.1.stg'),with=F],on='domain']
# 
# domain.smry <- domain.smry[,Var.1:=tot.sf*RP_sf^2/z][results.2[,.(Var.2=sd(wtd.avg.sf.1)^2+sd(wtd.avg.sf.2)^2,avg.n.2=round(mean(n.2),0),
#                 N.2.avg=round(mean(N.dom.2),0),avg.RP.2=signif(mean(RP.2),2),wtd.avg.sf.1=mean(wtd.avg.sf.1),wtd.avg.sf.2=mean(wtd.avg.sf.2)),by='domain'],on='domain']
# domain.smry <- domain.smry[domain.smry.1.stg[,c('domain','wtd.avg.sf.1.stg','n.1','N.dom.1'),with=F],on='domain']
# domain.smry[,adj.factor:=Var.2/Var.1]
# domain.smry[,c('domain','n.1','N.dom.1','RP_sf','wtd.avg.sf.1.stg','avg.n.2','N.2.avg','avg.RP.2','wtd.avg.sf.1','wtd.avg.sf.2','adj.factor')]
## figure out adjustment factors
seed <- 444*(1:12)
target.cost <- seq(from=5e5, to=10e5,by=5e4)
# s <- 444
# cost <- 5e5
results.1 <- lapply(seed,function(s){
  results.0 <- lapply(target.cost,function(cost){
    cat(file=stderr(), "*1st stage target cost: ", unlist(cost),", seed: ", unlist(s),".\n")
    smpl.design.1$value <- cost

    cb.smpl <- size.stratify(copy(cb.smpl.frame),copy(smpl.design.1),num.strata,ID.field,sz.meas.param,seed=s,smpl.out = T)
    strata.smry <- cb.smpl[,.(N.strat=length(Census.ID)),by=c('domain','stratum')]
    strata.smry <- strata.smry[cb.smpl[sampled=='Y',.(strat.n=length(Census.ID),sd.strat.smpl=sd(sim.avg.sf),sum.sf=sum(sim.avg.sf)),by=c('domain','stratum')],on=c('domain','stratum')]
    strata.smry[,wtd.sf:=N.strat*sum.sf/strat.n]
    domain.smry <- cb.smpl[,.(sum.meas.sz=sum(sim.avg.sf,na.rm=T)),by=c('domain','z')]
    domain.smry <- domain.smry[strata.smry[,.(avg.sf=sum(wtd.sf)/sum(N.strat),V.strat=sum(N.strat*sd.strat.smpl)^2/sum(strat.n),V.strat.FPC=sum(N.strat*sd.strat.smpl^2)),by='domain'],on='domain']
    domain.smry[,SE:=sqrt(V.strat-V.strat.FPC)]
    domain.smry[,RP:=z/sum.meas.sz*SE]

    cb.smpl[,N.strat:=lapply(.SD,length),.SDcols='Census.ID',by='stratum']
    smpl.frame.1 <- smpl.frame.0[cb.smpl[,c('Census.ID','domain','z','stratum','N.strat','sampled')],on='Census.ID']
    smpl.frame.1[,sim.avg.sf:=lapply(.SD,mean),.SDcols='sim.sf',by=c('Census.ID','bldg.type')]
    BT.strata.smry <- smpl.frame.1[,.(N.BT.strat=length(Census.ID)),by=c('domain','stratum','bldg.type')]
    BT.strata.smry <- BT.strata.smry[smpl.frame.1[sampled=='Y',.(BT.strat.n=length(Census.ID),sd.BT.strat=sd(sim.avg.sf),BT.strat.sf=sum(sim.avg.sf)),by=c('domain','stratum','bldg.type')],on=c('domain','stratum','bldg.type')]
    BT.strata.smry[,BT.wtd.sf:=N.BT.strat*BT.strat.sf/BT.strat.n]
    BT.smry <- smpl.frame.1[,.(sum.meas.sz=sum(sim.avg.sf,na.rm=T)),by=c('domain','z','bldg.type')]
    BT.smry <- BT.smry[BT.strata.smry[,.(avg.sf=sum(BT.wtd.sf)/sum(N.BT.strat),V.strat=sum(N.BT.strat*sd.BT.strat)^2/sum(BT.strat.n),
                                         V.strat.FPC=sum(N.BT.strat*sd.BT.strat^2)),by=c('domain','bldg.type')],on=c('domain','bldg.type')]
    BT.smry[,SE:=sqrt(V.strat-V.strat.FPC)]
    BT.smry[,RP:=z/sum.meas.sz*SE]
    
    setnames(domain.smry,'domain','bldg.type')
    RPs <- rbind(domain.smry[,c('bldg.type','RP'),with=F],BT.smry[,c('bldg.type','RP'),with=F])
    RPs[,n.1:=nrow(cb.smpl[sampled=='Y'])]
    return(RPs)
  })
  tmp.results <- data.table(rbind.fill.matrix(lapply(results.0, rbind)))
  tmp.results[,seed:=s]
  return(tmp.results)
})

results.2 <- data.table(rbind.fill.matrix(lapply(results.1, rbind)))
str(results.2)
results.2$RP <- as.numeric(results.2$RP)
results.2$n.1 <- as.numeric(results.2$n.1)
results.2$seed <- as.numeric(results.2$seed)
xyplot(RP~n.1|bldg.type,data=results.2,ylab='First Stage RP',xlab='Census Blocks sampled')


# xyplot(RP_sf~n.1|domain,data=results.2,ylab='Second Stage RP (%)',xlab='Census Blocks sampled')
# 
# results.2[seed %in% c(444,888),.(rp.1=head(rp.1,2),n.1=head(n.1,2)),by=c('targ.cost')]
# results.2[seed ==444,c('domain','targ.cost','rp.1','n.1'),with=F]
# 
# stripplot(pct.dff~seed|domain,data=results.2)
# with(results.2[,.(rp.1=head(rp.1,1),n.1=head(n.1,1)),by=c('targ.cost','seed')],plot(n.1,rp.1))
# 
# with(results.2[domain=='University'],plot(n.1,RP_sf))
# results.2[domain=='University']
# 
# keep.results.2 <- copy(results.2)
# 
# write.csv(results.2,file='Step1_summary.csv')


# ## Step 2. Additive Errors method.-----------
# ## draw stage 1 sample
# stage.1 <- size.stratify(copy(cb.smpl.frame),copy(smpl.design.1),num.strata,ID.field,sz.meas.param,smpl.out = F)
# cb.smpl <- size.stratify(copy(cb.smpl.frame),copy(smpl.design.1),num.strata,ID.field,sz.meas.param,seed=333,smpl.out = T)
# cb.smpl[,N.strat.1:=lapply(.SD,length),.SDcols='Census.ID',by='stratum']
# smpl.frame.1 <- smpl.frame[cb.smpl[,c('Census.ID','N.strat.1','stratum','sampled'),with=F],on='Census.ID']
# colnames(smpl.frame.1)[(ncol(smpl.frame.1)-1):ncol(smpl.frame.1)] <- c('cb.stratum','cb.sampled')
# cb.smpl <- cb.smpl[sampled=='Y']
# cb.smpl[,strat.n.1:=lapply(.SD,length),.SDcols='Census.ID',by='stratum']
# cb.smpl[,strat.wt.1:=N.strat.1/strat.n.1]
# ## draw stage 2 sample
# # read in simulated frame
# smpl.frame.2 <- readRDS('sim_frame.rds')
# # Extract sampled census blocks from simulated frame
# # smpl.frame.2 <- smpl.frame.2[Census.ID %in% cb.smpl$Census.ID]
# # now set up design - domains are building types
# smpl.frame.2[,domain:=bldg.type]
# smpl.design.2 <- smpl.frame.2[,.(Population=length(RecordID)),by='domain']
# smpl.design.2$targ.conf <- 0.9
# smpl.design.2$opt.param <- 'Precision'
# smpl.design.2$value <- 0.1
# smpl.design.2[,Population:=NULL]
# head(cb.smpl)
# set.seed(444)
# bldg.smpl <- size.stratify(copy(smpl.frame.2),copy(smpl.design.2),num.strata,smpl.out = T)
# head(bldg.smpl)
# smpl.frame.1 <- merge(smpl.frame.1,bldg.smpl[,c('RecordID','domain','stratum','sampled'),with=F],by='RecordID',all.x=T)
# head(smpl.frame.1)
# 
# smry.1 <- smpl.frame.1[,.(sd.strat.1=sd(sim.sf),tot.sf.1=sum(sim.sf)),by=c('cb.stratum','N.strat.1')]
# smry.1 <- smry.1[,.(dom.sd.1=sum(N.strat.1*sd.strat.1),dom.sf.1=sum(tot.sf.1),dom.sd.fpc.1=sum(N.strat.1*sd.strat.1^2))]
# smry.2 <- smpl.frame.1[,.(N.strat.2=length(RecordID),sd.strat.2=sd(sim.sf),tot.sf.2=sum(sim.sf)),by=c('domain','stratum')]
# smry.2 <- smry.2[,.(dom.sd.2=sum(N.strat.2*sd.strat.2),dom.sf.2=sum(tot.sf.2),dom.sd.fpc.2=sum(N.strat.2*sd.strat.2^2))]
# 
# a.1 <- (1.645*smry.1$dom.sd.1/smry.1$dom.sf.1)^2
# a.2 <- (1.645*smry.2$dom.sd.2/smry.2$dom.sf.2)^2
# FPC <- (1.645/smry.2$dom.sf.2)^2*smry.2$dom.sd.fpc.2-(1.645/smry.1$dom.sf.1)^2*smry.1$dom.sd.fpc.1
# n.2 <- seq(from=0,to=2000,by=50)
# n.1 <- a.1/(a.2/n.2+0.1^2+FPC)
# plot(n.2,n.1)
#   
#   
# ## Step 4. Repeated 2 stage sample selection method.----------
# num.runs <- c(50,100,250,500,1000)
# system.time(stats.1 <- lapply(num.runs,function(s){
#   runs <- s
#   set.seed(444)
#   stats.0 <- lapply(1:runs,function(r) {
#     output.1 <- size.stratify(copy(cb.smpl.frame),copy(smpl.design.1),num.strata,ID.field,sz.meas.param,smpl.out = T)
#     # head(output.1)
#     strata.smry <- output.1[,.(N.strat=length(Census.ID)),by=c('domain','stratum')]
#     strata.smry <- strata.smry[output.1[sampled=='Y',.(strat.n=length(Census.ID),sd.strat.smpl=sd(sim.avg.sf),sum.sf=sum(sim.avg.sf)),by=c('domain','stratum')],on=c('domain','stratum')]
#     strata.smry[,wtd.sf:=N.strat*sum.sf/strat.n]
#     domain.smry <- output.1[,.(sum.meas.sz=sum(sim.avg.sf,na.rm=T)),by=c('domain','z')]
#     domain.smry <- domain.smry[strata.smry[,.(avg.sf=sum(wtd.sf)/sum(N.strat),V.strat=sum(N.strat*sd.strat.smpl)^2/sum(strat.n),V.strat.FPC=sum(N.strat*sd.strat.smpl^2)),by='domain'],on='domain']
#     domain.smry[,SE:=sqrt(V.strat-V.strat.FPC)]
#     domain.smry[,RP:=z/sum.meas.sz*SE]
#     domain.smry[,run:=r]
#     return(domain.smry[,c('avg.sf','SE','run'),with=F])
#   })
#   tmp.stats <- data.table(rbind.fill.matrix(lapply(stats.0, rbind)))
#   tmp.stats[,runs:=s]
#   return(tmp.stats)
# }))
# stats <- data.table(rbind.fill.matrix(lapply(stats.1, rbind)))
# stats[,rp:=z*SE/avg.sf/nrow(cb.smpl.frame)]
# head(stats)
# true.mean <- mean(smpl.frame$sim.sf)
# z<-qnorm(1-(1-0.9)/2)
# stats[,.(resamp.rp=1-true.mean/mean(avg.sf),avg.rp=mean(rp)),by='runs']
# stats[,.(z*sd(avg.sf)/mean(avg.sf)),by='runs']
# histogram(~avg.sf|paste0('runs=',runs),data=stats)
# histogram(~SE|paste0('runs=',runs),data=stats)
# 
# 
# # output.1 <- srs(frame.1=copy(cb.smpl.frame),design.1=copy(smpl.design.1))
# head(output.1)
# smpl.frame.2 <- copy(smpl.frame)
# # colnames(smpl.frame.2)
# smpl.frame.2 <- smpl.frame.2[Census.ID %in% output.1]
# smpl.design.1 <- smpl.design.1[smpl.frame.2[,.(cb.cost=sum(cb.survey.cost)),by='stabbr'],on='domain']
# 
# saveRDS(smpl.frame.2,file='frame_2.rds')
# old.cb.smpl.frame <- readRDS('frame_2.rds')
# 
# 
# smpl.frame.2 <- readRDS('frame_2.rds')


## old stuff------------
{# 
# # write.csv(smpl.frame,file='sim_frame.csv')
# # cb.smry <- smpl.frame[,.(rec.ct=length(RecordID)),by='Census.ID']
# # sum(cb.smry$rec.ct)/nrow(cb.smry)
# # P:\Projects\NEEA22 (CBSA Phase 1 Planning)\_04 Assessment of Alternative Designs\Tool\SD2\test_designframe.csv
# 
# # set inputs for sample design [THESE NEED TO BE GLOBALLY DEFINED?]--------
# # overhead cost
# # fname.cost <- './CBSA Data Collection Cost.xlsx'
# # ## building survey costs
# # #oh.cost <- data.table(read.xlsx(fname.cost,sheet='for R - OH costs',colNames = T))
# # oh.cost <- data.table(readWorksheetFromFile(fname.cost,sheet='for R - OH costs',header = T))
# # 
# # Cost.OH <- 6e5 #oh.cost$base.cost
# 
# # define max sampling fraction
# # max.n.frac <- 0.5
# 
# 
# ## all this stuff is to design a sample, in lieu of using the web app -------------
# 
# 
# ## test sample designs--------
# {
#   # get top utilities by largest total floor area in frame
#   # top.utils <- smpl.frame[,.(rec.cnt=length(RecordID),tot.sf=sum(sim.sf)),by='OverSampleUtility'][tot.sf>50e6]
#   # # setorder(top.utils,-tot.sf)
#   # smpl.frame[,Utility:=OverSampleUtility]
#   # # reassign remaining utilities to BPA
#   # smpl.frame[!(OverSampleUtility %in% top.utils$OverSampleUtility),Utility:='Bonneville Power Administration']
#   # smpl.frame[,colnames(smpl.frame)[19:29]:=NULL]
#   # domain.cols <- c('stabbr','Density','bldg.type')
#   # smpl.frame[,domain:=paste(get('stabbr'),get('Pub.Priv'),get('Density'),get('bldg.type'),sep='_')]
#   # # reassign domains, defined as pop < 20, to one domain
#   # small.domains <- smpl.frame[,.(rec.cnt=length(RecordID)),by='domain'][rec.cnt<50]
#   # smpl.frame[domain %in% small.domains$domain,domain:='Too Small to Stratify']
#   # # head(smpl.frame)
#   # # smpl.frame[,.(length(RecordID)),by='domain']
#   # domains <- unique(smpl.frame$domain)
#   
#   smpl.frame[,domain:=NULL]
#   # smpl.frame[bldg.type=='Office' & Pub.Priv=='Private',.(rec.ct=length(RecordID),tot.sf=sum(sim.sf))]
#   smpl.frame[,domain:=bldg.type]  
#   smpl.design <- smpl.frame[,.(Population=length(RecordID),Tot_SF=sum(sim.sf),Avg_SF=mean(sim.sf),CV_SF=sd.pop(sim.sf)/mean(sim.sf)),
#                             by='domain']
#   target.conf <- rep(0.9,nrow(smpl.design))
#   target.prec <- rep(0.1,nrow(smpl.design))
#   target.cost <- rep(1e5,nrow(smpl.design))
#   optim.param <- rep('Precision',nrow(smpl.design))
#   # optim.param <- rep('Cost',nrow(smpl.design))
#   # head(smpl.design)
#   smpl.design$targ.conf <- target.conf
#   smpl.design$opt.param <- optim.param
#   # smpl.design$value <- target.cost
#   smpl.design$value <- target.prec
#   
#   
#   # smpl.design[stabbr %in% c('ID','MT') & domain != 'Too Small to Stratify',opt.param:='cost']
#   # smpl.design[stabbr %in% c('ID','MT') & domain != 'Too Small to Stratify',value:=27e3]
#   # smpl.design[domain == 'Too Small to Stratify',opt.param:='cost']
#   # smpl.design[domain == 'Too Small to Stratify',value:=27e3]
#   smpl.design[,c('RP_SF','RP_kWh','RP_Thrm','RP_E_EUI','RP_G_EUI','RP_Yr_Built','Cost','SampleSize'):=0]
#   # smpl.design[Tot_SF<5e6 & opt.param=='precision',targ.conf:=0.8]
#   # smpl.design[Tot_SF<5e6 & opt.param=='precision',value:=0.2]
#   # smpl.design[,.(length(domain)),by=c('opt.param','targ.conf','value')]
#   
#   smpl.frame[,.(rec.ct=length(RecordID),tot.sf=sum(sim.sf),avg.sf=mean(sim.sf),cv.sf=sd.pop(sim.sf)/mean(sim.sf),
#                 tot.cost=sum(survey.cost),avg.cost=mean(survey.cost),cv.cost=sd.pop(survey.cost)/mean(survey.cost)),by='domain'][,
#                                                                                                                                  .(pct.ct=rec.ct/sum(rec.ct),pct.sf=tot.sf/sum(tot.sf),pct.avg.sf=avg.sf/(sum(tot.sf)/sum(rec.ct)),
#                                                                                                                                    pct.cost=tot.cost/sum(tot.cost),pct.avg.cost=avg.cost/(sum(tot.cost)/sum(rec.ct)))]
#   
#   ### get results of sample design-------
#   rm(frame,design)
#   frame <- copy(smpl.frame)
#   design <- copy(smpl.design)
#   num.strata <-3
#   ID.field <- 'RecordID'
#   sz.meas.param <- 'sim.sf'
#   tail(frame)
#   tail(design)
#   system.time(output <- size.stratify(frame=copy(smpl.frame),design=copy(smpl.design[,c('domain','targ.conf','opt.param','value'),with=F]),num.strata,ID.field,sz.meas.param))
#   output[,RP_sf:=round(signif(RP_sf,3),2)]
#   output[,RP_vintage:=round(signif(RP_vintage,3),3)]
#   output[,RP_Elect.EUI:=round(signif(RP_Elect.EUI,3),2)]
#   output[,RP_Gas.EUI:=round(signif(RP_Gas.EUI,3),2)]
#   output[,RP_kWh:=round(signif(RP_kWh,3),2)]
#   output[,RP_therm:=round(signif(RP_therm,3),2)]
#   output[,.(tot.smpl.sz=sum(SampleSize),tot.cost=sum(Cost))]
#   output[is.nan(RP_SF)]
#   output[,.(mean(RP_SF),mean(Cost),max(Cost),mean(Sample_Size),max(Sample_Size)),by=c('targ.conf','opt.param','value')]
#   summary(output)
# }## end of testing sample design
# 
# ## import design from web app -----------
# {
#   # web.frame <- data.table(read.csv('P:/Projects/NEEA22 (CBSA Phase 1 Planning)/_04 Assessment of Alternative Designs/Tool/SD2/test_designframe.csv'))
#   # colnames(web.frame)[grepl('BLD_SF',colnames(web.frame))] <- 'sim.sf'
#   # colnames(web.frame)[grepl('Vintage_SBWRevised',colnames(web.frame))] <- 'sim.vintage'
#   # colnames(web.frame)[grepl('kWh_SBWRevised',colnames(web.frame))] <- 'sim.kWh'
#   # colnames(web.frame)[grepl('Therm_SBWRevised',colnames(web.frame))] <- 'sim.therm'
#   # saveRDS(web.frame,file='test_designframe.rds')
#   
#   web.frame <- readRDS('test_designframe.rds')
#   web.design <- data.table(read.csv('test_tmp.csv'))
#   # web.design <- web.design[web.frame[,.(Population=length(RecordID)),by='domain'],on='domain']
#   # web.design$Population <- web.design$i.Population
#   # web.design[,i.Population:=NULL]
#   # 
#   # web.frame.1 <- copy(web.frame[,colnames(web.frame)[c(2:6,8:17,22)],with=F])
#   # web.design.1 <- copy(web.design[,colnames(web.design)[c(21,6,10:19)],with=F])
#   web.o <- size.stratify(frame=copy(web.frame[,2:22,with=F]),design=copy(web.design[,2:5,with=F]))
#   
#   frame <- copy(web.frame)
#   design <- copy(web.design)
# }
# 
# ## oversample design -----------
# smpl.design.os <- smpl.frame[,.(Population=length(RecordID),Tot_SF=sum(sim.sf),Avg_SF=mean(sim.sf),CV_SF=sd.pop(sim.sf)/mean(sim.sf)),
#                              by=c('OverSampleUtility','bldg.type','domain')]
# 
}
