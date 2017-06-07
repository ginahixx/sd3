# setwd("P:/Projects/NEEA22 (CBSA Phase 1 Planning)/_04 Assessment of Alternative Designs/Tool/FaithSD3")
## working in SD3 branch
#debugmode <- T

library(plyr)
library(data.table)
library(XLConnect)
# library(openxlsx)
## building survey costs
# oh.cost <- data.table(read.xlsx('./CBSA Data Collection Cost.xlsx',sheet='for R - OH costs',colNames = T))
oh.cost <- data.table(readWorksheetFromFile('CBSA Data Collection Cost.xlsx',sheet='for R - OH costs',header = T))
Cost.OH <- oh.cost$base.cost
Cost.OH.1st.stage <- oh.cost$cost.1st.stage
Cost.1.FE <- oh.cost$cost.frm.enhance.1.stage
Cost.2.FE <- oh.cost$cost.frm.enhance.2.stage

## define function to take standard deviation of population. R's sd formula assumes sd of sample. correct with sqrt(N-1)/sqrt(N)
sd.pop <- function(x) sd(x)*sqrt(length(x)-1)/sqrt(length(x))

## define function to calc precision/cost and select sample -----------
size.stratify <- function(frame,design,num.strata=3,max.n.frac=0.7,ID.field='RecordID',sz.meas.param='sim.sf',seed=444,smpl.out=F){
  # get domain names
  domains <- design$domain
  # limit frame to only those records assigned to domains
  frame <- frame[domain %in% domains]
  
  # get population of each domain
  design <- design[frame[,.(Population=length(get(ID.field))),by='domain'],on='domain']
  
  # set number of strata for each domain
  design[,num.smpld.strata:=num.strata]
  # design[Population<100,num.smpld.strata:=2]
  # design[Population<30,num.smpld.strata:=1]
  
  # join design to frame to assign targets to each record in frame to use in sample size calcs 
  frame <- frame[design,on='domain']
  if(debugmode){
    cat(file=stderr(), "*in size.stratify , domains are ", unlist(domains), ", frame cols are", unlist(colnames(frame)),", design cols are ", unlist(colnames(design)), ", data is ", unlist(design), ".\n")
  }
  
  # get z statistic for given confidence
  frame[,z:=qnorm(1-(1-targ.conf)/2)]
  
  ## set up supporting calc's for assigning strata to each record in the frame
  # sort by sq.ft in ascending order to calculate cumulative percentage for assigning to strata
  setorderv(frame,c('domain',sz.meas.param))
  frame[,cusum:=lapply(.SD,cumsum),.SDcols=sz.meas.param,by='domain']
  frame[,max.cusum:=lapply(.SD,max),.SDcols='cusum',by='domain']
  frame[,cusum.pct:=cusum/max.cusum]
  ## calculate stratum interval. first get sq.ft differences between consecutive rows (x[i+1]-x[i])
  #    suppress warning about recycling (diff assigns last difference to first difference since it would otherwise return a vector shorter than the one passed to it)
  options(warn=-1)
  frame[,dif:=lapply(.SD,diff),.SDcols=sz.meas.param,by='domain'] 
  options(warn=1)
  # set last record of each domain dif=0, instead of recycled first difference
  frame[cusum.pct==1,dif:=0]
  # next get square root of differences
  frame[,sqrt.diff:=sqrt(dif)]
  # next get cumulative sum of sq rt of differences
  frame[,cusum.sqft.diff:=lapply(.SD,cumsum),.SDcols='sqrt.diff',by='domain']
  # stratum interval
  frame[,max.csum.sqft.diff:=lapply(.SD,max),.SDcols='cusum.sqft.diff',by='domain']
  frame[,strat.int:=round(max.csum.sqft.diff/num.smpld.strata,digits=6)]
  
  # assign sampled strata
  frame[,stratum:=ceiling(cusum.sqft.diff/strat.int)]
  frame[is.na(stratum),stratum:=1]
  # ensure smallest stratum assignment is 1
  frame[stratum<1,stratum:=1]
  # ensure largest sampled stratum assignment is not largest than number of sampled strata desired
  frame[stratum>num.smpld.strata,stratum:=num.smpld.strata]
  frame$stratum <- as.integer(frame$stratum)
  
  ## calculate sample size for each stratum!----------
  # summarize to strata-level
  strata.smry <- frame[,.(N.strat=length(get(ID.field)),sd.strat=sd.pop(get(sz.meas.param)),mean.strat=mean(get(sz.meas.param)),mean.strat.cost=mean(survey.cost),sum.strat=sum(get(sz.meas.param))),
                       by=c('domain','opt.param','targ.conf','stratum','z','value')]
  ## summarize to domain-level to calculate domain sample size, n.dom, based on whether optimizing to precision or cost
  domain.smry <- strata.smry[,.(N.sd.strat.by.sqrt.cost=sum(N.strat*sd.strat/sqrt(mean.strat.cost))),by=c('domain','opt.param','targ.conf','z','value')] 
  domain.smry$n.dom <- as.numeric(0)
  ## eqn 5.25 from Cochran
  domain.smry[opt.param=='Precision']$n.dom <- unique(strata.smry[opt.param=='Precision',.(n.dom=sum(N.strat*sd.strat*sqrt(mean.strat.cost))*sum(N.strat*sd.strat/sqrt(mean.strat.cost))/
                                                                                             ((as.numeric(value)*sum(sum.strat)/z)^2+sum(N.strat*sd.strat^2))),by=c('domain','opt.param')])$n.dom
  ## eqn 5.24 from Cochran
  domain.smry[opt.param=='Cost']$n.dom <- unique(strata.smry[opt.param=='Cost',.(n.dom=as.numeric(value)*sum(N.strat*sd.strat/sqrt(mean.strat.cost))/
                                                                                   sum(N.strat*sd.strat*sqrt(mean.strat.cost))),by=c('domain','opt.param')])$n.dom
  # merge domain.smry back with strata.smry to assign n.dom to each domain
  strata.smry <- strata.smry[domain.smry,on=c('domain','opt.param','targ.conf','z','value')]
  # product of stratum pop and std dev
  strata.smry[,N.sd.strat:=N.strat*sd.strat]
  # sum of product across frame (or domain)
  strata.smry[,sum.N.sd.strat:=lapply(.SD,sum),.SDcols='N.sd.strat',by='domain']
  # use Neyman allocation to allocate n.dom among strata
  strata.smry[,strat.n:=ceiling(n.dom*N.strat*sd.strat/sqrt(mean.strat.cost)/N.sd.strat.by.sqrt.cost)]
  strata.smry[is.na(strat.n),strat.n:=N.strat]
  # maximum sample size for stratum
  strata.smry[,n.max:=floor(max.n.frac*N.strat)]
  # check sample size smaller than max sample size allowed
  strata.smry[strat.n>n.max,strat.n:=n.max]
  # minimum sample size of 2
  strata.smry[strat.n<2,strat.n:=2]
  strata.smry$strat.n <- as.integer(strata.smry$strat.n)
  
  # draw sample!---------
  ## if smpl.out is FALSE, then returning domain level results and only want one sample drawn so set.seed inside function
  if(!smpl.out) set.seed(seed)
  ## else if smpl.out is TRUE, then retruning sampled records and set.seed is called outside function for multiple unqiue sample draws
  smpld.ids <- unlist(lapply(domains,function(d){
    lapply(1:nrow(strata.smry[domain==d]),function(s) {
      as.character(sample(as.matrix(frame[domain==d & stratum==s,c(ID.field),with=F]),strata.smry[domain==d & stratum==s]$strat.n))
    })
  }),recursive = TRUE)
  frame[,c('cusum','max.cusum','cusum.pct','dif','sqrt.diff','cusum.sqft.diff','max.csum.sqft.diff','strat.int'):=NULL]
  frame$sampled <- 'N'
  
  frame[get(ID.field) %in% smpld.ids,sampled:='Y']
  
  tmp.frame <- copy(frame) #[strata.smry[,c('domain','stratum','strat.n'),with=F],on=c('domain','stratum')]
  domain.smry <- domain.smry[frame[sampled=='Y',.(Cost=sum(survey.cost)),by='domain'],on='domain']
  
  ## calc RP! (eqn 5.27 in Cochran)----------
  meas.sz <- colnames(tmp.frame)[grepl('^sim\\.*.',colnames(tmp.frame))]
  for(m in meas.sz){
    # strata.smry <- strata.smry[tmp.frame[sampled=='Y',.(sd.strat.smpl=sd(get(m),na.rm=T)),by=c('domain','stratum')],on=c('domain','stratum')]
    strata.smry <- strata.smry[tmp.frame[,.(sd.strat.smpl=sd(get(m),na.rm=T)),by=c('domain','stratum')],on=c('domain','stratum')]
    domain.smry <- domain.smry[tmp.frame[,.(sum.meas.sz=sum(get(m),na.rm=T)),by='domain'],on='domain']
    domain.smry <- domain.smry[strata.smry[,.(V.strat=sum(N.strat*sd.strat.smpl)^2/sum(strat.n),V.strat.FPC=sum(N.strat*sd.strat.smpl^2)),by='domain'],on='domain']
    domain.smry[,paste('RP',m,sep='_'):=z/sum.meas.sz*sqrt(V.strat-V.strat.FPC)]
    domain.smry[,c('sum.meas.sz','V.strat','V.strat.FPC'):=NULL]
    strata.smry[,sd.strat.smpl:=NULL]
  }
  colnames(domain.smry)[grepl('*sim\\.*.',colnames(domain.smry))] <- gsub('sim\\.','',colnames(domain.smry)[grepl('sim\\.*.',colnames(domain.smry))])
  domain.smry <- domain.smry[strata.smry[,.(SampleSize=sum(strat.n)),by='domain'],on='domain']
  tmp.design <- domain.smry[,c('domain','Cost','SampleSize',colnames(domain.smry)[grepl('^RP*.',colnames(domain.smry))]),with=F]

  ## if smpl.out is FALSE, then return domain level results
  if(!smpl.out) return(tmp.design)
  ## if smpl.out is TRUE, then return the frame
  return(tmp.frame[tmp.design,on='domain'])
} # end of function size.stratify

## the coefficients come frome doing thousands of runs on various census block sample sizes 
##  and fitting a model to the resulting 1st stage sample fraction and 2 stage sample size adjustment factor
get.adj.factor <- function(bldg.frame,design.1.stage,budget.1st.stage,coeff.a=-0.401276934141468,coeff.b=1.05222053889306,seed=444){
  if(debugmode){
    cat(file=stderr(), "*in get.adj.factor , domains are ", unlist(design.1.stage$domains), ", design cols are", unlist(colnames(design.1.stage)), ", data is ", unlist(design.1.stage), ".\n")
  }
  
  cb.frame <- bldg.frame[,.(bldg.count=length(RecordID),sim.sf=mean(sim.sf),cb.survey.cost=sum(cb.survey.cost)),
                    by=c('Census.ID','OverSampleUtility','Density','stabbr','cty_name')]
  cb.frame[,survey.cost:=cb.survey.cost]
  cb.frame[,domain:='All']
  cb.design <- cb.frame[,.(cb.pop=length(Census.ID)),by='domain']
  cb.design$targ.conf <- 0.9
  cb.design$opt.param <- 'Cost'
  cb.design$value <- budget.1st.stage
  cb.design[,cb.pop:=NULL]
  # size.stratify(frame=copy(cb.frame),design=copy(cb.design),ID.field='Census.ID',smpl.out=F)
  set.seed(seed)
  cb.smpl <- size.stratify(frame=copy(cb.frame),design=copy(cb.design),max.n.frac=1,ID.field='Census.ID',seed=seed,smpl.out=T)
  smpl.frame.2 <- bldg.frame[cb.smpl[,c('Census.ID','sampled'),with=F],on='Census.ID']
  smpl.frame.smry <- smpl.frame.2[,.(N=length(RecordID),mean.sf=mean(sim.sf),sd.sf=sd.pop(sim.sf)),by='domain']
  smpl.frame.smry <- smpl.frame.smry[design.1.stage,on='domain']
  smpl.frame.smry[,rp:=0.1]
  smpl.frame.smry[targ.conf==0.8,rp:=0.2]
  smpl.frame.smry[,n.o:=(qnorm(1-(1-targ.conf)/2)*sd.sf/(rp*mean.sf))^2]
  smpl.frame.smry[,n.srs:=round(n.o/(1+n.o/N),0)]
  ## need SRS by domain
  
  smpl.frac <- nrow(smpl.frame.2[sampled=='Y'])/nrow(smpl.frame.2)
  smpl.sz.adj <- coeff.a*log(smpl.frac)+coeff.b
  set.seed(seed)
  smpl.1.stage <- size.stratify(copy(bldg.frame),copy(design.1.stage),seed=seed,smpl.out = F)
  smpl.1.stage[,cost.2:=smpl.sz.adj*Cost]
  smpl.1.stage[,n.2:=round(smpl.sz.adj*SampleSize,0)]
  smpl.1.stage[,cb.n:=nrow(cb.smpl[sampled=='Y'])]
  smpl.1.stage[,cb.var.cst:=sum(cb.smpl[sampled=='Y']$cb.survey.cost)]
  smpl.1.stage <- smpl.1.stage[smpl.frame.smry[,c('domain','n.srs'),with=F],on='domain']
  setcolorder(smpl.1.stage,c(1:3,10:11,4:9,12:14))
  return(smpl.1.stage)
}

# # smpl.frame <- data.table(readRDS('sim_frame.rds'))
# smpl.frame[,domain:=bldg.type]
# # smpl.design <- smpl.frame[,.(pop=length(RecordID)),by='domain']
# # smpl.design$targ.conf <- 0.9
# # smpl.design$opt.param <- 'Precision'
# # smpl.design$value <- 0.1
# # 
# budget.1st.stage <- 1e5
# # 
# bldg.frame <- copy(smpl.frame)
# # bldg.frame[,domain:=bldg.type]
# design.1.stage <- copy(smpl.design)
# coeff.a <- -0.401276934141468
# coeff.b <- 1.05222053889306



## cut and paste these lines into web app R script (replacing the current call to size.stratify)
## this is what gets returned to the design table in the web app
#results <- get.adj.factor(copy(smpl.frame),copy(smpl.design),budget.1st.stage)
## this is a global variable to populate the summary table
# <<<<<<< HEAD
# totals <- results[,.(cb.n=head(cb.n,1),n.1.stage=sum(SampleSize),n.srs=head(n.srs,1),var.cost.1.stg=sum(Cost),base.cost.1.stg=Cost.OH,tot.cost.1.stage=sum(Cost)+Cost.OH,
#                      frm.enh.1.stg=Cost.1.FE,tot.w.fe.1.stg=sum(Cost)+Cost.OH+Cost.1.FE,n.2.stage=sum(n.2),var.cost.2.stg=sum(cost.2),
#                      base.cost.2.stg=Cost.OH+Cost.OH.1st.stage,tot.cost.2.stage=sum(cost.2)+Cost.OH+Cost.OH.1st.stage,
#                      frm.enh.2.stg=Cost.2.FE,tot.w.fe.2.stg=sum(cost.2)+Cost.OH+Cost.OH.1st.stage+Cost.1.FE)]
# =======
# # totals <- results[,.(n.1.stage=sum(SampleSize),var.cost.1.stg=sum(Cost),base.cost.1.stg=Cost.OH,tot.cost.1.stage=sum(Cost)+Cost.OH,
# #                      frm.enh.1.stg=Cost.1.FE,tot.w.fe.1.stg=sum(Cost)+Cost.OH+Cost.1.FE,n.2.stage=sum(n.2),var.cost.2.stg=sum(cost.2),
# #                      base.cost.2.stg=Cost.OH+Cost.OH.1st.stage,tot.cost.2.stage=sum(cost.2)+Cost.OH+Cost.OH.1st.stage,
# #                      frm.enh.2.stg=Cost.2.FE,tot.w.fe.2.stg=sum(cost.2)+Cost.OH+Cost.OH.1st.stage+Cost.1.FE)]
# >>>>>>> 765d57c4504f782db8555983d2a6c2ecef6049be
