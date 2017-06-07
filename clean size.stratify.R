
## define function to take standard deviation of population. R's sd formula assumes sd of sample. correct with sqrt(N-1)/sqrt(N)
sd.pop <- function(x) sd(x)*sqrt(length(x)-1)/sqrt(length(x))

## define function to calc precision/cost and select sample -----------

size.stratify <- function(frame,design,num.strata=3,ID.field='RecordID',sz.meas.param='sim.sf',seed=444,smpl.out=F){
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
                                                                                             ((value*sum(sum.strat)/z)^2+sum(N.strat*sd.strat^2))),by=c('domain','opt.param')])$n.dom
  ## eqn 5.24 from Cochran
  domain.smry[opt.param=='Cost']$n.dom <- unique(strata.smry[opt.param=='Cost',.(n.dom=(value)*sum(N.strat*sd.strat/sqrt(mean.strat.cost))/
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
