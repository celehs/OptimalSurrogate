pte.estimate = function(sob=data$sob , yob=data$yob , aob=data$aob , var = T , conf.int = T , rep=500) {

n=length(yob)
nn=199
from = min(sob); to = max(sob); step=((to - from)/nn)
s=seq(from, to, by = step)

#### estimation
# gs
bw = 1.06*sd(sob)*n^(-1/5)/(n^0.06) 
kern = Kern.FUN(zz=s,zi=sob,bw)
m.s.hat=apply(yob*kern,2,sum)/apply(kern,2,sum)

kern2 = Kern.FUN(zz=sob,zi=sob,bw)
m.sob.hat=apply(yob*kern2,2,sum)/apply(kern2,2,sum)
c.hat=mean(yob*(1-aob))/mean(1-aob)-mean(m.sob.hat*(1-aob))/mean(1-aob)

f0s.hat=apply(kern*(1-aob),2,mean)/mean((1-aob))
fs.hat=apply(kern,2,mean)
integrand<-f0s.hat^2/fs.hat
temp=(integrand[1] + integrand[nn+1] + 2*sum(integrand[seq(2,nn,by=2)]) + 4 *sum(integrand[seq(3,nn-1, by=2)]) )*step/3
g.s.hat=m.s.hat+f0s.hat/fs.hat *c.hat/temp
gs.es=g.s.hat

# pte
causal=mean(yob*aob)/mean(aob)-mean(yob*(1-aob))/mean(1-aob)
tempind=c(sapply(1:n, function(kk){which.min(abs(sob[kk]-s))}))
causals=mean(g.s.hat[tempind]*aob)/mean(aob)-mean(g.s.hat[tempind]*(1-aob))/mean(1-aob)
pte1.es=causals/causal

tempind=c(sapply(1:n, function(kk){which.min(abs(sob[kk]-s))}))
mses=2*( mean((yob-g.s.hat[tempind])^2) )
c=mean(yob*(1-aob))/mean(1-aob)
mse=2*( mean((yob-c)^2) )
pte2.es=sqrt(1-mses/mse)

#### variance
if(var | conf.int){
re=rep
v=matrix(rexp(n*re),nrow=n);
g.s.re=apply(v,2,resam, yob,sob,aob,n,kern,kern2,nn,s,step)

temp=g.s.re[3,]; temp2=g.s.re[4,];
causal.se=sd(g.s.re[1,])
causals.se=sd(g.s.re[2,])
pte1.se=sd(temp[(temp<1)*(temp>0)>0])
pte2.se=sd(temp2[(temp2<1)*(temp2>0)>0])
gs.se=apply(g.s.re[-(1:4),],1,sd)

if(conf.int) {	
  conf.l.causal = causal - 1.96*causal.se
  conf.u.causal = causal + 1.96*causal.se
  conf.l.causals = causals - 1.96*causals.se
  conf.u.causals = causals + 1.96*causals.se
  conf.l.pte1 = pte1.es - 1.96*pte1.se
  conf.u.pte1 = pte1.es + 1.96*pte1.se
  conf.l.pte2 = pte2.es - 1.96*pte2.se
  conf.u.pte2 = pte2.es + 1.96*pte2.se
  conf.l.gs = gs.es - 1.96*gs.se
  conf.u.gs = gs.es + 1.96*gs.se
}

}

if(!var & !conf.int) {return(list("delta" = causal, "delta.gs" =causals, "pte1" = pte1.es, "pte2" = pte2.es))}
if(var & !conf.int) {return(list("delta" = causal, "delta.gs" =causals, "pte1" = pte1.es, "pte2" = pte2.es, 
                                 "delta.se" = causal.se, "delta.gs.se" = causals.se, "pte1.se" = pte1.se,"pte2.se" = pte2.se) )}
if(conf.int) {return(list("delta" = causal, "delta.gs" =causals, "pte1" = pte1.es, "pte2" = pte2.es, 
                          "delta.se" = causal.se, "delta.gs.se" = causals.se, "pte1.se" = pte1.se,"pte2.se" = pte2.se,
                          "conf.int.delta" = c(conf.l.causal, conf.u.causal),
                          "conf.int.delta.gs" = c(conf.l.causals, conf.u.causals),
                          "conf.int.pte1" = c(conf.l.pte1, conf.u.pte1),
                          "conf.int.pte2" = c(conf.l.pte2, conf.u.pte2)) )}	

}

############################################################################### funs
VTM<-function(vc, dm){
  matrix(vc, ncol=length(vc), nrow=dm, byrow=T)
}

Kern.FUN <- function(zz,zi,bw) 
{ 
  out = (VTM(zz,length(zi))- zi)/bw
  dnorm(out)/bw
} 

resam<- function(v,yob,sob,aob,n,kern,kern2,nn,s,step){
  # gs
  m.s.hat=apply(as.numeric(v)*yob*kern,2,sum)/apply(as.numeric(v)*kern,2,sum)
  
  m.sob.hat=apply(as.numeric(v)*yob*kern2,2,sum)/apply(as.numeric(v)*kern2,2,sum)
  c.hat=mean(as.numeric(v)*yob*(1-aob))/mean(as.numeric(v)*(1-aob))-
    mean(as.numeric(v)*m.sob.hat*(1-aob))/mean(as.numeric(v)*(1-aob))
  
  f0s.hat=apply(as.numeric(v)*kern*(1-aob),2,mean)/mean(as.numeric(v)*(1-aob))
  f1s.hat=apply(as.numeric(v)*kern*aob,2,mean)/mean(as.numeric(v)*aob)
  fs.hat=apply(as.numeric(v)*kern,2,mean)
  integrand<-f0s.hat^2/fs.hat
  temp=(integrand[1] + integrand[nn+1] + 2*sum(integrand[seq(2,nn,by=2)]) + 4 *sum(integrand[seq(3,nn-1, by=2)]) )*step/3
  g.s.hat=m.s.hat+f0s.hat/fs.hat *c.hat/temp
  
  # pte
  causal=mean(as.numeric(v)*yob*aob)/mean(as.numeric(v)*aob)-mean(as.numeric(v)*yob*(1-aob))/mean(as.numeric(v)*(1-aob))
  
  tempind=c(sapply(1:n, function(kk){which.min(abs(sob[kk]-s))}))
  causals=mean(as.numeric(v)*g.s.hat[tempind]*aob)/mean(as.numeric(v)*aob)-
    mean(as.numeric(v)*g.s.hat[tempind]*(1-aob))/mean(as.numeric(v)*(1-aob))
  
  pte1=causals/causal
  
  tempind=c(sapply(1:n, function(kk){which.min(abs(sob[kk]-s))}))
  mses=2*( mean(as.numeric(v)*(yob-g.s.hat[tempind])^2) )
  
  c=mean(as.numeric(v)*yob*(1-aob))/mean(as.numeric(v)*(1-aob))
  mse=2*( mean(as.numeric(v)*(yob-c)^2) )
  
  pte2=sqrt(1-mses/mse)
  
  out=c(causal,causals,pte1, pte2, g.s.hat)
}

