#### libraries ####
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

library(cmdstanr)
library(tidyverse)
library(bayesplot)
library(loo)
library(posterior)
library(rstan)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library("patchwork")
library(shinystan)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = F)
setwd("C:/data/gesposito/psiEvita/control_exp")
set_cmdstan_path('C:/Users/espositogi/Documents/.cmdstan/cmdstan-2.32.2') 

rm(list = ls())
set.seed(1)
dir=getwd()

#### importing the data ####
data<- read.csv("dataForRefit.csv")%>%
  filter(n>0)

#### Preparing data for refit ####
id<-unique(data$s)
ii<-match(data$s,id)
dist<-(data$x/10)+0.2 
condition<-abs(data$c-4) # 1=AT, 2=A, 3=T
nT<-data$n
y<-data$y

I<-length(id)
N<-length(ii)
C<-length(unique(condition))

x<-matrix(,C,C)
x[1,]<-c(1,0,0)
x[2,]<-c(1,1,0)
x[3,]<-c(1,0,1)

data_list <- list(x=x,
                  C=C,
                  N=N,
                  I=I,
                  y=y,
                  ii=ii,
                  nT=nT,
                  dist=dist,
                  condition=condition)
#### Refit classic model ####
mod<-cmdstan_model("classical_model_exp2.stan",dir=paste(dir,sep=""))
fit_classical_exp2<-mod$sample(data=data_list,
                seed=123,
                chains=4,
                parallel_chains = 4,
                iter_warmup = 2500,
                iter_sampling= 2500,
                max_treedepth = 10,
                adapt_delta = .9,
                init=2,
                output_dir=paste(dir,sep=""))
saveRDS(fit_classical_exp2,"fit_exp2_classical_1502.rds")

#### Diagnose classic model####
 fit_classical_exp2<-readRDS("fit_exp2_classical_1502.rds")

stanfit <- rstan::read_stan_csv(fit_classical_exp2$output_files())
launch_shinystan(stanfit,rstudio = T)

fit_classical_exp2$diagnostic_summary()


draws<-fit_classical_exp2$draws(
  variables=c(
    "mu_a","sigma_a","coef_a",
    "mu_log10_b","sigma_log10_b", "coef_log10_b",
    "mu_logit_l","sigma_logit_l","logit_l"
  ))

np<-nuts_params(fit_classical_exp2)
mcmc_parcoord(draws,np=np,regex_pars = "mu")
mcmc_pairs(draws,np=np,max_treedepth=10,pars=vars("mu_a[1]","sigma_a[1]","coef_a[1,1]"))
mcmc_pairs(draws,np=np,max_treedepth=10,pars=vars("mu_a[2]","sigma_a[2]","coef_a[1,2]"))
mcmc_pairs(draws,np=np,max_treedepth=10,pars=vars("mu_a[3]","sigma_a[3]","coef_a[1,3]"))

mcmc_pairs(draws,np=np,max_treedepth=10,pars=vars("mu_log10_b[1]","sigma_log10_b[1]","coef_log10_b[1,1]"))
mcmc_pairs(draws,np=np,max_treedepth=10,pars=vars("mu_log10_b[2]","sigma_log10_b[2]","coef_log10_b[1,2]"))
mcmc_pairs(draws,np=np,max_treedepth=10,pars=vars("mu_log10_b[3]","sigma_log10_b[3]","coef_log10_b[1,3]"))

mcmc_pairs(draws,np=np,max_treedepth=10,pars=vars("mu_logit_l","sigma_logit_l","logit_l[1]"))

loo_classical_exp2<-fit_classical_exp2$loo(cores=5)
plot(loo_classical_exp2)
print(loo_classical_exp2)

rhat<-summary(draws)%>%select(rhat)
print(max(rhat))

#### Inference ######
draws<-fit_classical_exp2$draws(format = "draws_df")
p_t_at_a<-mean(draws$`mu_a[2]`>0)
p_t_at_a<-2*min(c(p_t_at_a,1-p_t_at_a))

p_t_at_t<-mean(draws$`mu_a[3]`>0)
p_t_at_t<-2*min(c(p_t_at_t,1-p_t_at_t))

d_t_a_t<-draws$`mu_a[2]`-draws$`mu_a[3]`
p_t_a_t<-mean(d_t_a_t>0)
p_t_a_t<-2*min(c(p_t_a_t,1-p_t_a_t))

p_s_at_a<-mean(draws$`mu_log10_b[2]`>0)
p_s_at_t<-mean(draws$`mu_log10_b[3]`>0)
p_s_at_t<-mean(draws$`mu_log10_b[3]`>0)
d_s_a_t<-draws$`mu_log10_b[2]`-draws$`mu_log10_b[3]`
p_s_a_t<-mean(d_s_a_t>0)
p_s_a_t<-2*min(c(p_s_a_t,1-p_s_a_t))

##### Figures #####
# threhshold
threshold<-t(rbind(draws$`mu_a[1]`,draws$`mu_a[2]`,draws$`mu_a[3]`))
mu_a<-as_tibble(threshold)%>%rename(AT=V1,auditory=V2,tactile=V3) 

AT<-mu_a$AT[sample(1:10^4,10^4,replace=T)]
To<-mu_a$AT[sample(1:10^4,10^4,replace=T)]+mu_a$tactile[sample(1:10^4,10^4,replace=T)]
Ao<-mu_a$AT[sample(1:10^4,10^4,replace=T)]+mu_a$auditory[sample(1:10^4,10^4,replace=T)]
threshold<-tibble(AT,To,Ao)%>%
  pivot_longer(cols = c(AT,To,Ao))%>%
  rename(Condition=name)

qT<-threshold%>%group_by(Condition)%>%summarize(LB=quantile(value,0.025),M=quantile(value,0.5),UB=quantile(value,0.975))

ggT <- ggplot(threshold, aes(x=value,color=Condition,fill=Condition)) +
  geom_density(alpha = 0.4,position = "identity",show.legend=FALSE) +
  scale_color_manual(values = c(rgb(1, .2, .0), rgb(.8, .0, .8), rgb(0, .0, 1))) +
  geom_errorbarh(aes(xmin = qT$LB[1], xmax = qT$UB[1], y = 1.25), size = 0.75, height=0, col = rgb(1, .2, .0)) +
  geom_point(aes(x = qT$M[1], y = 1.25), size = 1.5, col = rgb(1, .2, .0)) +
  geom_errorbarh(aes(xmin = qT$LB[2], xmax = qT$UB[2], y = 1), size = 0.75,height=0, col = rgb(.8, .0, .8)) +
  geom_point(aes(x = qT$M[2], y = 1), size = 1.5, col = rgb(.8, .0, .8)) +
  geom_errorbarh(aes(xmin = qT$LB[3], xmax = qT$UB[3], y = 1.50), size = 0.75, height=0,col = rgb(0, .0, 1)) +
  geom_point(aes(x = qT$M[3], y = 1.50), size = 1.5, col = rgb(0, .0, 1)) +
  theme_minimal() +
  theme_classic() +
  labs(x = "Threshold (cm)", y = "Density") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text = element_text(size = 10)) +
  xlim(c(min(threshold$value), max(threshold$value))) +
  scale_fill_manual(values = c(rgb(1, .2, .0), rgb(.8, .0, .8), rgb(0, .0, 1))) +
  guides(fill="none",size="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print(ggT)


# Slope
slope<-t(rbind(draws$`mu_log10_b[1]`,draws$`mu_log10_b[2]`,draws$`mu_log10_b[3]`))
mu_log10_b<-as_tibble(slope)%>%rename(AT=V1,auditory=V2,tactile=V3) 

AT<-10^(mu_log10_b$AT)
To<-10^(mu_log10_b$AT+mu_log10_b$tactile)
Ao<-10^(mu_log10_b$AT+mu_log10_b$auditory)

slopeNat<-tibble(To,Ao,AT)%>%
  pivot_longer(cols = c(To,Ao,AT))%>%
  rename(Condition=name)

qSlopeNat<-slopeNat%>%group_by(Condition)%>%summarize(LB=quantile(value,0.025),M=quantile(value,0.5),UB=quantile(value,0.975))


ggS<-ggplot(slopeNat, aes(x=value,color=Condition,fill=Condition)) +
  geom_density(alpha = 0.4,position = "identity",show.legend=FALSE) +
  geom_errorbarh(aes(xmin=qSlopeNat$LB[1],xmax=qSlopeNat$UB[1],y=1),size=0.75,height=0,col=rgb(1,.2,.0))+
  geom_point(aes(x=qSlopeNat$M[1],y=1),size=1.5,col=rgb(1,.2,.0))+
  geom_errorbarh(aes(xmin=qSlopeNat$LB[2],xmax=qSlopeNat$UB[2],y=.75),size=0.75,height=0,col=rgb(.8,.0,.8))+
  geom_point(aes(x=qSlopeNat$M[2],y=.75),size=1.5,col=rgb(.8,.0,.8))+
  geom_errorbarh(aes(xmin=qSlopeNat$LB[3],xmax=qSlopeNat$UB[3],y=.5),size=0.75,height=0,col=rgb(0,0,1))+
  geom_point(aes(x=qSlopeNat$M[3],y=.5),size=1.5,col=rgb(0,0,1))+
  theme_minimal()+
  theme_classic()+
  xlab("Slope (1/cm)")+
  ylab("Density")+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text = element_text(size=10))+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_blank())+
  xlim(c(min(slopeNat$value), max(slopeNat$value))) +
  scale_fill_manual(values=c(rgb(1,.2,.0),rgb(.8,.0,.8),rgb(0,.0,1)), 
                    labels=c('A', 'AT','T'), 
                    guide = guide_legend(override.aes = list(shape = 15, color = c(rgb(1, .2, .0), rgb(.8, .0, .8), rgb(0,0,1))))) +
  scale_color_manual(values=c(rgb(1, .2, .0), rgb(.8, .0, .8),  rgb(0,0,1)), 
                     guide = guide_legend(override.aes = list(shape = NA)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print(ggS)

ggEstimate<-ggarrange(ggT, ggS,nrow=1,ncol=2,common.legend=TRUE,legend='bottom')+
  plot_annotation(title = "Population mean estimate") & 
  theme(plot.title = element_text(hjust = 0.5,size=16))
print(ggEstimate)

### plot delta histograms TvsA, Tvs AT, AvsAT ###
# threshold
ATvsA<-mu_a$auditory[sample(1:10^4,10^4,replace=T)]
ATvsT<-mu_a$tactile[sample(1:10^4,10^4,replace=T)]
TvsA<-mu_a$auditory[sample(1:10^4,10^4,replace=T)]-mu_a$tactile[sample(1:10^4,10^4,replace=T)]
dThreshold<-tibble(ATvsA,ATvsT,TvsA)%>%
  pivot_longer(cols = c(ATvsA,ATvsT,TvsA))%>%
  rename(Delta=name)

qdT<-dThreshold%>%group_by(Delta)%>%summarize(LB=quantile(value,0.025),M=quantile(value,0.5),UB=quantile(value,0.975))

ggdT<-ggplot(dThreshold, aes(x=value,color=Delta,fill=Delta)) +
  geom_density(alpha = 0.4,position = "identity",show.legend=FALSE) +
  geom_vline(xintercept = 0,colour="#000000")+
  scale_color_manual(values = c(rgb(1, .2, .0), rgb(0, .4, 1),rgb(1,0.8,0.1))) +
  #geom_text(aes(x=2.5,y=0.875,label=paste("P(AT\u2260A)=",pThreshold[1],"\n P(AT\u2260T)=",pThreshold[2],"\n P(T\u2260A)=",pThreshold[3]),hjust="left"),size=3.5,color='black')+
  theme_minimal()+
  geom_errorbarh(aes(xmin=qdT$LB[1],xmax=qdT$UB[1],y=2.5),size=0.75,height=0,col=rgb(1,0.2,0))+
  geom_point(aes(x=qdT$M[1],y=2.5),size=1.5,col=rgb(1,0.2,0))+
  geom_errorbarh(aes(xmin=qdT$LB[2],xmax=qdT$UB[2],y=3.75),size=0.75,height=0,col=rgb(0,0.4,1))+
  geom_point(aes(x=qdT$M[2],y=3.75),size=1.5,col=rgb(0,0.4,1))+
  geom_errorbarh(aes(xmin=qdT$LB[3],xmax=qdT$UB[3],y=1.25),size=0.75,height=0,col=rgb(1,0.8,0.1))+
  geom_point(aes(x=qdT$M[3],y=1.25),size=1.5,col=rgb(1,0.8,0.1))+
  theme_minimal()+
  theme_classic()+
  labs(x="\u0394 Threshold (cm)",y="Density")+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text = element_text(size=10))+
  xlim(c(min(dThreshold$value), max(dThreshold$value))) +
  scale_fill_manual(values=c(rgb(1,0.2,0),rgb(0,0.4,1),rgb(1,0.8,0.1)))+
  guides(fill="none",size="none")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print(ggdT)

# slope
ATvsA<-mu_log10_b$auditory[sample(1:10^4,10^4,replace=T)]
ATvsT<-mu_log10_b$tactile[sample(1:10^4,10^4,replace=T)]
TvsA<-mu_log10_b$auditory[sample(1:10^4,10^4,replace=T)]-mu_log10_b$tactile[sample(1:10^4,10^4,replace=T)]
dSlope<-tibble(ATvsA,ATvsT,TvsA)%>%
  pivot_longer(cols = c(ATvsA,ATvsT,TvsA))%>%
  rename(Delta=name)

qdS<-dSlope%>%group_by(Delta)%>%summarize(LB=quantile(value,0.025),M=quantile(value,0.5),UB=quantile(value,0.975))


ggdS<-ggplot(dSlope, aes(x=value,color=Delta,fill=Delta)) +
  geom_density(alpha = 0.4,position = "identity",show.legend=FALSE) +
  geom_vline(xintercept = 0,colour="#000000")+
  geom_errorbarh(aes(xmin=qdS$LB[1],xmax=qdS$UB[1],y=3),size=0.75,height=0,col=rgb(1,.2,.0))+
  # geom_text(aes(x=0.08,y=9,label=paste("P(AT>A)=",pSlope[1],"\n P(AT>T)=",pSlope[2],"\n P(T\u2260A)=",pSlope[3],"\n P(Predicted AT\u2260AT)=",pSlope[4]),hjust="left"),size=3.5,color='black')+
  geom_point(aes(x=qdS$M[1],y=3),size=1.5,col=rgb(1,.2,.0))+
  geom_errorbarh(aes(xmin=qdS$LB[2],xmax=qdS$UB[2],y=2),size=0.75,height=0,col=rgb(0,0.4,1))+
  geom_point(aes(x=qdS$M[2],y=2),size=1.5,col=rgb(0,0.4,1))+
  geom_errorbarh(aes(xmin=qdS$LB[3],xmax=qdS$UB[3],y=2.5),size=0.75,height=0,col=rgb(1,0.8,0.1))+
  geom_point(aes(x=qdS$M[3],y=2.5),size=1.5,col=rgb(1,0.8,.1))+
  theme_minimal()+
  theme_classic()+
  xlab("\u0394 log10(slope) (1/cm)")+
  ylab("Density")+
  theme(axis.title.x=element_text(size=14))+
  theme(axis.title.y=element_text(size=14))+
  theme(axis.text = element_text(size=10))+
  theme(legend.text = element_text(size = 12))+
  theme(legend.title = element_blank())+
  xlim(c(min(dSlope$value), max(dSlope$value))) +
  scale_fill_manual(values=c(rgb(1,0.2,0),rgb(0,0.4,1),rgb(1,0.8,0.1)),
                    labels=c('A-AT','T-AT','A-T'), 
                    guide = guide_legend(override.aes = list(shape = 15, color = c(rgb(1, .2, .0), rgb(0, 0.4, 1),rgb(1,0.8,0.1))))) +
  scale_color_manual(values=c(rgb(1, .2, .0), rgb(0, 0.4, 1),rgb(1,0.8,0.1)), 
                     guide = guide_legend(override.aes = list(shape = NA)))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
print(ggdS)

ggDelta<-ggarrange(ggdT, ggdS,nrow=1,ncol=2,common.legend=TRUE,legend='bottom')+
  plot_annotation(title = "Estimate of differences between conditions") & 
  theme(plot.title = element_text(hjust = 0.5,size=16))
print(ggDelta)

ggAll<-ggarrange(ggEstimate,ggDelta,nrow=2,ncol=1)
print(ggAll)

ggsave(filename = "DensityControl1502.tiff",plot=ggAll,device = "tiff",dpi=300,units="cm",width=16,height =12)
#ggsave(filename = "DensityControl1502.png",plot=ggAll,device = "png",dpi=600,units="cm",width=30,height =20)




