# a,b,l,mu_a,mu_log10_b,mu_logit_l,sigmas, 
d<-list(mu_a=fit_classical_exp1$draws(variables="mu_a",format="draws_matrix"),
       mu_b=fit_classical_exp1$draws(variables="mu_log10_b",format="draws_matrix"),
        mu_logit_l=fit_classical_exp1$draws(variables="mu_logit_l",format="draws_matrix"),
        a=fit_classical_exp1$draws(variables="a",format="draws_matrix"),
        b=fit_classical_exp1$draws(variables="b",format="draws_matrix"),
        l=fit_classical_exp1$draws(variables="l",format="draws_matrix"),
        sigma_a=fit_classical_exp1$draws(variables="sigma_a",format="draws_matrix"),
        sigma_b=fit_classical_exp1$draws(variables="sigma_log10_b",format="draws_matrix"),
        sigma_logit_l=fit_classical_exp1$draws(variables="sigma_logit_l",format="draws_matrix")
               )

data$x<-(data$x/10)+0.2
#### Functions ####
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)
evalTheta<-function(x,params){
  a=params$a
  b=params$b
  l=params$l
  
  theta=l+(1-l-l)*erfc(-b*(x-a)/sqrt(2))/2
  return(theta)
}
preparePsiParams<-function(d,S,n){
  a<-d$a[,S]
  a[(n+1):(2*n)]<-d$a[,S+20]
  a[(2*n+1):(3*n)]<-d$a[,S+40]
  b<-d$b[,S]
  b[(n+1):(2*n)]<- d$b[,S+20]
  b[(2*n+1):(3*n)]<- d$b[,S+40] 
  l<-d$l[,S]
  l[(n+1):(2*n)]<-d$l[,S]
  l[(2*n+1):(3*n)]<-d$l[,S] 
  psiParams<-tibble(a,b,l) 
  psiParams$cond<-c(rep(1,n),rep(2,n),rep(3,n))
  return(psiParams)
}
assessFit<-function(Cold,d,z,minx,maxx){
  D<-data.frame()
  x<-seq(minx,maxx,.05)
  lx<-length(x)
  n<-dim(d$mu_a)[1]
  for(S in z){
    raw<-Cold%>%filter(s==S)
    psiParams<-preparePsiParams(d,S,n)
    bestFitParams<-psiParams%>%group_by(cond)%>%summarise(across(.fns=~ median(.x, na.rm = TRUE)))
    randFitParams<-psiParams%>%group_by(cond)%>%sample_frac(size=1/250)%>%ungroup()
    
    bestFitPsi<-data_frame(temperature=rep(x,nrow(randFitParams)),Bp=NA,c=NA)
    for(i in 1:length(unique(Cold$c))){
      idx<-seq(1+(i-1)*lx,lx+(i-1)*lx)
      bestFitPsi$Bp[idx]<-evalTheta(x,slice(bestFitParams,i))
    }
    bestFitPsi$c[seq(1,lx*3)]<-c(rep("AT",lx),rep("A",lx),rep("T",lx))
    
    
    randFitPsi<-data_frame(temperature=rep(x,nrow(randFitParams)),p=NA,c=NA,iter=NA)
    for(i in 1:nrow(randFitParams)){
      idx<-seq(1+(i-1)*lx,lx+(i-1)*lx)
      randFitPsi$p[idx]<-evalTheta(x,slice(randFitParams,i))
      randFitPsi$iter[idx]<-i
    }
    randFitPsi$c<-c(rep("AT",lx*nrow(randFitParams)/3),rep("A",lx*nrow(randFitParams)/3),rep("T",lx*nrow(randFitParams)/3))
    fits<-full_join(randFitPsi,bestFitPsi)
    
    raw<-raw%>%mutate(pDetect=y/n)%>%rename(temperature=x)
    raw$c[raw$c==1]<-"AT"
    raw$c[raw$c==2]<-"A"
    raw$c[raw$c==3]<-"T"
    data<-full_join(fits,raw)
    data<-data%>%filter(is.na(c)!=1)
    data$s<-S
    D<-bind_rows(D,data)
  }
  
  g<-ggplot(D)+
    geom_line(aes(x=temperature,y=p,group=iter),colour="#999999")+
    geom_line(aes(x=temperature,y=Bp),size=1)+
    geom_point(aes(x=temperature,y=pDetect,colour=n),size=2)+
    geom_vline(aes(xintercept=0))+
    scale_color_gradient(low="blue", high="red",trans="log10",limits=c(1,60))+
    xlab("Relative distance (cm)")+
    ylab("Detection Probability")+
    facet_grid(c~s)+
    theme_minimal()+
    xlim(minx,maxx)
  return(g)
}
z=1:10
f110<-assessFit(data,d,z,-3.5,3.5)
#print(f110)
ggsave(filename = "IndividualFits1_2102.tiff",plot=f110,device = "tiff",dpi=300,units="cm",width=15,height =10)
#ggsave(filename = "IndividualFits1_2102.png",plot=f110,device = "png",dpi=600,units="cm",width=19,height =10)
z=11:20
f1120<-assessFit(data,d,z,-3.5,3.5)
#print(f1120)
ggsave(filename = "IndividualFits2_2102.tiff",plot=f1120,device = "tiff",dpi=300,units="cm",width=15,height =10)
#ggsave(filename = "IndividualFits2_1711.png",plot=f1120,device = "png",dpi=600,units="cm",width=29,height =20)
#ggIndividual_exp_2<-ggarrange(f110, f1120,nrow=2,ncol=1, align = "hv",common.legend=TRUE, legend = "right")
#print(ggIndividual_exp_2)
#ggsave(filename = "IndividualFits_exp2.tiff",plot=ggIndividual_exp_2,device = "tiff",dpi=300,units="cm",width=14,height =15)
#####
evalPhi<-function(x){
  p=erfc(x/sqrt(2))/2
  return(p)
}
#####
visualizeFullModelPosterior<-function(d,temperature,M){
  ca<-cb<-cl<-matrix(,10^5,3)
  mca<-mcb<-mcl<-a<-b<-l<-matrix(,.95*10^5,3)
  ql<-qa<-qb<-matrix(,3,3)
  for(i in 1:10^5){
    k<-round(runif(1,2,1+dim(d$mu_a)[1]))-1
    for (j in 1:3){
      #cl[i,j]<-rbeta(1,1,d$b_l[k])
      cl[i,j]<-1/(1+exp(-rnorm(1,d$mu_logit_l[k],d$sigma_logit_l[k])))/2
      ca[i,j]<-rnorm(1,d$mu_a[k,j],d$sigma_a[k,j])
      cb[i,j]<-rnorm(1,d$mu_b[k,j],d$sigma_b[k,j])
    }
  }
  for (j in 1:3){
    ql[,j]<-quantile(cl[,j],c(.025,.5,.975))
    qa[,j]<-quantile(ca[,j],c(.025,.5,.975))
    qb[,j]<-quantile(cb[,j],c(.025,.5,.975))
    
    idx<-cl[,j]>ql[3,j]|cl[,j]<ql[1,j]
    mcl[,j]<-cl[!idx,j]
    idx<-ca[,j]>qa[3,j]|ca[,j]<qa[1,j]
    mca[,j]<-ca[!idx,j]
    idx<-cb[,j]>qb[3,j]|cb[,j]<qb[1,j]
    mcb[,j]<-cb[!idx,j]
  }
  a[,1]<-mca[,1]
  a[,2]<-mca[,1]+mca[,2]
  a[,3]<-mca[,1]+mca[,3]
  b[,1]<-10^(mcb[,1])
  b[,2]<-10^(mcb[,1]+mcb[,2])
  b[,3]<-10^(mcb[,1]+mcb[,3])
  
  bt1<-ql[2,1]+(1-2*ql[2,1])*evalPhi(-10^(qb[2,1])*(temperature-(qa[2,1])))
  bt2<-ql[2,2]+(1-2*ql[2,2])*evalPhi(-10^(qb[2,1]+qb[2,2])*(temperature-(qa[2,1]+qa[2,2])))
  bt3<-ql[2,3]+(1-2*ql[2,3])*evalPhi(-10^(qb[2,1]+qb[2,3])*(temperature-(qa[2,1]+qa[2,3])))
  t1<-t2<-t3<-matrix(seq(1,M),M,length(temperature)+1)
  for (i in 1:M){
    t1[i,1:length(temperature)]<-mcl[i,1]+(1-mcl[i,1])*evalPhi(-b[i,1]*(temperature-a[i,1]))
    t2[i,1:length(temperature)]<-mcl[i,2]+(1-mcl[i,2])*evalPhi(-b[i,2]*(temperature-a[i,2]))
    t3[i,1:length(temperature)]<-mcl[i,3]+(1-mcl[i,3])*evalPhi(-b[i,3]*(temperature-a[i,3]))
  }
  colnames(t1)<-colnames(t2)<-colnames(t3)<-c(as.character(temperature),"DrawIdx")
  t1<-as.data.frame(t1)%>%
    pivot_longer(cols=as.character(temperature),names_to = "temperature",values_to = "probability")
  t2<-as.data.frame(t2)%>%
    pivot_longer(cols=as.character(temperature),names_to = "temperature",values_to = "probability")
  t3<-as.data.frame(t3)%>%
    pivot_longer(cols=as.character(temperature),names_to = "temperature",values_to = "probability")
  t<-full_join(t1,t2,by=c("DrawIdx","temperature"))%>%
    full_join(t3,by=c("DrawIdx","temperature"))%>%
    rename(T=probability.x,A=probability.y,AT=probability)%>%
    pivot_longer(cols=c(AT,T,A),names_to = "condition",values_to = "probability")%>%
    group_by(temperature,condition)%>%
    summarise(lb=quantile(probability,0.025),ub=quantile(probability,0.975))
  
  
  bt<-tibble(probability=c(bt1,bt2,bt3),
             temp=as.character(rep(temperature,3)),
             condition=c(rep("AT",length(temperature)),rep("A",length(temperature)),rep("T",length(temperature))))%>%
    rename(temperature=temp)
  data<-full_join(bt,t,by=c("temperature","condition"))
  
  
  ggplot(data)+
    geom_vline(xintercept = 0,colour="black")+
    geom_line(aes(color=condition,x=as.double(temperature),y=lb),size=1,linetype=5,alpha=.5)+
    geom_line(aes(color=condition,x=as.double(temperature),y=ub),size=1,linetype=5,alpha=.5)+
    geom_line(aes(color=condition,x=as.double(temperature),y=probability),size=1,alpha=1)+
    theme_minimal()+
    theme(axis.title.x=element_text(size=18))+
    theme(axis.title.y=element_text(size=18))+
    theme(axis.text = element_text(size=14))+
    theme(legend.text = element_text(size = 18))+ 
    theme(legend.title = element_text(size = 20))+
    guides(alpha="none")+
    labs(x="Relative distance from center (cm)",y="Probability of detection")+
    scale_color_manual(values=c(rgb(1,.2,.0),rgb(.8,.2,.80),rgb(0,.0,1)))+
    scale_alpha_manual(values=c(.01,1))
    }
#### Visualize full posterior ####
temperature<-seq(-2.5,2.5,0.02)
visualizeFullModelPosterior(d,temperature,10^3)
ggsave(filename = "PsiATiPost_new2202.tiff",device = "tiff",dpi=300,units="cm",width=20,height = 10)
#ggsave(filename = "PsiATiPost_new2510.png",device = "png",dpi=300,units="cm",width=20,height = 10)

