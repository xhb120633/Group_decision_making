##Figure6 plotting
library(emmeans)
library(ggplot2)
library(gridExtra)
library(fdrtool)
pic_theme <- theme(axis.title = element_text(face="bold", size = 45),
                   axis.text = element_text(face="bold", size = 42),
                   title=element_text(face='bold',size=40),
                   legend.title = element_text(face="bold", size = 38),
                   legend.text = element_text(face="bold", size = 38),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black", size = 1),
                   axis.ticks = element_line(size = 1),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank(),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   text = element_text(family = "sans"),
                   legend.position = c(0.7, 0.90))
 
parameter_S$discuss_list<-as.factor(parameter_S$discuss_list)
parameter_S$leader_power_list<-as.factor(parameter_S$leader_power_list)
##load parameter_S first

#plot Figure6A 6B group differences
summary_responsibility1<-data.frame(aggregate(responsibility1~discuss_list+leader_power_list,FUN='mean',data=parameter_S),(aggregate(responsibility1~discuss_list+leader_power_list,FUN='sd',data=parameter_S))[3])
n<-c(155,140,128,32,112,28)
colnames(summary_responsibility1)[c(3,4)]=c('mean','se')
summary_responsibility1$se<-summary_responsibility1$se/sqrt(n)

Fig6A<-ggplot()+
  #geom_violin(data=parameter_S,aes(x=discuss_list,y=responsibility1,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.5)+
  geom_boxplot(data=parameter_S,aes(x=discuss_list,y=responsibility1,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.5,notch=TRUE)+
  #geom_point(data=parameter_S,aes(x=discuss_list,y=responsibility1,color=leader_power_list),alpha=0.8,size=4,position=position_jitterdodge(jitter.width=0.05,dodge.width=0.8))+guides(color=FALSE)+
  #geom_point(data=summary_responsibility1,aes(x=discuss_list,y=mean,group=leader_power_list),position=position_dodge(width=0.8),size=5,alpha=0.8)+
  #geom_errorbar(data=summary_responsibility1,aes(x=discuss_list,ymin=mean-se,ymax=mean+se,group=leader_power_list),position=position_dodge(width=0.8),size=1,alpha=1,width=0.06)+
  labs(x='Discussion',y='responsibility',title='responsibility  Comparison ')+
  scale_color_manual(values=c('#33a02c','#a6cee3','#1f78b4'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_fill_manual(values=c('#33a02c','#a6cee3','#1f78b4'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_x_discrete(breaks=c(0,1),labels=c('No Discussion','Discussion'))+
  scale_y_continuous(expand = c(0,0),limits=c(0,100), breaks=seq(0, 100, 20))+
  pic_theme+
  theme(legend.position = c(0.9, 0.1),
        legend.title=element_blank())



summary_identity1<-data.frame(aggregate(identity1~discuss_list+leader_power_list,FUN='mean',data=parameter_S),(aggregate(identity1~discuss_list+leader_power_list,FUN='sd',data=parameter_S))[3])
n<-c(155,140,128,32,112,28)
colnames(summary_identity1)[c(3,4)]=c('mean','se')
summary_identity1$se<-summary_identity1$se/sqrt(n)

Fig6B<-ggplot()+
  #geom_violin(data=parameter_S,aes(x=discuss_list,y=identity1,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.5)+
  geom_boxplot(data=parameter_S,aes(x=discuss_list,y=identity1,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.5,notch=TRUE)+
  #geom_point(data=parameter_S,aes(x=discuss_list,y=identity1,color=leader_power_list),alpha=0.8,size=4,position=position_jitterdodge(jitter.width=0.05,dodge.width=0.8))+guides(color=FALSE)+
  #geom_point(data=summary_identity1,aes(x=discuss_list,y=mean,group=leader_power_list),position=position_dodge(width=0.8),size=5,alpha=0.8)+
  #geom_errorbar(data=summary_identity1,aes(x=discuss_list,ymin=mean-se,ymax=mean+se,group=leader_power_list),position=position_dodge(width=0.8),size=1,alpha=1,width=0.06)+
  labs(x='Discussion',y='identity',title='identity  Comparison ')+
  scale_color_manual(values=c('#33a02c','#a6cee3','#1f78b4'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_fill_manual(values=c('#33a02c','#a6cee3','#1f78b4'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_x_discrete(breaks=c(0,1),labels=c('No Discussion','Discussion'))+
  scale_y_continuous(expand = c(0,0),limits=c(0,100), breaks=seq(0, 100, 20))+
  pic_theme+
  theme(legend.position = c(0.9, 0.1),
        legend.title=element_blank())

##relevant statistical analysis
aov_r1<-aov(responsibility1~discuss_list*leader_power_list,data=parameter_S)
emmeans(aov_r1,pairwise~leader_power_list)

aov_r2<-aov(identity2~discuss_list*leader_power_list,data=parameter_S)
emmeans(aov_r2,pairwise~leader_power_list)
emmeans(aov_r2,pairwise~discuss_list)

#correlations
tmp_data=parameter_S
q_p_overall_cor_array=array(rep(0,30), dim = c(6,5))
q_p_overall_cor_p_array=array(rep(0,30), dim = c(6,5))
for (q in 1:6){
  for (p in 1:5){
    tmp_cor=as.numeric(cor.test(tmp_data[,13+q],tmp_data[,p])$estimate)
    tmp_p=as.numeric(cor.test(tmp_data[,13+q],tmp_data[,p])$p.value)
    q_p_overall_cor_array[q,p]=tmp_cor
    q_p_overall_cor_p_array[q,p]=tmp_p
  }
}

q_p_overall_cor_array=q_p_overall_cor_array[c(1,4),]
q_p_overall_cor_p_array=q_p_overall_cor_p_array[c(1,4),]

tmp_r<-c()
tmp_p<-c()
tmp_q_label<-c()
tmp_p_label<-c()
for (q in 1:2){
  for (p in 1:5){
    tmp_r<-c(tmp_r,q_p_overall_cor_array[q,p])
    tmp_p<-c(tmp_p,q_p_overall_cor_p_array[q,p])
    tmp_q_label<-c(tmp_q_label,q)
    tmp_p_label<-c(tmp_p_label,p)
  }
}

q_p_corr<-data.frame(tmp_r,tmp_p,tmp_q_label,tmp_p_label)

q_p_corr$tmp_q_label<-as.factor(q_p_corr$tmp_q_label)
q_p_corr$tmp_p_label<-as.factor(q_p_corr$tmp_p_label)

##linear model
q_loc<-c(14,17)
tmp_t<-c()
tmp_sd<-c()
for (q in 1:2){
  for (p in 1:5){
   tmp_y<-parameter_S[,p]
   tmp_x<-parameter_S[,q_loc[q]]
   tmp_data<-data.frame(tmp_y,tmp_x)
   tmp_lm<-lm(scale(tmp_y)~scale(tmp_x),data=tmp_data)
   q_p_corr[which(q_p_corr$tmp_q_label==q&q_p_corr$tmp_p_label==p),1]<-as.numeric(summary(tmp_lm)$coefficients[2,1])
   q_p_corr[which(q_p_corr$tmp_q_label==q&q_p_corr$tmp_p_label==p),2]<-as.numeric(summary(tmp_lm)$coefficients[2,4])
   tmp_sd<-c(tmp_sd,as.numeric(summary(tmp_lm)$coefficients[2,2]))
   tmp_t<-c(tmp_t,as.numeric(summary(tmp_lm)$coefficients[2,3]))
  }
}
 q_p_corr<-data.frame(q_p_corr,tmp_sd,tmp_t)


Fig5C<-ggplot(data=q_p_corr,aes(x=tmp_p_label,y=tmp_r,fill=tmp_q_label))+
  geom_col(width=0.5,position = position_dodge(width = 0.52))+
  geom_errorbar(aes(x=tmp_p_label,ymin=tmp_r-tmp_sd,ymax=tmp_r+tmp_sd,),width=0.3,position = position_dodge(width = 0.52))+
  scale_fill_manual(values=c('#fc8d59','#91cf60'),labels=c('Responsibility','Identity'))+
  scale_x_discrete(breaks=c(1,2,3,4,5),labels=c('Sensitivity','Decay','Learning Rate','Explore Bonus','Social Weight'))+
  xlab('Parameter')+
  ylab('Correlation coefficient')+
  pic_theme+
  theme(legend.position = c(0.9, 0.1),
        legend.title=element_blank())

#可以根据fdr来看是否该标显著性
fdrtool(q_p_corr$tmp_p,statistic="pvalue")

cor.test(parameter_S$responsibility1,parameter_S$identity2)