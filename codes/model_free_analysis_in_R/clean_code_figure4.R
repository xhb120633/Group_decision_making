library(R.matlab)
library(ggplot2)
library(emmeans)

## the theme for plotting 
pic_theme <- theme(axis.title = element_text(face="bold", size = 45),
                   axis.text = element_text(face="bold", size = 42),
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
                   legend.position = c(0.92, 0.90))

###if you load the depdencies, the variavle parameter_S should be there already.
#plot
summary_sensitivity<-data.frame(aggregate(Sensitivity~discuss_list+leader_power_list,FUN='mean',data=parameter_S),(aggregate(Sensitivity~discuss_list+leader_power_list,FUN='sd',data=parameter_S))[3])
n<-c(155,140,128,32,112,28)
colnames(summary_sensitivity)[c(3,4)]=c('mean','se')
summary_sensitivity$se<-summary_sensitivity$se/sqrt(n)

Figurep4a<-ggplot()+
  #geom_violin(data=parameter_S,aes(x=discuss_list,y=Sensitivity,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.5)+
  geom_boxplot(data=parameter_S,aes(x=discuss_list,y=Sensitivity,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.75,notch=TRUE)+
  #geom_point(data=parameter_S,aes(x=discuss_list,y=Sensitivity,color=leader_power_list),alpha=0.8,size=4,position=position_jitterdodge(jitter.width=0.05,dodge.width=0.8))+guides(color=FALSE)+
  #geom_point(data=summary_sensitivity,aes(x=discuss_list,y=mean,group=leader_power_list),position=position_dodge(width=0.8),size=5,alpha=0.8)+
  #geom_errorbar(data=summary_sensitivity,aes(x=discuss_list,ymin=mean-se,ymax=mean+se,group=leader_power_list),position=position_dodge(width=0.8),size=1,alpha=1,width=0.06)+
  labs(x='Discussion',y='Sensitivity Parameter',title='Sensitivity Parameter Comparison ')+
  scale_color_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_fill_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_x_discrete(breaks=c(0,1),labels=c('No Discussion','Discussion'))+
  scale_y_continuous(expand = c(0,0),limits=c(0,1), breaks=seq(0, 1, 0.2))+
  pic_theme+
  theme(legend.position = c(0.9, 0.97),
        legend.title=element_blank())

##p2
summary_Decay<-data.frame(aggregate(Decay~discuss_list+leader_power_list,FUN='mean',data=parameter_S),(aggregate(Decay~discuss_list+leader_power_list,FUN='sd',data=parameter_S))[3])
n<-c(155,140,128,32,112,28)
colnames(summary_Decay)[c(3,4)]=c('mean','se')
summary_Decay$se<-summary_Decay$se/sqrt(n)

Figurep4b<-ggplot()+
  #geom_violin(data=parameter_S,aes(x=discuss_list,y=Decay,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.5)+
  geom_boxplot(data=parameter_S,aes(x=discuss_list,y=Decay,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.75,notch=TRUE)+
  #geom_point(data=parameter_S,aes(x=discuss_list,y=Decay,color=leader_power_list),alpha=0.8,size=4,position=position_jitterdodge(jitter.width=0.05,dodge.width=0.8))+guides(color=FALSE)+
  #geom_point(data=summary_Decay,aes(x=discuss_list,y=mean,group=leader_power_list),position=position_dodge(width=0.8),size=5,alpha=0.8)+
  #geom_errorbar(data=summary_Decay,aes(x=discuss_list,ymin=mean-se,ymax=mean+se,group=leader_power_list),position=position_dodge(width=0.8),size=1,alpha=1,width=0.06)+
  labs(x='Discussion',y='Decay Parameter',title='Decay Parameter Comparison ')+
  scale_color_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_fill_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_x_discrete(breaks=c(0,1),labels=c('No Discussion','Discussion'))+
  scale_y_continuous(expand = c(0,0),limits=c(0,1), breaks=seq(0, 1, 0.2))+
  pic_theme+
  theme(legend.position = c(0.9, 0.97),
        legend.title=element_blank())

##p3
summary_LearningRate<-data.frame(aggregate(LearningRate~discuss_list+leader_power_list,FUN='mean',data=parameter_S),(aggregate(LearningRate~discuss_list+leader_power_list,FUN='sd',data=parameter_S))[3])
n<-c(155,140,128,32,112,28)
colnames(summary_LearningRate)[c(3,4)]=c('mean','se')
summary_LearningRate$se<-summary_LearningRate$se/sqrt(n)

Figurep4c<-ggplot()+
  #geom_violin(data=parameter_S,aes(x=discuss_list,y=LearningRate,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.5)+
  geom_boxplot(data=parameter_S,aes(x=discuss_list,y=LearningRate,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.75,notch=TRUE)+
  #geom_point(data=parameter_S,aes(x=discuss_list,y=LearningRate,color=leader_power_list),alpha=0.8,size=4,position=position_jitterdodge(jitter.width=0.05,dodge.width=0.8))+guides(color=FALSE)+
  #geom_point(data=summary_LearningRate,aes(x=discuss_list,y=mean,group=leader_power_list),position=position_dodge(width=0.8),size=5,alpha=0.8)+
  #geom_errorbar(data=summary_LearningRate,aes(x=discuss_list,ymin=mean-se,ymax=mean+se,group=leader_power_list),position=position_dodge(width=0.8),size=1,alpha=1,width=0.06)+
  labs(x='Discussion',y='LearningRate Parameter',title='LearningRate Parameter Comparison ')+
  scale_color_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_fill_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_x_discrete(breaks=c(0,1),labels=c('No Discussion','Discussion'))+
  scale_y_continuous(expand = c(0,0),limits=c(0,1), breaks=seq(0, 1, 0.2))+
  pic_theme+
  theme(legend.position = c(0.9, 0.97),
        legend.title=element_blank())



summary_ExplorBonus<-data.frame(aggregate(ExplorBonus~discuss_list+leader_power_list,FUN='mean',data=parameter_S),(aggregate(ExplorBonus~discuss_list+leader_power_list,FUN='sd',data=parameter_S))[3])
n<-c(155,140,128,32,112,28)
colnames(summary_ExplorBonus)[c(3,4)]=c('mean','se')
summary_ExplorBonus$se<-summary_ExplorBonus$se/sqrt(n)

Figurep4d<-ggplot()+
  #geom_violin(data=parameter_S,aes(x=discuss_list,y=ExplorBonus,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.5)+
  geom_boxplot(data=parameter_S,aes(x=discuss_list,y=ExplorBonus,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.75,notch=TRUE)+
  #geom_point(data=parameter_S,aes(x=discuss_list,y=ExplorBonus,color=leader_power_list),alpha=0.8,size=4,position=position_jitterdodge(jitter.width=0.05,dodge.width=0.8))+guides(color=FALSE)+
  #geom_point(data=summary_ExplorBonus,aes(x=discuss_list,y=mean,group=leader_power_list),position=position_dodge(width=0.8),size=5,alpha=0.8)+
  #geom_errorbar(data=summary_ExplorBonus,aes(x=discuss_list,ymin=mean-se,ymax=mean+se,group=leader_power_list),position=position_dodge(width=0.8),size=1,alpha=1,width=0.06)+
  labs(x='Discussion',y='ExplorBonus Parameter',title='ExplorBonus Parameter Comparison ')+
  scale_color_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_fill_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_x_discrete(breaks=c(0,1),labels=c('No Discussion','Discussion'))+
  scale_y_continuous(expand = c(0,0),limits=c(-2,10), breaks=seq(-2, 10, 2))+
  pic_theme+
  theme(legend.position = c(0.9, 0.97),
        legend.title=element_blank())

##p5
summary_BetaOthers<-data.frame(aggregate(BetaOthers~discuss_list+leader_power_list,FUN='mean',data=parameter_S),(aggregate(BetaOthers~discuss_list+leader_power_list,FUN='sd',data=parameter_S))[3])
n<-c(155,140,128,32,112,28)
colnames(summary_BetaOthers)[c(3,4)]=c('mean','se')
summary_BetaOthers$se<-summary_BetaOthers$se/sqrt(n)

Figure4e<-ggplot()+
  #geom_violin(data=parameter_S,aes(x=discuss_list,y=BetaOthers,fill=leader_power_list),position=position_dodge(width=0.9),alpha=0.5)+
  geom_boxplot(data=parameter_S,aes(x=discuss_list,y=BetaOthers,fill=leader_power_list),position=position_dodge(width=0.9),alpha=0.75,notch=TRUE)+
  #geom_point(data=parameter_S,aes(x=discuss_list,y=BetaOthers,color=leader_power_list),alpha=0.8,size=4,position=position_jitterdodge(jitter.width=0.05,dodge.width=0.9))+guides(color=FALSE)+
  #geom_point(data=summary_BetaOthers,aes(x=discuss_list,y=mean,group=leader_power_list),position=position_dodge(width=0.9),size=5,alpha=0.8)+
  #geom_errorbar(data=summary_BetaOthers,aes(x=discuss_list,ymin=mean-se,ymax=mean+se,group=leader_power_list),position=position_dodge(width=0.9),size=1,alpha=1,width=0.06)+
  labs(x='Discussion',y='BetaOthers Parameter',title='BetaOthers Parameter Comparison ')+
  scale_color_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_fill_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_x_discrete(breaks=c(0,1),labels=c('No Discussion','Discussion'))+
  scale_y_continuous(expand = c(0,0),limits=c(0,1), breaks=seq(0, 1, 0.2))+
  pic_theme+
  theme(legend.position = c(0.9, 0.3),
        legend.title=element_blank())


lpp1<-lmer(Sensitivity~leader_power_list*discuss_list+(1|group_id),data=parameter_S)
summary(lpp1)
anova(lpp1)
emmeans(lpp1,pairwise~leader_power_list)
emmeans(lpp1,pairwise~discuss_list)

aov_p1<-aov(Sensitivity~leader_power_list*discuss_list,data=parameter_S)
summary(aov_p1)
emmeans(aov_p1,pairwise~leader_power_list)
emmeans(aov_p1,pairwise~discuss_list)


lpp2<-lmer(Decay~leader_power_list*discuss_list+(1|group_id),data=parameter_S)
summary(lpp2)
anova(lpp2)
emmeans(lpp2,pairwise~leader_power_list)
emmeans(lpp2,pairwise~discuss_list)

aov_p2<-aov(Decay~leader_power_list*discuss_list,data=parameter_S)
summary(aov_p2)
emmeans(aov_p2,pairwise~leader_power_list)
emmeans(aov_p2,pairwise~discuss_list)



contrasts(parameter_S$leader_power_list)<-cbind(c(1,0,-1),c(0,1,-1))

lpp3<-lmer(LearningRate~leader_power_list*discuss_list+(1|group_id),data=parameter_S)
summary(lpp3)
anova(lpp3)
emmeans(lpp3,pairwise~leader_power_list)
emmeans(lpp3,pairwise~discuss_list)

aov_p3<-aov(LearningRate~leader_power_list*discuss_list,data=parameter_S)
summary(aov_p3)
emmeans(aov_p3,pairwise~leader_power_list)
emmeans(aov_p3,pairwise~discuss_list)

## С?ı?????##
##############SOS########
lpp4<-lmer(ExplorBonus~leader_power_list*discuss_list+(1|group_id),data=parameter_S)
summary(lpp4)
anova(lpp4)
emmeans(lpp4,pairwise~leader_power_list)
emmeans(lpp4,pairwise~discuss_list)

aov_p4<-aov(ExplorBonus~leader_power_list*discuss_list,data=parameter_S)
summary(aov_p4)
emmeans(aov_p4,pairwise~leader_power_list)
emmeans(aov_p4,pairwise~discuss_list)




lpp5<-lmer(BetaOthers~leader_power_list*discuss_list+(1|group_id),data=parameter_S)
summary(lpp5)
anova(lpp5)
emmeans(lpp5,pairwise~leader_power_list)
emmeans(lpp5,pairwise~discuss_list)


aov_p5<-aov(BetaOthers~leader_power_list*discuss_list,data=parameter_S)
summary(aov_p5)
emmeans(aov_p5,pairwise~leader_power_list)
emmeans(aov_p5,pairwise~discuss_list)