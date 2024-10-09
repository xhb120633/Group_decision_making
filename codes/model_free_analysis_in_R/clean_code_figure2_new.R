##model free analysis for effect of social influence and individual-group inconsistence
library(lmerTest)
library(MASS)
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(cocor)
library(R.matlab)
library(lmerTest)
library(emmeans)
library(psych)
library(plyr)
library(overlapping)
library(lattice)
library(HDInterval)
library(effectsize)
library(psych)
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

#function preparation
cor.diff.test = function(x1, x2, y1, y2, method="pearson") {
  cor1 = cor.test(x1, x2, method=method)
  cor2 = cor.test(y1, y2, method=method)
  
  r1 = cor1$estimate
  r2 = cor2$estimate
  n1 = sum(complete.cases(x1, x2))
  n2 = sum(complete.cases(y1, y2))
  fisher = ((0.5*log((1+r1)/(1-r1)))-(0.5*log((1+r2)/(1-r2))))/((1/(n1-3))+(1/(n2-3)))^0.5
  
  p.value = (2*(1-pnorm(abs(fisher))))
  
  result= list(
    "cor1" = list(
      "estimate" = as.numeric(cor1$estimate),
      "p.value" = cor1$p.value,
      "n" = n1
    ),
    "cor2" = list(
      "estimate" = as.numeric(cor2$estimate),
      "p.value" = cor2$p.value,
      "n" = n2
    ),
    "p.value.twosided" = as.numeric(p.value),
    "p.value.onesided" = as.numeric(p.value) / 2
  )
  cat(paste(sep="",
            "cor1: r=", format(result$cor1$estimate, digits=3), ", p=", format(result$cor1$p.value, digits=3), ", n=", result$cor1$n, "\n",
            "cor2: r=", format(result$cor2$estimate, digits=3), ", p=", format(result$cor2$p.value, digits=3), ", n=", result$cor2$n, "\n",
            "diffence: p(one-sided)=", format(result$p.value.onesided, digits=3), ", p(two-sided)=", format(result$p.value.twosided, digits=3), "\n"
  ))
  return(result);
}

z_score<-function(x,mean,sd){
  z=(x-mean)/sd
  return(z)
}

group_class<-function(sub_id){
  if (sub_id<3000){
    leader=0
    if(sub_id<2000){
      discuss=0
    }else{
      discuss=1
    }
  }else{
    leader=1
    if(sub_id<4000){
      discuss=0
    }else{
      discuss=1
    }
  }
  return(c(discuss,leader))
}

group_individual_class<-function(sub_id,i){
  if (sub_id<3000){
    leader=0
    if(sub_id<2000){
      discuss=0
    }else{
      discuss=1
    }
  }else{
    if(i!=5){
      leader=1
    }else{
      leader=2
    }
    if(sub_id<4000){
      discuss=0
    }else{
      discuss=1
    }
  }
  return(c(discuss,leader))
}

cal_individual_performance<-function(s_decision,g_decision){
  n<-c(0,0,0,0)
  individual_performance<-c()
  s_decision=s_decision-10
  g_decision=g_decision-10
  for (t in 1:length(s_decision)){
    tmp_choice=s_decision[t]
    tmp_g_decision=g_decision[t]
    tmp_performance<-IGT_gain[tmp_choice,n[tmp_choice]+1]+IGT_loss[tmp_choice,n[tmp_choice]+1]
    n[tmp_g_decision]<-n[tmp_g_decision]+1
    individual_performance<-c(individual_performance,tmp_performance)
  }
  return(individual_performance)
}

cal_block_performance<-function(individual_performance){
  block_performance<-rep(0,10)
  for (b in 1:10){
    block_performance[b]=sum(individual_performance[(10*(b-1)+1):(10*b)])
  }
  return(block_performance)
}

cal_ss<-function(c){
  ss<-rep(0,10)
  for (i in 1:10){
    tmp_data=c[(10*(i-1)+1):(10*i)]
    n_d=length(which(tmp_data==11|tmp_data==12))
    n_a=length(which(tmp_data==13|tmp_data==14))
    ss[i]=n_a-n_d
    if(n_a+n_d!=10){
      break
    }
  }
  ss<-data.frame(c(1:10),ss)
  return(ss)
}

##########begin analyze

group_standard_score=c()
for(i in 1:dim(group_tag)[1]){
  sub_id=group_tag[i,1]
  if(sub_id<3000){
    if(sub_id<2000){
      discuss=0
    }else{
      discuss=1
    }
    leader=0
  }else{
    leader=1
    if(sub_id<4000){
      discuss=0
    }else{
      discuss=1
    }
  }
  tmp_data=no_leader_data[which(no_leader_data[,1]==sub_id),]
  tmp_score=sum(cal_ss(tmp_data[,13])$ss)
  group_standard_score<-rbind(group_standard_score,c(sub_id,tmp_score,discuss,leader))
}
group_standard_score<-as.data.frame(group_standard_score)
colnames(group_standard_score)<-c('sub_id','score','discuss','leader')
group_standard_score$discuss<-as.factor(group_standard_score$discuss)
group_standard_score$leader<-as.factor(group_standard_score$leader)

#### Fig5A  com vs. nocom, no leader
temp_performance <- subset(group_standard_score,leader==0)
Fig5A <- ggplot(temp_performance, aes(x = discuss, y = score, fill=discuss)) + 
  #rotate_x_text(angle = 45) +
  #scale_x_discrete(labels = c("1" = "Sensitivity","2" = "Decay","3" = "LearningRate","4" = "ExplorBonus","5" = "BetaOthers")) + #改x轴label名
  geom_violin(trim=FALSE,color="white") + 
  geom_boxplot(width=0.2,position=position_dodge(0.9))+
  geom_point(data=temp_performance,aes(x=discuss,y=score),color="grey",position = position_jitterdodge(dodge.width = 0.8,jitter.width = 1),alpha=0.9,size=6)+
  scale_fill_manual(values=c('#6699cc','#fc8d59'),labels=c("NoCom","Com"))+
  #scale_y_continuous(limits = c(-2,10), breaks = seq(-2,10,2))+
  pic_theme+ 
  ylab("Value")+xlab("Parameter name") #设置x轴和y轴的标题
Fig5A


individual_ss=c()
for (i in 1:length(individual_tag)){
  id=individual_tag[i]
  tmp_data=subset(IGT_data_individual,subNo==id)
  ss=c(id,sum(cal_ss(tmp_data$chose_performance)$ss),-1,-1)
  individual_ss=rbind(individual_ss,ss)
}
colnames(individual_ss)<-c('sub_id','score','discuss','leader')
individual_ss<-as.data.frame(individual_ss)

rownames(individual_ss)=c(1:dim(individual_ss)[1])
individual_ss$discuss<-as.factor(individual_ss$discuss)
individual_ss$leader<-as.factor(individual_ss$leader)
group_standard_score<-rbind(individual_ss,group_standard_score)


summary_group_standard_score<-data.frame(aggregate(.~discuss+leader,FUN='mean',data=group_standard_score)[,c(1,2,4)],(aggregate(.~discuss+leader,FUN='sd',data=group_standard_score))[4])
n<-c(39,31,28,31,28)
colnames(summary_group_standard_score)[c(3,4)]=c('mean','se')
summary_group_standard_score$se<-summary_group_standard_score$se/sqrt(n)

#plot figure 2a
Figure_2a <-
  ggplot(data = summary_group_standard_score, aes(x = discuss,y=mean,fill=leader,group=leader)) + 
  scale_fill_manual(values=c('#808080','#91bfdb','#fc8d59'),labels=c("Individual","No Leader","Leader"))+
  geom_col(width =c(0.4/2,rep(0.4,4)), position = position_dodge(width = 0.45),alpha=1)+
  #geom_boxplot(data=subset(group_standard_score,discuss==-1),aes(x=discuss,y=score,color=leader,fill=NULL), width=0.35,alpha=1,size=1)+
  #geom_boxplot(data=subset(group_standard_score,discuss!=-1),aes(x=discuss,y=score,color=leader,fill=NULL), width=0.7,position = position_dodge(width = 0.8),alpha=1,size=1)+
  geom_point(data=group_standard_score,aes(x=discuss,y=score,group=leader,color=leader),position = position_jitterdodge(dodge.width = 0.45,jitter.width = 0.2),alpha=0.4,size=6)+
  #geom_point(data = summary_group_standard_score, aes(x = discuss,y=mean,color=leader),size=6,position = position_dodge(width = 0.45))+
  geom_errorbar(position = position_dodge(width = 0.45),aes(x=discuss,ymin=mean-se,ymax=mean+se,group=leader),size=1,alpha=1,width=c(0.06,rep(0.12,4)))+
  scale_x_discrete("Discussion", labels = c("-1" = "Individual","0" = "No Discussion", "1" = "Discussion")) +
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual(values=c('#808080','#91bfdb','#fc8d59'),labels=c("Individual","No Leader","Leader"))+
  labs(x='Discussion Condition',y = "Group Standard Score",fill=c('Group')) +
  pic_theme+
  theme(legend.position = c(0.88, 0.10),
        legend.title=element_blank())

#figure 2a group comparison
aov1<-aov(score~discuss*leader,data=subset(group_standard_score,discuss!=-1))
eta_squared(aov1, partial = T)
#leader effect size
t_to_d(-2.163,115)
#communication effect size
t_to_d(-4.965,115)
#figure 2a group-independent individual comparison
group_list<-c()
for(g in 1:dim(group_standard_score)[1]){
  if (group_standard_score[g,]$discuss==-1){
    group='Individual'
  }else if (group_standard_score[g,]$leader==0&&group_standard_score[g,]$discuss==0){
    group='Control'
  }else if (group_standard_score[g,]$leader==0&&group_standard_score[g,]$discuss==1){
    group='Discuss'
  }else if (group_standard_score[g,]$leader==1&&group_standard_score[g,]$discuss==0){
    group='Leader'
  }else if (group_standard_score[g,]$leader==1&&group_standard_score[g,]$discuss==1){
    group='Mix'
  }
  group_list<-c(group_list,group)
}
group_standard_score<-data.frame(group_standard_score,group_list)
colnames(group_standard_score)[5]='Group'

aov_group<-aov(score~Group,data=group_standard_score)
eta_squared(aov_group, partial = T)
TukeyHSD(aov_group)
emmeans(aov_group,pairwise~Group)
t_to_d(3.769, 153)

tmp_t1<-t.test(subset(group_standard_score,Group=='Individual')$score,subset(group_standard_score,Group=='Control')$score)
tmp_t2<-t.test(subset(group_standard_score,Group=='Individual')$score,subset(group_standard_score,Group=='Discuss')$score)
tmp_t3<-t.test(subset(group_standard_score,Group=='Individual')$score,subset(group_standard_score,Group=='Leader')$score)
tmp_t4<-t.test(subset(group_standard_score,Group=='Individual')$score,subset(group_standard_score,Group=='Mix')$score)

p<-c(tmp_t1$p.value,tmp_t2$p.value,tmp_t3$p.value,tmp_t4$p.value)

p.adjust(p,method = 'bonferroni')


t_to_d(3.58, 51)
t_to_d(-2.62, 67)
t_to_d(-0.41, 59)
t_to_d(1.17, 55)

#####################figure 2b individual performance
individual_block_standard_score<-c()
for (i in 1:length(individual_tag)){
  sub_id=individual_tag[i]
  tmp_data=IGT_data_individual[which(IGT_data_individual$subNo==sub_id),]
  tmp_score=as.numeric(cal_ss(tmp_data$chose_performance)$ss)
  individual_block_standard_score<-rbind(individual_block_standard_score,data.frame(rep(sub_id,10),c(1:10),tmp_score,rep(-1,10),rep(-1,10)))
}
colnames(individual_block_standard_score)<-c('sub_id','block','group_performance','discuss','leader')


group_individual_block_standard_score<-c()
for (g in 1:dim(group_tag)[1]){
  for (i in 1:5){
    sub_id=group_tag[g,i] 
    discuss<-group_individual_class(sub_id,i)[1]
    leader<-group_individual_class(sub_id,i)[2]
    if(leader<2){
      tmp_data=no_leader_data[which(no_leader_data[,1]==sub_id),]
      tmp_s_decision=tmp_data[,11]
      tmp_g_decision=tmp_data[,13]
    }else{
      tmp_data=leader_data[which(leader_data[,1]==sub_id),]
      tmp_s_decision=tmp_data[,11]
      tmp_g_decision=tmp_data[,16]
    }
    tmp_score=as.numeric(cal_ss(tmp_data[,11])$ss)
    tmp_individual_performance<-as.numeric(cal_individual_performance(tmp_s_decision,tmp_g_decision))
    tmp_individual_block_performance<-cal_block_performance(tmp_individual_performance)
    group_individual_block_standard_score<-rbind(group_individual_block_standard_score,data.frame(rep(sub_id,10),c(1:10),tmp_score,tmp_individual_block_performance,rep(discuss,10),rep(leader,10)))
  }
}
colnames(group_individual_block_standard_score)<-c('sub_id','block','individual_performance','individual_score','discuss','leader')

group_individual_block_standard_score$discuss<-as.factor(group_individual_block_standard_score$discuss)
group_individual_block_standard_score$leader<-as.factor(group_individual_block_standard_score$leader)

individual_block_standard_score<-data.frame(individual_block_standard_score[,1:3],individual_block_standard_score$group_performance,individual_block_standard_score[,4:5])
colnames(individual_block_standard_score)<-c('sub_id','block','individual_performance','individual_score','discuss','leader')
group_individual_block_standard_score<-rbind(individual_block_standard_score,group_individual_block_standard_score)

individual_block_standard_score<-aggregate(.~block+discuss+leader,mean,data=group_individual_block_standard_score)

tmp_group_individual_block_standard_score<-subset(group_individual_block_standard_score,leader!=-1)

for (g in 1:dim(group_tag)[1]){
  id_list<-group_tag[g,]
  loc<-c()
  for(i in 1:5){
    loc<-c(loc,which(tmp_group_individual_block_standard_score[,1]==id_list[i]))
  }
  tmp_group_individual_block_standard_score[loc,]$individual_score<-g
}
colnames(tmp_group_individual_block_standard_score)[4]='group_id'


group_standard_score<-aggregate(.~sub_id+leader+discuss,FUN='sum',data=group_individual_block_standard_score)
group_standard_score<-subset(group_standard_score,leader!=-1)

summary_individual_virtual_performance<-data.frame(aggregate(.~discuss+leader,FUN='mean',data=group_standard_score[,c(1,2,3,5)]),(aggregate(.~discuss+leader,FUN='sd',data=group_standard_score))[5])
n<-c(155,140,124,112,31,28)
colnames(summary_individual_virtual_performance)[c(4,5)]=c('mean','se')
summary_individual_virtual_performance$se<-summary_individual_virtual_performance$se/sqrt(n)

Figure2b<-
  ggplot(data = summary_individual_virtual_performance, aes(x = discuss,y=mean,fill=leader,group=leader)) +  
  scale_fill_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c("No Leader","Non-Leading Member","Leader"))+
  geom_col(width =c(rep(0.4,6)),position = position_dodge(width = 0.55),alpha=1,size=1)+
  geom_point(data=group_standard_score,aes(x=discuss,y=individual_performance,group=leader,color=leader),position = position_jitterdodge(dodge.width = 0.55,jitter.width =0.08 ),alpha=0.4,size=6)+guides(color='none')+
  geom_errorbar(position = position_dodge(width = 0.55),aes(x=discuss,ymin=mean-se,ymax=mean+se,group=leader),size=1.2,alpha=1,width =c(rep(0.2,6)))+
  #geom_point(position = position_dodge(width = 0.8),aes(x=discuss,y=mean,group=leader),size=6,alpha=1)+
  scale_x_discrete( labels = c("0" = "No Discussion", "1" = "Discussion"))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'))+
  labs(y = "Individual Standard Score",fill=c('Group')) +
  pic_theme+
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2))

##use lm to comppare 
tmp_group_standard_score<-subset(group_standard_score,leader!=-1)

# for (g in 1:dim(group_tag)[1]){
#   id_list<-group_tag[g,]
#   loc<-c()
#   for(i in 1:5){
#     loc<-c(loc,which(tmp_group_standard_score[,1]==id_list[i]))
#   }
#   tmp_group_standard_score[loc,]$block<-g
# }
# colnames(tmp_group_standard_score)[4]='group_id'
# 
# tmp_group_standard_score$leader<-as.factor(tmp_group_standard_score$leader)
# tmp_group_standard_score$discuss<-as.factor(tmp_group_standard_score$discuss)
# 
# contrasts(tmp_group_standard_score$leader)<-cbind(c(1,0,-1),c(0,1,-1))
# contrasts(tmp_group_standard_score$discuss)<-c(-1,1)
# 
# 
# individual_performance_lm<-lmer(individual_performance~discuss*leader+(1|group_id),data=tmp_group_standard_score)
# anova(individual_performance_lm)
# emmeans(individual_performance_lm,pairwise~leader)
# emmeans(individual_performance_lm,pairwise~discuss)

individual_performance_aov<-aov(individual_performance~discuss*leader,data=tmp_group_standard_score)
TukeyHSD(individual_performance_aov)
emmeans(individual_performance_aov,pairwise~leader)
t_to_d(2.318,584)
eta_squared(individual_performance_aov, partial = T)

summary(individual_performance_aov)
emmeans(individual_performance_aov,pairwise~leader)
emmeans(individual_performance_aov,pairwise~discuss)


#prepare the data for each trial for model-free analysis in Figure 2c
#no leader data part
i_data_new<-c()
no_leader_id_list<-unique(no_leader_data[,1])
for (i in 1:length(no_leader_id_list)){
  tmp_id=no_leader_id_list[i]
  tmp_data<-no_leader_data[which(no_leader_data[,1]==tmp_id),]
  discuss=group_individual_class(tmp_id,4)[1]
  leader=group_individual_class(tmp_id,4)[2]
  tmp_i_data<-c()
  for (t in 2:100){
    stay=as.numeric(tmp_data[t-1,11]==tmp_data[t,11])
    reward=tmp_data[t-1,9]/100
    reward=as.numeric(reward>0)
    social_number=tmp_data[t-1,14:17]
    N=as.numeric(social_number[tmp_data[t-1,3]])
    inconsistence=as.numeric(tmp_data[t-1,11]!=tmp_data[t-1,13])
    choose_to_revealed=as.numeric(tmp_data[t,11]==tmp_data[t-1,13])
    leader_choose_stay=-1
    tmp_i_data<-rbind(tmp_i_data,data.frame(tmp_id,discuss,leader,stay,reward,N,inconsistence,choose_to_revealed,leader_choose_stay))
  }
  i_data_new<-rbind(i_data_new,tmp_i_data)
}

#leader data part
leader_id_list<-unique(leader_data[,1])
for (i in 1:length(leader_id_list)){
  tmp_id=leader_id_list[i]
  tmp_data<-leader_data[which(leader_data[,1]==tmp_id),]
  discuss=group_individual_class(tmp_id,5)[1]
  leader=group_individual_class(tmp_id,5)[2]
  tmp_i_data<-c()
  for (t in 2:100){
    stay=as.numeric(tmp_data[t-1,16]==tmp_data[t,16])
    reward=tmp_data[t-1,9]/100
    reward=as.numeric(reward>0)
    social_number=tmp_data[t,17:20]
    N=as.numeric(social_number[tmp_data[t,3]])
    inconsistence=-1
    choose_to_revealed=as.numeric(tmp_data[t,16]==tmp_data[t-1,16])
    leader_choose_stay<-as.numeric(tmp_data[t,11]==tmp_data[t,16])
    tmp_i_data<-rbind(tmp_i_data,data.frame(tmp_id,discuss,leader,stay,reward,N,inconsistence,choose_to_revealed,leader_choose_stay))
  }
  i_data_new<-rbind(i_data_new,tmp_i_data)
}

##plot the social influence-reward stay probabiliy
##plot social influence*reward stay/switch probability
plot_raw_data<-aggregate(.~leader+discuss+N+reward,FUN = 'mean',data = i_data_new)
plot_raw_data_sd<-aggregate(.~leader+discuss+N+reward,FUN = 'sd',data = i_data_new)
tmp_n<-aggregate(.~leader+discuss+N+reward,FUN = 'table',data = i_data_new)$stay
tmp_n<-apply(tmp_n,FUN='sum',1)
plot_raw_data<-data.frame(plot_raw_data[,c(1,2,3,4,6)],plot_raw_data_sd[,6])
colnames(plot_raw_data)[c(5,6)]<-c('stay_mean','stay_se')
plot_raw_data$stay_se<-plot_raw_data$stay_se/sqrt(tmp_n)

plot_raw_data$leader<-as.factor(plot_raw_data$leader)
plot_raw_data$discuss<-as.factor(plot_raw_data$discuss)
plot_raw_data$reward<-as.factor(plot_raw_data$reward)

##segment data
#no leader no discussion (to obtain other groups of plot, simply modify the subset setting)
tmp_data=subset(plot_raw_data,leader==0&discuss==0)
Figure2c<-ggplot(data=tmp_data,aes(color=reward,group=reward))+
  geom_line(aes(x=N,y=stay_mean),size=1)+
  #scale_x_discrete(labels=c('Loss','Gain'))+
  #geom_smooth(aes(x=N,y=stay),method='lm',se=F)
  geom_errorbar(aes(x=N,ymin=stay_mean-stay_se,ymax=stay_mean+stay_se),size=1.2,alpha=1,width=0.1)+
  scale_color_manual(values=c('#b2df8a','#1f78b4'))+
  scale_y_continuous(limits = c(0.2,0.8),breaks=seq(0,0.8,0.2))+
  ylab('Probability of Stay')+
  pic_theme

#tmp_data<-aggregate(.~leader+discuss+N+reward+tmp_id,FUN = 'mean',data = i_data_new)
tmp_data<-subset(i_data_new,leader==2&discuss==1)
tmp_data$reward<-as.factor(tmp_data$reward-0.5)
tmp_data$N<-scale(tmp_data$N,center = T,scale = F)
lm_social_reward<-glmer(stay~N*reward+(1|tmp_id),data=tmp_data,family='binomial')
summary(lm_social_reward)   
#anova(lm_social_reward)
# aov_social_reward<-aov(stay~N*reward+Error(tmp_id),data=tmp_data)
# summary(aov_social_reward)
# emmeans(aov_social_reward,pairwise~N)
# emmeans(aov_social_reward,pairwise~reward)    
# emmeans(aov_social_reward,pairwise~reward|N) 
# eta_squared(aov_social_reward)
car::Anova(lm_social_reward,type=3)

confint(lm_social_reward)

tmp_data1<-subset(tmp_data,reward=='0.5')
tmp_data2<-subset(tmp_data,reward=='-0.5')

lm_social_reward1<-glmer(stay~N+(1|tmp_id),data=tmp_data1,family='binomial')
summary(lm_social_reward1)

fixcoef <- fixef(lm_social_reward1)
OR <- exp(fixcoef)
print(OR)

lm_social_reward2<-glmer(stay~N+(1|tmp_id),data=tmp_data2,family='binomial')
summary(lm_social_reward2)

fixcoef <- fixef(lm_social_reward2)
OR <- exp(fixcoef)
print(OR)


##plot the inconsistence-reward stay probabiliy
##plot social influence*reward stay/switch probability
plot_raw_data<-aggregate(.~leader+discuss+inconsistence+reward+tmp_id,FUN = 'mean',data = i_data_new)

plot_raw_data$leader<-as.factor(plot_raw_data$leader)
plot_raw_data$discuss<-as.factor(plot_raw_data$discuss)
plot_raw_data$reward<-as.factor(plot_raw_data$reward)
plot_raw_data$inconsistence<-as.factor(plot_raw_data$inconsistence)  

#no leader no discussion (to obtain other groups of plot, simply modify the subset setting)
tmp_data=subset(plot_raw_data,leader==0&discuss==0)
Figure2d<-ggplot(data=tmp_data,aes(fill=reward))+
  geom_boxplot(aes(x=inconsistence,y=stay),size=1,notch = TRUE)+
  scale_fill_manual(values=c('#b2df8a','#1f78b4'))+
  ylab('Probability of Stay')+
  pic_theme

#tmp_data<-aggregate(.~leader+discuss+N+reward+tmp_id,FUN = 'mean',data = i_data_new)
tmp_data<-subset(i_data_new,leader==0&discuss==0)
tmp_data$reward<-as.factor(tmp_data$reward-0.5)
tmp_data$inconsistence<-as.factor(tmp_data$inconsistence-0.5)
lm_reward_incon<-glmer(stay~inconsistence*reward+(1|tmp_id),data=tmp_data,family = 'binomial')
summary(lm_reward_incon)

car::Anova(lm_reward_incon,type=3)

confint(lm_reward_incon)

tmp_data1<-subset(tmp_data,reward=='0.5')
tmp_data2<-subset(tmp_data,reward=='-0.5')

lm_reward_incon1<-glmer(stay~inconsistence+(1|tmp_id),data=tmp_data1,family='binomial')
summary(lm_reward_incon1)

fixcoef <- fixef(lm_reward_incon1)
OR <- exp(fixcoef)
print(OR)

lm_reward_incon2<-glmer(stay~inconsistence+(1|tmp_id),data=tmp_data2,family='binomial')
summary(lm_reward_incon2)

fixcoef <- fixef(lm_reward_incon2)
OR <- exp(fixcoef)
print(OR)

##social influence on exploration (for supplementary figure codes, please see social_influence_on_exploration.R)
#prepare the data for each trial for model-free analysis
#members of no leader data part
i_data_socialexplore <-c()
no_leader_id_list<-unique(no_leader_data[,1])
no_leader_id_list<-no_leader_id_list[no_leader_id_list<3000] # members of no leader groups
for (i in 1:length(no_leader_id_list)){
  tmp_id=no_leader_id_list[i]
  tmp_data<-no_leader_data[which(no_leader_data[,1]==tmp_id),]
  discuss=group_individual_class(tmp_id,4)[1]
  leader=group_individual_class(tmp_id,4)[2]
  tmp_i_data<-c()
  for (t in 2:100){
    
    #tmp_four_social <- tmp_data[t-1,14:17]
    tmp_four_social <- c(tmp_data[t-1,14],tmp_data[t-1,15],tmp_data[t-1,16],tmp_data[t-1,17])
    first_max <- sort(tmp_four_social,decreasing = T)[1]
    second_max <- sort(tmp_four_social,decreasing = T)[2]
    third_max <- sort(tmp_four_social,decreasing = T)[3]
    forth_max <- sort(tmp_four_social,decreasing = T)[4]
    first_max_position <- which(tmp_four_social==first_max)
    second_max_position <- which(tmp_four_social == second_max)
    third_max_position <- which(tmp_four_social == third_max)
    forth_max_position <- which(tmp_four_social == forth_max)
    
    if(length(first_max_position)==1){
      if(length(second_max_position)==1){ # 4,1,0,0;3,2,0,0
        if(tmp_data[t,3]==first_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,3]==second_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,3]==third_max_position[1] | tmp_data[t,3]==forth_max_position[2]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(length(second_max_position)==2){ # 3,1,1,0
        if(tmp_data[t,3]==first_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,3]==second_max_position[1] | tmp_data[t,3]==third_max_position[2]){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,3]==forth_max_position){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(length(second_max_position)==3){ # 5,0,0,0; 2,1,1,1
        Highsocial_chose <- -666
        Lowsocial_chose <- -666
      }
    }else if(length(first_max_position)==2){  # 2, 2 ,1 ,0; the max may the first 2 or the second 2
      if(tmp_data[t-1,12]==first_max_position[1]){
        if(tmp_data[t,3]==first_max_position[1] | tmp_data[t,3]==third_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,3]==first_max_position[2]){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,3]==forth_max_position){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(tmp_data[t-1,12]==first_max_position[2]){
        if(tmp_data[t,3]==first_max_position[2] | tmp_data[t,3]==third_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,3]==first_max_position[1]){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,3]==forth_max_position){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }  
    }
    
    
    individual_choice = tmp_data[t,3]
    group_choice_last = tmp_data[t-1,12]
    social_number_last=tmp_data[t-1,14:17]
    tmp_i_data<-rbind(tmp_i_data,data.frame(tmp_id,discuss,leader,individual_choice,group_choice_last,social_number_last,Highsocial_chose,Lowsocial_chose))
  }
  i_data_socialexplore <-rbind(i_data_socialexplore,tmp_i_data)
}

##the effect of social influence on exploration, members of no leader groups
plot_raw_data2<-subset(i_data_socialexplore,Highsocial_chose!=-666 & Lowsocial_chose!=-666)
table(plot_raw_data2$Highsocial_chose)
table(plot_raw_data2$Lowsocial_chose)
plot_raw_data2<-aggregate(.~tmp_id,FUN = 'mean',data=plot_raw_data2)
tmp_plot_raw_data2<-data.frame(plot_raw_data2,rep(0,dim(plot_raw_data2)[1]))
tmp_plot_raw_data2$Highsocial_chose=1-tmp_plot_raw_data2$Highsocial_chose
plot_raw_data2<-data.frame(plot_raw_data2,rep(1,dim(plot_raw_data2)[1]))
colnames(plot_raw_data2)[c(10,12)]=c('Probability','Type')
colnames(tmp_plot_raw_data2)[c(10,12)]=c('Probability','Type')
plot_raw_data2<-rbind(plot_raw_data2,tmp_plot_raw_data2)
plot_raw_data2$Type=as.factor(plot_raw_data2$Type)
#segment data
#members of no leader groups
tmp_data<-subset(plot_raw_data2, leader==0&discuss==0) # two conditions: leader==0 & discuss == 0 or 1


Figure2e<-ggplot(data=tmp_data)+
  geom_point(aes(x=Type,y=Probability),size=10,color='skyblue',alpha=0.8)+
  geom_line(aes(x=(Type),y=Probability,group=tmp_id),linetype='dashed',size=1,alpha=0.1)+
  scale_x_discrete(labels=c('Minimum N','Maximum N'))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  ylab("Probablitiy of exploration")+
  pic_theme

t.test(subset(tmp_data,Type==0)$Probability,subset(tmp_data,Type==1)$Probability,paired = T)

t_to_d(16.798,154)
t_to_d(19.259,138)



##individual-group inconsistence on exploration
  plot_raw_data2<-subset(i_data_new,inconsistence==1 & choose_to_revealed!=1)
  plot_raw_data2<-aggregate(.~tmp_id,FUN = 'mean',data=plot_raw_data2)
  tmp_plot_raw_data2<-data.frame(plot_raw_data2,rep(1,dim(plot_raw_data2)[1]))
  tmp_plot_raw_data2$stay=1-tmp_plot_raw_data2$stay
  plot_raw_data2<-data.frame(plot_raw_data2,rep(0,dim(plot_raw_data2)[1]))
  colnames(plot_raw_data2)[c(4,10)]=c('Probability','Type')
  colnames(tmp_plot_raw_data2)[c(4,10)]=c('Probability','Type')
  plot_raw_data2<-rbind(plot_raw_data2,tmp_plot_raw_data2)
  plot_raw_data2$Type=as.factor(plot_raw_data2$Type)
#segment data
  #no leader no discussion
  tmp_data<-subset(plot_raw_data2, leader==0&discuss==0)
  
  ggplot(data=tmp_data)+
    geom_point(aes(x=Type,y=Probability),size=10,color='skyblue',alpha=0.8)+
    geom_line(aes(x=(Type),y=Probability,group=tmp_id),linetype='dashed',size=1,alpha=0.1)+
    scale_x_discrete(labels=c('Stay','Switch'))+
    pic_theme
 
   t.test(subset(tmp_data,Type==0)$Probability,subset(tmp_data,Type==1)$Probability,paired = T)
   
   t_to_d(8.702,154)
   t_to_d(13.033,139)
   t_to_d(11.528,123)
   t_to_d(10.467,111)
##leader inconsistence ratio vs. consistence ratio
   
   plot_raw_data3<-subset(i_data_new,leader_choose_stay!=-1)
   plot_raw_data3<-aggregate(.~tmp_id,FUN = 'mean',data=plot_raw_data3)
   tmp_plot_raw_data3<-data.frame(plot_raw_data3,rep(1,dim(plot_raw_data3)[1]))
   tmp_plot_raw_data3$leader_choose_stay=1-tmp_plot_raw_data3$leader_choose_stay
   plot_raw_data3<-data.frame(plot_raw_data3,rep(0,dim(plot_raw_data3)[1]))
   colnames(plot_raw_data3)[c(9,10)]=c('Probability','Type')
   colnames(tmp_plot_raw_data3)[c(9,10)]=c('Probability','Type')
   plot_raw_data3<-rbind(plot_raw_data3,tmp_plot_raw_data3)
   plot_raw_data3$Type=as.factor(plot_raw_data3$Type)
   
   tmp_data<-subset(plot_raw_data3, leader==2&discuss==1)
   
   Figure2f<-ggplot(data=tmp_data)+
     geom_point(aes(x=Type,y=Probability),size=10,color='skyblue',alpha=0.8)+
     geom_line(aes(x=(Type),y=Probability,group=tmp_id),linetype='dashed',size=1,alpha=0.1)+
     scale_x_discrete(labels=c('Stay','Switch'))+
     pic_theme
   
   t.test(subset(tmp_data,Type==0)$Probability,subset(tmp_data,Type==1)$Probability,paired = T)
   
   t_to_d(7.650,30)
   t_to_d(7.970,27)