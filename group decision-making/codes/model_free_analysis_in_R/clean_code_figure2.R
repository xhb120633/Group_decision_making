##code for figure2
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

group_standard_score=c()
for(i in 1:dim(group_tag)[1]){
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
  sub_id=group_tag[i,1]
  tmp_data=no_leader_data[which(no_leader_data[,1]==sub_id),]
  tmp_score=sum(cal_ss(tmp_data[,13])$ss)
  group_standard_score<-rbind(group_standard_score,c(sub_id,tmp_score,discuss,leader))
}
group_standard_score<-as.data.frame(group_standard_score)
colnames(group_standard_score)<-c('sub_id','score','discuss','leader')
group_standard_score$discuss<-as.factor(group_standard_score$discuss)
group_standard_score$leader<-as.factor(group_standard_score$leader)

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
n<-c(39,31,28,32,28)
colnames(summary_group_standard_score)[c(3,4)]=c('mean','se')
summary_group_standard_score$se<-summary_group_standard_score$se/sqrt(n)

#plot figure 2a
Figure_2a <-
  ggplot(data = summary_group_standard_score, aes(x = discuss,y=mean)) + 
  #scale_fill_manual(values=c('#fb9a99','#33a02c','#1f78b4'),labels=c("Individual","No Leader","Leader"))+
  #geom_col(width =c(0.4/2,rep(0.4,4)), position = position_dodge(width = 0.45),alpha=0.6)+
  geom_boxplot(data=subset(group_standard_score,discuss==-1),aes(x=discuss,y=score,color=leader,fill=NULL), width=0.35,alpha=1,size=1)+
  geom_boxplot(data=subset(group_standard_score,discuss!=-1),aes(x=discuss,y=score,color=leader,fill=NULL), width=0.7,position = position_dodge(width = 0.8),alpha=1,size=1)+
  geom_point(data=group_standard_score,aes(x=discuss,y=score,group=leader,color=leader),position = position_jitterdodge(dodge.width = 0.8,jitter.width = 0.2),alpha=0.5,size=6)+
  #geom_errorbar(position = position_dodge(width = 0.8),aes(x=discuss,ymin=mean-se,ymax=mean+se,group=leader,color=leader),size=1,alpha=1,width=c(0.06,rep(0.12,4)))+
  #geom_point(data = summary_group_standard_score, aes(x = discuss,y=mean,color=leader),size=6,position = position_dodge(width = 0.8))+
  scale_x_discrete("Discussion", labels = c("-1" = "Individual","0" = "No Discussion", "1" = "Discussion")) +
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual(values=c('#fb9a99','#33a02c','#1f78b4'),labels=c("Individual","No Leader","Leader"))+
  labs(x='Discussion Condition',y = "Group Standard Score",fill=c('Group')) +
  pic_theme+
  theme(legend.position = c(0.88, 0.10),
        legend.title=element_blank())

#figure 2a group comparison
aov1<-aov(score~discuss*leader,data=subset(group_standard_score,discuss!=-1))
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
TukeyHSD(aov_group)
emmeans(aov_group,pairwise~Group)
##figure 2b data
group_block_standard_score<-c()
for (g in 1:dim(group_tag)[1]){
  sub_id=group_tag[g,1]
  tmp_data=no_leader_data[which(no_leader_data[,1]==sub_id),]
  tmp_score=as.numeric(cal_ss(tmp_data[,13])$ss)
  discuss<-group_class(sub_id)[1]
  leader<-group_class(sub_id)[2]
  group_block_standard_score<-rbind(group_block_standard_score,data.frame(rep(sub_id,10),c(1:10),tmp_score,rep(discuss,10),rep(leader,10)))
}
colnames(group_block_standard_score)<-c('sub_id','block','group_performance','discuss','leader')
group_block_standard_score$discuss<-as.factor(group_block_standard_score$discuss)
group_block_standard_score$leader<-as.factor(group_block_standard_score$leader)
group_block_standard_score$block<-as.numeric(group_block_standard_score$block)
#lm fitting and post-hoc comparison 
group_lm1<-lm(group_performance~block*discuss*leader,data=group_block_standard_score)
group_lm2<-lm(group_performance~(block+I(block^2))*discuss*leader,data=group_block_standard_score)

group_lm1<-aov(group_performance~block*discuss*leader,data=group_block_standard_score)
group_block_standard_score$block<-as.factor(group_block_standard_score$block)
summary(aov(group_performance~block*discuss*leader,data=group_block_standard_score))
anova(group_lm1)
emmeans(group_lm1,pairwise~discuss|block)
emmeans(group_lm1,pairwise~leader|block)


#plot figure 2b
individual_block_standard_score<-c()
for (i in 1:length(individual_tag)){
  sub_id=individual_tag[i]
  tmp_data=IGT_data_individual[which(IGT_data_individual$subNo==sub_id),]
  tmp_score=as.numeric(cal_ss(tmp_data$chose_performance)$ss)
  individual_block_standard_score<-rbind(individual_block_standard_score,data.frame(rep(sub_id,10),c(1:10),tmp_score,rep(-1,10),rep(-1,10)))
}
colnames(individual_block_standard_score)<-c('sub_id','block','group_performance','discuss','leader')

group_block_standard_score<-rbind(individual_block_standard_score,group_block_standard_score)

group_block_mean_score<-aggregate(group_performance~block+discuss+leader,mean,data=group_block_standard_score)

Figure2b<-
  ggplot(data = group_block_mean_score, aes(x = block,y = group_performance)) + 
  geom_line(aes(color=leader, linetype=discuss),size=1.5,alpha=1)+
  geom_point(aes(color=leader,shape=discuss),size=3)+guides(shape=FALSE)+
  scale_linetype_manual(values=c('solid','dashed','solid'),labels = c("-1" = "Individual","0" = "No Discussion", "1" = "Discussion"))+
  scale_color_manual(values=c('#fb9a99','#33a02c','#1f78b4'),labels=c("Individual","No Leader","Leader"))+
  scale_x_continuous( breaks=seq(0, 10, 2))+
  scale_y_continuous( breaks=seq(-6, 6, 3))+
  labs(x='Block',y = "Standard Score") +
  pic_theme+
  theme(legend.position = c(0.88, 0.30),
        legend.title=element_blank())

##Figure 2d individual standard score 
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

##plot figure2d
ggplot(data = individual_block_standard_score, aes(x = block,y = individual_performance)) + 
  geom_line(aes(color=leader, linetype=discuss),size=1.5,alpha=1)+
  scale_linetype_manual(values=c('solid','dashed','solid'),labels = c("-1" = "Individual","0" = "No Discussion", "1" = "Discussion"))+
  scale_color_manual(values=c('#fb9a99','#33a02c','#a6cee3','#1f78b4'),labels=c("-1" = "Individual","0" = "No Leader", "1" = "Non-leading Member",'2'='Leader'))+
  scale_x_continuous( breaks=seq(0, 10, 2))+
  #scale_y_continuous( breaks=seq(-6, 6, 3))+
  labs(x='Block',y = "Earned Score") +
  pic_theme+
  theme(legend.position = c(0.88, 0.30),
        legend.title=element_blank())

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

##lm fitting and post-hoc comparison
block_score_lm<-lmer(individual_performance~block*discuss*leader+(1|sub_id)+(1|group_id),data=tmp_group_individual_block_standard_score,contrasts=list(discuss=c(-1,1), leader=cbind(c(0,-1,1),c(-1,0,1))))
anova(block_score_lm)
emmeans(block_score_lm,pairwise~discuss|block)
emmeans(block_score_lm,pairwise~leader|block,p.adjust.methods='bonf')

block_score_aov<-aov(individual_performance~block*discuss*leader,data=tmp_group_individual_block_standard_score)
emmeans(block_score_aov,pairwise~discuss|block)
emmeans(block_score_aov,pairwise~leader|block,p.adjust.methods='bonf')


##fig 2c
group_standard_score<-aggregate(.~sub_id+leader+discuss,FUN='sum',data=group_individual_block_standard_score)

summary_individual_virtual_performance<-data.frame(aggregate(.~discuss+leader,FUN='mean',data=group_standard_score[,c(1,2,3,5)]),(aggregate(.~discuss+leader,FUN='sd',data=group_standard_score))[5])
n<-c(39,155,140,128,112,32,28)
colnames(summary_individual_virtual_performance)[c(4,5)]=c('mean','se')
summary_individual_virtual_performance$se<-summary_individual_virtual_performance$se/sqrt(n)

Figure2e<-
  ggplot(data = summary_individual_virtual_performance, aes(x = discuss,y=mean)) +  
  #scale_fill_manual(values=c('#fb9a99','#33a02c','#a6cee3','#1f78b4'),labels=c('Individual',"No Leader","Non-Leading Member","Leader"))+
  geom_violin(data=subset(group_standard_score,discuss==-1),aes(x=discuss,y=individual_performance,color=leader,fill=NULL), width=0.2,alpha=1,size=1)+
  geom_violin(data=subset(group_standard_score,discuss!=-1),aes(x=discuss,y=individual_performance,color=leader,fill=NULL), width=0.6,position = position_dodge(width = 0.8),alpha=1,size=1)+
  geom_point(data=group_standard_score,aes(x=discuss,y=individual_performance,group=leader,color=leader),position = position_jitterdodge(dodge.width = 0.8,jitter.width =0.2 ),alpha=0.4,size=6)+guides(color=FALSE)+
  geom_errorbar(position = position_dodge(width = 0.8),aes(x=discuss,ymin=mean-se,ymax=mean+se,group=leader),size=1.2,alpha=1,width=0.1)+
  geom_point(position = position_dodge(width = 0.8),aes(x=discuss,y=mean,group=leader),size=6,alpha=1)+
  scale_x_discrete( labels = c('-1'='Individual',"0" = "No Discussion", "1" = "Discussion"))+
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual(values=c('#fb9a99','#33a02c','#a6cee3','#1f78b4'))+
  labs(y = "Individual Standard Score",fill=c('Group')) +
  pic_theme+
  theme(legend.title=element_blank(),legend.position = c(0.8, 0.2))

##use lm to comppare 
tmp_group_standard_score<-subset(group_standard_score,leader!=-1)

for (g in 1:dim(group_tag)[1]){
  id_list<-group_tag[g,]
  loc<-c()
  for(i in 1:5){
    loc<-c(loc,which(tmp_group_standard_score[,1]==id_list[i]))
  }
  tmp_group_standard_score[loc,]$block<-g
}
colnames(tmp_group_standard_score)[4]='group_id'

tmp_group_standard_score$leader<-as.factor(tmp_group_standard_score$leader)
tmp_group_standard_score$discuss<-as.factor(tmp_group_standard_score$discuss)

contrasts(tmp_group_standard_score$leader)<-cbind(c(1,0,-1),c(0,1,-1))
contrasts(tmp_group_standard_score$discuss)<-c(-1,1)


individual_performance_lm<-lmer(individual_performance~discuss*leader+(1|group_id),data=tmp_group_standard_score)
anova(individual_performance_lm)
emmeans(individual_performance_lm,pairwise~leader)
emmeans(individual_performance_lm,pairwise~discuss)

individual_performance_aov<-aov(individual_performance~discuss*leader,data=tmp_group_standard_score)
summary(individual_performance_aov)
emmeans(individual_performance_aov,pairwise~leader)
emmeans(individual_performance_aov,pairwise~discuss)
####boostrap correlation
boostrap_matrix<-matrix(rep(-1,595000),nrow=119,ncol=5000)
#random id
for (g in 1:dim(boostrap_matrix)[1]){
  if(g<60){
    boostrap_matrix[g,]=sample(c(1:5),5000,replace=T)
  }else{
    boostrap_matrix[g,]=sample(c(1:4),5000,replace=T)
  }
}

group_boostrap_matrix<-matrix(rep(-1,595000),nrow=119,ncol=5000)
for (i in 1:5000){
  group_boostrap_matrix[1:31,i]=sample(c(1:31),31,replace=T)
  group_boostrap_matrix[32:59,i]=sample(c(32:59),28,replace=T)
  group_boostrap_matrix[60:91,i]=sample(c(60:91),32,replace=T)
  group_boostrap_matrix[92:119,i]=sample(c(92:119),28,replace=T)
}

#extract data following boostap matrix
no_leader_nd_r<-c()
no_leader_d_r<-c()
non_leading_nd_r<-c()
non_leading_d_r<-c()
leader_leading_nd_r<-c()
leader_leading_d_r<-c()
for (i in 1:5000){
  tmp_data<-c()
  for (ig in 1:119){
    g=group_boostrap_matrix[ig,i]
    if(g<60){
      s=boostrap_matrix[g,i]
      sub_id=group_tag[g,s]
      tmp_data<-rbind(tmp_data,group_standard_score[which(group_standard_score[,1]==sub_id),])
    }else{
      s=boostrap_matrix[g,i]
      sub_id=group_tag[g,s]
      tmp_data<-rbind(tmp_data,group_standard_score[which(group_standard_score[,1]==sub_id),])
      sub_id=group_tag[g,5]
      tmp_data<-rbind(tmp_data,group_standard_score[which(group_standard_score[,1]==sub_id),])
    }
  }
  tmp_no_leader_nd_data<-subset(tmp_data,leader==0&discuss==0)
  tmp_no_leader_d_data<-subset(tmp_data,leader==0&discuss==1)
  tmp_non_leading_nd_data<-subset(tmp_data,leader==1&discuss==0)
  tmp_non_leading_d_data<-subset(tmp_data,leader==1&discuss==1)
  tmp_leader_nd_data<-subset(tmp_data,leader==2&discuss==0)
  tmp_leader_d_data<-subset(tmp_data,leader==2&discuss==1)
  
  tmp_r1<-as.numeric(cor.test(scale(tmp_no_leader_nd_data$individual_performance),scale(tmp_no_leader_nd_data$group_score))$estimate)
  tmp_r2<-as.numeric(cor.test(scale(tmp_no_leader_d_data$individual_performance),scale(tmp_no_leader_d_data$group_score))$estimate)
  
  tmp_r3<-as.numeric(cor.test(scale(tmp_non_leading_nd_data$individual_performance),scale(tmp_non_leading_nd_data$group_score))$estimate)
  tmp_r4<-as.numeric(cor.test(scale(tmp_non_leading_d_data$individual_performance),scale(tmp_non_leading_d_data$group_score))$estimate)
  
  tmp_r5<-as.numeric(cor.test(scale(tmp_leader_nd_data$individual_performance),scale(tmp_leader_nd_data$group_score))$estimate)
  tmp_r6<-as.numeric(cor.test(scale(tmp_leader_d_data$individual_performance),scale(tmp_leader_d_data$group_score))$estimate)
  
  no_leader_nd_r<-c(no_leader_nd_r,tmp_r1)
  no_leader_d_r<-c(no_leader_d_r,tmp_r2)
  
  non_leading_nd_r<-c(non_leading_nd_r,tmp_r3)
  non_leading_d_r<-c(non_leading_d_r,tmp_r4)
  
  leader_leading_nd_r<-c(leader_leading_nd_r,tmp_r5)
  leader_leading_d_r<-c(leader_leading_d_r,tmp_r6)
}


cocor.dep.groups.overlap(mean(non_leading_r), leader_r, mean(leader_leading_r), 60, alternative = "two.sided",
                         test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0,
                         data.name = NULL, var.labels = NULL, return.htest = FALSE)

cocor.indep.groups(mean(non_leading_r), mean(no_leader_r), 240, 295, alternative = "two.sided",
                   test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0,
                   data.name = NULL, var.labels = NULL, return.htest = FALSE)

cocor.indep.groups(leader_r, mean(no_leader_r), 60, 295, alternative = "two.sided",
                   test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0,
                   data.name = NULL, var.labels = NULL, return.htest = FALSE)


cocor.indep.groups(mean(leader_leading_nd_r), mean(leader_leading_d_r), 32, 28, alternative = "two.sided",
                   test = "all", alpha = 0.05, conf.level = 0.95, null.value = 0,
                   data.name = NULL, var.labels = NULL, return.htest = FALSE)


no_leader_nd_r<-as.data.frame(no_leader_nd_r)
no_leader_nd_r<-data.frame(no_leader_nd_r,rep(0,5000),rep(0,5000))
colnames(no_leader_nd_r)<-c('r','discuss','leader')

no_leader_d_r<-as.data.frame(no_leader_d_r)
no_leader_d_r<-data.frame(no_leader_d_r,rep(1,5000),rep(0,5000))
colnames(no_leader_d_r)<-c('r','discuss','leader')

non_leading_nd_r<-as.data.frame(non_leading_nd_r)
non_leading_nd_r<-data.frame(non_leading_nd_r,rep(0,5000),rep(1,5000))
colnames(non_leading_nd_r)<-c('r','discuss','leader')

non_leading_d_r<-as.data.frame(non_leading_d_r)
non_leading_d_r<-data.frame(non_leading_d_r,rep(1,5000),rep(1,5000))
colnames(non_leading_d_r)<-c('r','discuss','leader')

leader_leading_nd_r<-as.data.frame(leader_leading_nd_r)
leader_leading_nd_r<-data.frame(leader_leading_nd_r,rep(0,5000),rep(2,5000))
colnames(leader_leading_nd_r)<-c('r','discuss','leader')

leader_leading_d_r<-as.data.frame(leader_leading_d_r)
leader_leading_d_r<-data.frame(leader_leading_d_r,rep(1,5000),rep(2,5000))
colnames(leader_leading_d_r)<-c('r','discuss','leader')

boostrapped_r<-rbind(no_leader_nd_r,no_leader_d_r,non_leading_nd_r,non_leading_d_r,leader_leading_nd_r,leader_leading_d_r)
boostrapped_r$r<-fisherz(boostrapped_r$r)
boostrapped_r$discuss<-as.factor(boostrapped_r$discuss)
boostrapped_r$leader<-as.factor(boostrapped_r$leader)

boostrapped_r$discuss<-revalue(boostrapped_r$discuss,c('0'='No Discussion','1'='Discussion'))
boostrapped_r$leader<-revalue(boostrapped_r$leader,c('0'='No Leader Group Member','1'='Non-leading Member','2'='Leader'))

r_ci<-c()
discuss_list<-unique(boostrapped_r$discuss)
leader_list<-unique(boostrapped_r$leader)
for (l in 1:length(leader_list)){
  for (d in 1:length(discuss_list)){
    tmp_data<-subset(boostrapped_r,discuss==discuss_list[d]&leader==leader_list[l])$r
    tmp_data1<-(as.numeric(t.test(tmp_data)$conf.int))
    r_ci<-rbind(r_ci,tmp_data1)
  }
}

r_ci<-data.frame(r_ci,rep(c('No Discussion','Discussion'),3),c(rep('No Leader Group Member',2),rep('Non-leading Member',2),rep('Leader',2)))
colnames(r_ci)<-c('lower_ci','upper_ci','discuss','leader')
factor(r_ci$leader)
factor(r_ci$discuss)

ggplot(data=boostrapped_r,aes(x=discuss,y=r,fill=NULL,color=leader))+
  geom_boxplot(notch=T,size=0.8)+
  #geom_vline(data=r_ci,aes(xintercept = lower_ci),color='red')+
  #geom_vline(data=r_ci,aes(xintercept = upper_ci),color='red')+
  scale_y_continuous(expand=c(0,0))+
  scale_color_manual(values=c('#33a02c','#a6cee3','#1f78b4'))+
  #facet_grid(rows=vars(leader),cols=vars(discuss))+guides(fill=F)+
  pic_theme+guides(color=F)


ggplot(data=boostrapped_r,aes(x=r,fill=NULL,color=leader))+
  geom_density()

a<-list(subset(boostrapped_r,discuss=='No Discussion')$r,subset(boostrapped_r,discuss=='Discussion')$r)
out<-overlap(a,plot=TRUE)

b<-list(subset(boostrapped_r,leader=='No Leader Group Member')$r,subset(boostrapped_r,leader=='Non-leading Member')$r,subset(boostrapped_r,leader=='Leader')$r)
out<-overlap(b,plot=TRUE)



hdi(subset(boostrapped_r,discuss=='No Discussion')$r, credMass = 0.8)
hdi(subset(boostrapped_r,discuss=='Discussion')$r, credMass = 0.8)

hdi(subset(boostrapped_r,leader=='No Leader Group Member')$r, credMass = 0.8)
hdi(subset(boostrapped_r,leader=='Non-leading Member')$r, credMass = 0.8)
hdi(subset(boostrapped_r,leader=='Leader')$r, credMass = 0.8)

t.test(subset(boostrapped_r,leader=='No Leader Group Member')$r)$conf.int
t.test(subset(boostrapped_r,leader=='Non-leading Member')$r)$conf.int
t.test(subset(boostrapped_r,leader=='Leader')$r)$conf.int


tmp_data<-parameter_S[,c(1:5,7:10,14:17)]
write.table(file='data_for_SEM.txt',tmp_data,row.names = F)