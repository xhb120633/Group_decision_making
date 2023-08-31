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

#prepare for the block standard score data
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
group_block_mean_score$block<-as.numeric(group_block_mean_score$block)

Figure_s2a<-
  ggplot(data = group_block_mean_score, aes(x = block,y = group_performance)) + 
  geom_line(aes(color=leader, linetype=discuss),size=1.5,alpha=1)+
  geom_point(aes(color=leader,shape=discuss),size=3)+guides(shape='none')+
  scale_linetype_manual(values=c('solid','dashed','solid'),labels = c("-1" = "Individual","0" = "No Discussion", "1" = "Discussion"))+
  scale_color_manual(values=c('#808080','#91bfdb','#fc8d59'),labels=c("Individual","No Leader","Leader"))+
  scale_x_continuous( breaks=seq(0, 10, 2))+
  scale_y_continuous( breaks=seq(-6, 6, 3))+
  labs(x='Block',y = "Standard Score") +
  pic_theme+
  theme(legend.position = c(0.88, 0.30),
        legend.title=element_blank())

##Figure s2b individual block standard score
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
individual_block_standard_score<-subset(individual_block_standard_score,leader!=-1)
##plot figure2d
Figure_s2b<-ggplot(data = individual_block_standard_score, aes(x = block,y = individual_performance)) + 
  geom_line(aes(color=leader, linetype=discuss),size=1.5,alpha=1)+
  scale_linetype_manual(values=c('solid','dashed','solid'),labels = c("0" = "No Discussion", "1" = "Discussion"))+
  scale_color_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c("0" = "No Leader", "1" = "Non-leading Member",'2'='Leader'))+
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


