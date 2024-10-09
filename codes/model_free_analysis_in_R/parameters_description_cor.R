#correlation between model parameters and social identity
library(ggplot2)
library(dplyr)

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

##load parameter_S first
setwd("E:/CCNU_work/Projects/IGT_paper1/Data")
parameter_S= read.table("./parameter_study1and2.txt",header=T)

parameter_S = subset(parameter_S, leader == 0)
##################
####  study 1 ####
##################
parameter_study1 <- subset(parameter_S,discuss_list == 0)
#calculate correlation, identity1
r_Sensitivity <- cor.test(parameter_study1$Sensitivity,parameter_study1$identity1)
rtest_Sensitivity <- cor.test(parameter_study1$Sensitivity,parameter_study1$identity1)
rtest_Decay <- cor.test(parameter_study1$Decay,parameter_study1$identity1)
rtest_LearningRate <- cor.test(parameter_study1$LearningRate,parameter_study1$identity1)
rtest_ExplorBonus <- cor.test(parameter_study1$ExplorBonus,parameter_study1$identity1)
rtest_BetaOthers <- cor.test(parameter_study1$BetaOthers,parameter_study1$identity1)

#calculate correlation, identity2
r2_Sensitivity <- cor(parameter_study1$Sensitivity,parameter_study1$identity2)
rtest2_Sensitivity <- cor.test(parameter_study1$Sensitivity,parameter_study1$identity2)
rtest2_Decay <- cor.test(parameter_study1$Decay,parameter_study1$identity2)
rtest2_LearningRate <- cor.test(parameter_study1$LearningRate,parameter_study1$identity2)
rtest2_ExplorBonus <- cor.test(parameter_study1$ExplorBonus,parameter_study1$identity2)
rtest2_BetaOthers <- cor.test(parameter_study1$BetaOthers,parameter_study1$identity2)


#calculate correlation,responsibility1
rtest3_Sensitivity <- cor.test(parameter_study1$Sensitivity,parameter_study1$responsibility1)
rtest3_Decay <- cor.test(parameter_study1$Decay,parameter_study1$responsibility1)
rtest3_LearningRate <- cor.test(parameter_study1$LearningRate,parameter_study1$responsibility1)
rtest3_ExplorBonus <- cor.test(parameter_study1$ExplorBonus,parameter_study1$responsibility1)
rtest3_BetaOthers <- cor.test(parameter_study1$BetaOthers,parameter_study1$responsibility1)  ##significant, -0.189


#calculate correlation, responsibility2
rtest4_Sensitivity <- cor.test(parameter_study1$Sensitivity,parameter_study1$responsibility2)
rtest4_Decay <- cor.test(parameter_study1$Decay,parameter_study1$responsibility2)
rtest4_LearningRate <- cor.test(parameter_study1$LearningRate,parameter_study1$responsibility2)
rtest4_ExplorBonus <- cor.test(parameter_study1$ExplorBonus,parameter_study1$responsibility2)
rtest4_BetaOthers <- cor.test(parameter_study1$BetaOthers,parameter_study1$responsibility2) 


Fig5E <-ggplot(data=parameter_study1,aes(x=responsibility1,y=BetaOthers))+
  geom_point(size=14,color= "#13A4E5", alpha=1)+
  stat_smooth(method = 'lm',color = "black",size=1)+
  #geom_abline(slope=1,intercept = 0,size=1)+
  #scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab("Responsibility rating")+
  ylab("Social information sensitivity betaother")+
  pic_theme
Fig5E
 

#calculate correlation, individual performance in groups
r_per_res1 <- cor.test(parameter_study1$score,parameter_study1$responsibility1)

r_per_Betaothers <- cor.test(parameter_study1$score,parameter_study1$BetaOthers)


# parameters density figure,  violin, four parameters [-1,1]
condition <- c(rep(1,155),rep(2,155),rep(3,155),rep(4,155))
parameters <- c(parameter_study1$Sensitivity,parameter_study1$Decay,parameter_study1$LearningRate,parameter_study1$BetaOthers)
data <- cbind(parameters,condition) 

data <- as.data.frame(data)
data$condition <- as.factor(data$condition) 

mean_para <- aggregate(data$parameters,by=list(data$condition),mean)
sd_para <- aggregate(data$parameters,by=list(data$condition),sd)


P1<- ggplot(data, aes(x = condition, y = parameters, fill=condition)) + 
  #rotate_x_text(angle = 45) +
  #scale_x_discrete(labels = c("1" = "Sensitivity","2" = "Decay","3" = "LearningRate","4" = "ExplorBonus","5" = "BetaOthers")) + #改x轴label名
  geom_violin(trim=FALSE,color="white") + 
  geom_boxplot(width=0.05,position=position_dodge(0.9))+
  geom_point(data=data,aes(x=condition,y=parameters,group=condition),color="grey",position = position_jitterdodge(dodge.width = 0.8,jitter.width = 1),alpha=0.9,size=6)+
  #scale_y_continuous(limits = c(-2,10), breaks = seq(-2,10,2))+
  scale_y_continuous(limits = c(-0.5,1.5))+
  pic_theme+ 
  ylab("Value")+xlab("Parameter name") #设置x轴和y轴的标题
P1

na
# one parameter, ExplorBonus, to shrink the size of figure, use two violins
condition2 <- c(rep(5,155),rep(6,155))
parameters2 <- c(parameter_study1$ExplorBonus,parameter_study1$ExplorBonus)
data2 <- cbind(parameters2,condition2) 

data2 <- as.data.frame(data2)
data2$condition2 <- as.factor(data2$condition2)

mean(data2$parameters2)
sd(data2$parameters2)

P2<- ggplot(data2, aes(x = condition2, y = parameters2, fill=condition2)) + 
  #rotate_x_text(angle = 45) +
  #scale_x_discrete(labels = c("1" = "Sensitivity","2" = "Decay","3" = "LearningRate","4" = "ExplorBonus","5" = "BetaOthers")) + #改x轴label名
  geom_violin(trim=FALSE,color="white") + 
  geom_boxplot(width=0.05,position=position_dodge(0.9))+
  geom_point(data=data2,aes(x=condition2,y=parameters2),color="grey",position = position_jitterdodge(dodge.width = 0.8,jitter.width = 1),alpha=0.9,size=6)+
  #scale_y_continuous(limits = c(-2,10), breaks = seq(-2,10,2))+
  pic_theme+ 
  ylab("Value")+xlab("Parameter name") #设置x轴和y轴的标题
P2




###################
####  study 2  ####
###################
parameter_study2 <- subset(parameter_S,discuss_list == 1)
#calculate correlation, identity1
r5_Sensitivity <- cor(parameter_study2$Sensitivity,parameter_study2$identity1,method = 'pearson')
rtest5_Sensitivity <- cor.test(parameter_study2$Sensitivity,parameter_study2$identity1)
rtest5_Decay <- cor.test(parameter_study2$Decay,parameter_study2$identity1)
rtest5_LearningRate <- cor.test(parameter_study2$LearningRate,parameter_study2$identity1)
rtest5_ExplorBonus <- cor.test(parameter_study2$ExplorBonus,parameter_study2$identity1)
rtest5_BetaOthers <- cor.test(parameter_study2$BetaOthers,parameter_study2$identity1)

#calculate correlation, identity2
r6_Sensitivity <- cor(parameter_study2$Sensitivity,parameter_study2$identity2,method = 'pearson')
rtest6_Sensitivity <- cor.test(parameter_study2$Sensitivity,parameter_study2$identity2)
rtest6_Decay <- cor.test(parameter_study2$Decay,parameter_study2$identity2)
rtest6_LearningRate <- cor.test(parameter_study2$LearningRate,parameter_study2$identity2)
rtest6_ExplorBonus <- cor.test(parameter_study2$ExplorBonus,parameter_study2$identity2)
rtest6_BetaOthers <- cor.test(parameter_study2$BetaOthers,parameter_study2$identity2)


#calculate correlation,responsibility1
rtest7_Sensitivity <- cor.test(parameter_study2$Sensitivity,parameter_study2$responsibility1)
rtest7_Decay <- cor.test(parameter_study2$Decay,parameter_study2$responsibility1)
rtest7_LearningRate <- cor.test(parameter_study2$LearningRate,parameter_study2$responsibility1)
rtest7_ExplorBonus <- cor.test(parameter_study2$ExplorBonus,parameter_study2$responsibility1)
rtest7_BetaOthers <- cor.test(parameter_study2$BetaOthers,parameter_study2$responsibility1)  ##significant, 0.208


#calculate correlation, responsibility2
rtest8_Sensitivity <- cor.test(parameter_study2$Sensitivity,parameter_study2$responsibility2)
rtest8_Decay <- cor.test(parameter_study2$Decay,parameter_study2$responsibility2)
rtest8_LearningRate <- cor.test(parameter_study2$LearningRate,parameter_study2$responsibility2)
rtest8_ExplorBonus <- cor.test(parameter_study2$ExplorBonus,parameter_study2$responsibility2)
rtest8_BetaOthers <- cor.test(parameter_study2$BetaOthers,parameter_study2$responsibility2) 


Fig5F <-ggplot(data=parameter_study2,aes(x=responsibility1,y=BetaOthers))+
  geom_point(size=14,color= "#13A4E5", alpha=1)+
  stat_smooth(method = 'lm',color = "black",size=1)+
  #geom_abline(slope=1,intercept = 0,size=1)+
  #scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab("Responsibility rating")+
  ylab("Social information sensitivity betaother")+
  pic_theme
Fig5F


#calculate correlation, individual performance in groups
r_per_res1_study2 <- cor.test(parameter_study2$score,parameter_study2$responsibility1)
r_per_Betaothers_study2 <- cor.test(parameter_study2$score,parameter_study2$BetaOthers)
#mediation analysis
library(bruceR)
parameter_study2 <- data.frame(parameter_study2)
SEM_study2 <- data.frame(cbind(scale(parameter_study2$BetaOthers,center = TRUE),scale(parameter_study2$responsibility1,center = TRUE),scale(parameter_study2$score),center = TRUE))
names(SEM_study2)<-c("BetaOthers","responsibility1","score")
PROCESS(parameter_study2, y="score", x="BetaOthers",
        meds="responsibility1", 
        ci="boot", nsim=5000, seed=3)


# parameters density figure,  violin, four parameters [-1,1]
condition3 <- c(rep(1,140),rep(2,140),rep(3,140),rep(4,140))
parameters3 <- c(parameter_study2$Sensitivity,parameter_study2$Decay,parameter_study2$LearningRate,parameter_study2$BetaOthers)
data3 <- cbind(parameters3,condition3) 

data3 <- as.data.frame(data3)
data3$condition3 <- as.factor(data3$condition3) 

mean_para <- aggregate(data3$parameters3,by=list(data3$condition3),mean)
sd_para <- aggregate(data3$parameters3,by=list(data3$condition3),sd)


P3<- ggplot(data3, aes(x = condition3, y = parameters3, fill=condition3)) + 
  #rotate_x_text(angle = 45) +
  #scale_x_discrete(labels = c("1" = "Sensitivity","2" = "Decay","3" = "LearningRate","4" = "ExplorBonus","5" = "BetaOthers")) + #改x轴label名
  geom_violin(trim=FALSE,color="white") + 
  geom_boxplot(width=0.05,position=position_dodge(0.9))+
  geom_point(data=data3,aes(x=condition3,y=parameters3,group=condition3),color="grey",position = position_jitterdodge(dodge.width = 0.8,jitter.width = 1),alpha=0.9,size=6)+
  #scale_y_continuous(limits = c(-2,10), breaks = seq(-2,10,2))+
  scale_y_continuous(limits = c(-0.5,1.5))+
  pic_theme+ 
  ylab("Value")+xlab("Parameter name") #设置x轴和y轴的标题
P3

na
# one parameter, ExplorBonus, to shrink the size of figure, use two violins
condition4 <- c(rep(5,140),rep(6,140))
parameters4 <- c(parameter_study2$ExplorBonus,parameter_study2$ExplorBonus)
data4 <- cbind(parameters4,condition4) 

data4 <- as.data.frame(data4)
data4$condition4 <- as.factor(data4$condition4)

mean(data4$parameters4)
sd(data4$parameters4)

P4<- ggplot(data4, aes(x = condition4, y = parameters4, fill=condition4)) + 
  #rotate_x_text(angle = 45) +
  #scale_x_discrete(labels = c("1" = "Sensitivity","2" = "Decay","3" = "LearningRate","4" = "ExplorBonus","5" = "BetaOthers")) + #改x轴label名
  geom_violin(trim=FALSE,color="white") + 
  geom_boxplot(width=0.05,position=position_dodge(0.9))+
  geom_point(data=data4,aes(x=condition4,y=parameters4),color="grey",position = position_jitterdodge(dodge.width = 0.8,jitter.width = 1),alpha=0.9,size=6)+
  #scale_y_continuous(limits = c(-2,10), breaks = seq(-2,10,2))+
  pic_theme+ 
  ylab("Value")+xlab("Parameter name") #设置x轴和y轴的标题
P4


####
## parameters comparison between communication and no communication
var.test(subset(parameter_S,discuss_list==0 & leader_power_list==0)$Sensitivity,subset(parameter_S,discuss_list==1 & leader_power_list==0)$Sensitivity)
t.test(Sensitivity~discuss_list,var.equal = T, data=parameter_S)
t_to_d(-0.167, 293) #non significant, 0.868

summary_sensitivity<-data.frame(aggregate(Sensitivity~discuss_list,FUN='mean',data=parameter_S),(aggregate(Sensitivity~discuss_list,FUN='sd',data=parameter_S))[2])
n<-c(155,140)
colnames(summary_sensitivity)[c(2,3)]=c('mean','se')
summary_sensitivity$se<-summary_sensitivity$se/sqrt(n)


###
var.test(subset(parameter_S,discuss_list==0 & leader_power_list==0)$Decay,subset(parameter_S,discuss_list==1 & leader_power_list==0)$Decay)
t.test(Decay~discuss_list,var.equal = T, data=parameter_S)
t_to_d(1.943, 293) #marginal significant，0.053

summary_Decay<-data.frame(aggregate(Decay~discuss_list,FUN='mean',data=parameter_S),(aggregate(Decay~discuss_list,FUN='sd',data=parameter_S))[2])
n<-c(155,140)
colnames(summary_Decay)[c(2,3)]=c('mean','se')
summary_Decay$se<-summary_Decay$se/sqrt(n)

#####
var.test(subset(parameter_S,discuss_list==0 & leader_power_list==0)$LearningRate,subset(parameter_S,discuss_list==1 & leader_power_list==0)$LearningRate)
t.test(LearningRate~discuss_list,var.equal = T, data=parameter_S)
t_to_d(-1.681, 293)  #non significant,0.094

summary_LearningRate<-data.frame(aggregate(LearningRate~discuss_list,FUN='mean',data=parameter_S),(aggregate(LearningRate~discuss_list,FUN='sd',data=parameter_S))[2])
n<-c(155,140)
colnames(summary_LearningRate)[c(2,3)]=c('mean','se')
summary_LearningRate$se<-summary_LearningRate$se/sqrt(n)

#####
var.test(subset(parameter_S,discuss_list==0 & leader_power_list==0)$ExplorBonus,subset(parameter_S,discuss_list==1 & leader_power_list==0)$ExplorBonus)
t.test(ExplorBonus~discuss_list,var.equal = T, data=parameter_S)
t_to_d(2.152, 293) #significant,0.032

summary_ExplorBonus<-data.frame(aggregate(ExplorBonus~discuss_list,FUN='mean',data=parameter_S),(aggregate(ExplorBonus~discuss_list,FUN='sd',data=parameter_S))[2])
n<-c(155,140)
colnames(summary_ExplorBonus)[c(2,3)]=c('mean','se')
summary_ExplorBonus$se<-summary_ExplorBonus$se/sqrt(n)

######
var.test(subset(parameter_S,discuss_list==0 & leader_power_list==0)$BetaOthers,subset(parameter_S,discuss_list==1 & leader_power_list==0)$BetaOthers)
t.test(BetaOthers~discuss_list,var.equal = T, data=parameter_S)
t_to_d(-1.995, 293) #significant,0.047

summary_BetaOthers<-data.frame(aggregate(BetaOthers~discuss_list,FUN='mean',data=parameter_S),(aggregate(BetaOthers~discuss_list,FUN='sd',data=parameter_S))[2])
n<-c(155,140)
colnames(summary_BetaOthers)[c(2,3)]=c('mean','se')
summary_BetaOthers$se<-summary_BetaOthers$se/sqrt(n)


###########
### Figure 5
condition4 <- c(rep(5,140),rep(6,140))
parameters4 <- c(parameter_study2$ExplorBonus,parameter_study2$ExplorBonus)
data4 <- cbind(parameters4,condition4) 

##load parameter_S first
setwd("E:/CCNU_work/Projects/IGT_paper1/Data")
parameter_S= read.table("./parameter_study1and2.txt",header=T)

parameter_S <- as.data.frame(parameter_S)
parameter_S$discuss_list <- as.factor(parameter_S$discuss_list)

mean(data4$parameters4)
sd(data4$parameters4)

Fig5B <- ggplot(parameter_S, aes(x = discuss_list, y = score, fill=discuss_list)) + 
  #rotate_x_text(angle = 45) +
  #scale_x_discrete(labels = c("1" = "Sensitivity","2" = "Decay","3" = "LearningRate","4" = "ExplorBonus","5" = "BetaOthers")) + #改x轴label名
  geom_violin(trim=FALSE,color="white") + 
  geom_boxplot(width=0.2,position=position_dodge(0.9))+
  geom_point(data=parameter_S,aes(x=discuss_list,y=score),color="grey",position = position_jitterdodge(dodge.width = 0.8,jitter.width = 1),alpha=0.9,size=6)+
  scale_fill_manual(values=c('#6699cc','#fc8d59'),labels=c("NoCom","Com"))+
  #scale_y_continuous(limits = c(-2,10), breaks = seq(-2,10,2))+
  pic_theme+ 
  ylab("Value")+xlab("Parameter name") #设置x轴和y轴的标题
Fig5B


Fig5C <- ggplot(parameter_S, aes(x = discuss_list, y = ExplorBonus, fill=discuss_list)) + 
  #rotate_x_text(angle = 45) +
  #scale_x_discrete(labels = c("1" = "Sensitivity","2" = "Decay","3" = "LearningRate","4" = "ExplorBonus","5" = "BetaOthers")) + #改x轴label名
  geom_violin(trim=FALSE,color="white") + 
  geom_boxplot(width=0.2,position=position_dodge(0.9))+
  geom_point(data=parameter_S,aes(x=discuss_list,y=ExplorBonus),color="grey",position = position_jitterdodge(dodge.width = 0.8,jitter.width = 1),alpha=0.9,size=6)+
  scale_fill_manual(values=c('#6699cc','#fc8d59'),labels=c("NoCom","Com"))+
  #scale_y_continuous(limits = c(-2,10), breaks = seq(-2,10,2))+
  pic_theme+ 
  ylab("Value")+xlab("Parameter name") #设置x轴和y轴的标题
Fig5C

Fig5D <- ggplot(parameter_S, aes(x = discuss_list, y = BetaOthers, fill=discuss_list)) + 
  #rotate_x_text(angle = 45) +
  #scale_x_discrete(labels = c("1" = "Sensitivity","2" = "Decay","3" = "LearningRate","4" = "ExplorBonus","5" = "BetaOthers")) + #改x轴label名
  geom_violin(trim=FALSE,color="white") + 
  geom_boxplot(width=0.2,position=position_dodge(0.9))+
  geom_point(data=parameter_S,aes(x=discuss_list,y=BetaOthers),color="grey",position = position_jitterdodge(dodge.width = 0.8,jitter.width = 1),alpha=0.9,size=6)+
  scale_fill_manual(values=c('#6699cc','#fc8d59'),labels=c("NoCom","Com"))+
  #scale_y_continuous(limits = c(-2,10), breaks = seq(-2,10,2))+
  pic_theme+ 
  ylab("Value")+xlab("Parameter name") #设置x轴和y轴的标题
Fig5D
