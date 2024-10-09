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

cal_entropy<-function(p){
  p <- p/sum(p)
  tmp_p <- p[p>0]
  entropy <- -1*sum(tmp_p*log2(tmp_p))
  return(entropy)
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


#substract group_represnetative_data 
H_list = c()
id_list = c()
for(i in group_standard_score$sub_id){
  tmp_data = no_leader_data[no_leader_data[,1]==i,]
  for (t in 1:100){
    tmp_N = as.numeric(tmp_data[t,14:17])
    tmp_H = cal_entropy(tmp_N)
    H_list = c(H_list,tmp_H)
    id_list = c(id_list,i)
  }
}

group_entropy = data.frame(id_list,H_list)
group_entropy = aggregate(data = group_entropy, .~id_list,FUN = mean)
group_standard_score<-data.frame(group_standard_score,group_entropy$H_list)

colnames(group_standard_score)[5] = 'group_entropy'

#substract for no_leader_no_discussion group

formal_data<- subset(group_standard_score,leader ==0 & discuss == 1)

ggplot(data=formal_data,aes(x=group_entropy, y = score))+
         geom_point()+
         geom_smooth(method = 'lm')+
         pic_theme

cor.test(formal_data$group_entropy,formal_data$score)

##compare with discussion?
formal_data<- subset(group_standard_score,leader ==0)

ggplot(data=formal_data,aes(x=discuss, y = group_entropy, fill = discuss))+
  geom_point()+
  geom_boxplot()+
  pic_theme

p1 <- ggplot(formal_data, aes(x = discuss, y = group_entropy, fill=discuss)) + 
  #rotate_x_text(angle = 45) +
  #scale_x_discrete(labels = c("1" = "Sensitivity","2" = "Decay","3" = "LearningRate","4" = "ExplorBonus","5" = "BetaOthers")) + #改x轴label名
  geom_violin(trim=FALSE,color="white") + 
  geom_boxplot(width=0.2,position=position_dodge(0.9))+
  geom_point(data=formal_data,aes(x=discuss,y=group_entropy),color="grey",position = position_jitterdodge(dodge.width = 0.8,jitter.width = 1),alpha=0.9,size=6)+
  scale_fill_manual(values=c('#6699cc','#fc8d59'),labels=c("NoCom","Com"))+
  #scale_y_continuous(limits = c(-2,10), breaks = seq(-2,10,2))+
  pic_theme+ 
  ylab("Value")+xlab("Parameter name") #设置x轴和y轴的标题
p1

group_entropy_no_discuss <- subset(formal_data, discuss==0)$group_entropy
group_entropy_discuss <- subset(formal_data, discuss==1)$group_entropy
t.test(group_entropy_no_discuss,group_entropy_discuss)

ggplot(data=group_standard_score,aes(x=group_entropy, y = score))+
  geom_point(aes(color = discuss))+guides(color = 'none')+
  geom_smooth(aes(color = discuss))+
  pic_theme

#how can group entropy be explained by average beta other of the group?
#add group mean beta other parameters in the dataframe!
mean_beta_list=c()
for (tmp_id in group_standard_score$sub_id){
  tmp_id_list = group_tag[(group_tag[,1]==tmp_id),]
  tmp_beta_list =c()
  for (i in 1:5){
    tmp_sub_id = tmp_id_list[i]
    tmp_parameter = subset(parameter_S, id_list == tmp_sub_id)
    tmp_beta_list = c(tmp_beta_list, tmp_parameter$BetaOthers)
  }
  mean_beta_list<-c(mean_beta_list,mean(tmp_beta_list))
}

group_standard_score<-data.frame(group_standard_score,mean_beta_list)
tmp_data <- subset(group_standard_score,leader ==0 & discuss ==1)

ggplot(data=tmp_data ,aes(x=mean_beta_list, y = group_entropy))+
  geom_point()+
  geom_smooth(method = 'lm')+
  pic_theme


cor.test(tmp_data$mean_beta_list,tmp_data$group_entropy)


#conduct a boostrapping-based correlation
formal_data <- subset(group_standard_score, leader == 0)
cor_list<-c()
n_sample = 4000
for (i in 1:n_sample){
  entropy_list <- formal_data$group_entropy
  beta_list <-c()
  for (g in 1:59){
    tmp_id = sample(group_tag[g,],1)
    tmp_beta = subset(parameter_S, id_list ==tmp_id)$BetaOthers
    beta_list <-c (beta_list, tmp_beta)
  }
  cor_list<-c(cor_list, cor.test(entropy_list,beta_list)$estimate)
}

# 1. Create a Histogram
p1<-ggplot(data.frame(cor_list), aes(x=cor_list)) +
  geom_histogram(binwidth = .01, fill="blue", color="black") + # Adjust binwidth as needed
  labs(title="Histogram of Correlation Estimates", x="Correlation", y="Frequency")

# 2. Compute the 95% Confidence Interval
ci_lower <- quantile(cor_list, probs = 0.025)
ci_upper <- quantile(cor_list, probs = 0.975)

# 3. Add Confidence Interval to the Plot
p1 + geom_vline(xintercept = ci_lower, color="red", linetype="dashed") +
  geom_vline(xintercept = ci_upper, color="red", linetype="dashed") +
  annotate("text", x = ci_lower, y = Inf, label = paste("Lower CI:", round(ci_lower, 2)), vjust = 1.5, color = "red") +
  annotate("text", x = ci_upper, y = Inf, label = paste("Upper CI:", round(ci_upper, 2)), vjust = 2, color = "red")+
  pic_theme


###Try on RSA
##Q1. if combination of parameters can correlate with individual performance
## construct RDM of individual parameters
library(psych)
min_max_normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

formal_parameter <- subset(parameter_S, leader == 0 & discuss == 0)[,1:6]
formal_parameter <- as.data.frame(lapply(formal_parameter, min_max_normalize))
parameter_RDM <- dist(formal_parameter)
      