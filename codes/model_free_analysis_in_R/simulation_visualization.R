#visualization on model predictions
library(ggplot2)
library(R.matlab)
library(reshape2)
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

#calculate mean and se
#function preparation
group_majority<-function(choice){
 n=as.numeric(table(choice))
 g_decision=which(n==max(n))
 if(length(g_decision)>1){
   g_decision=sample(g_decision,1)
 }
 return(g_decision)
}

visualize_simulation<-function(simulated_choice_probability,actual_choice_probability){
  
  mean_simulated_choice_probability<-apply(simulated_choice_probability,MARGIN = c(2,3),mean)
  se_simulated_choice_probability<-apply(simulated_choice_probability,MARGIN = c(2,3),sd)/sqrt(dim(simulated_choice_probability)[1])
  
  mean_actual_choice_probability<-apply(actual_choice_probability,MARGIN = c(2,3),mean)
  se_actual_choice_probability<-apply(actual_choice_probability,MARGIN = c(2,3),sd)/sqrt(dim(actual_choice_probability)[1])
  
  ##data reshape
  mean_simulated_choice_probability<-melt(mean_simulated_choice_probability)
  colnames(mean_simulated_choice_probability)<-c('Deck','Trial','Probability')
  mean_simulated_choice_probability$Deck<-as.factor(mean_simulated_choice_probability$Deck)
  
  se_simulated_choice_probability<-melt(se_simulated_choice_probability)
  colnames(se_simulated_choice_probability)<-c('Deck','Trial','Probability')
  se_simulated_choice_probability$Deck<-as.factor(se_simulated_choice_probability$Deck)
  
  mean_actual_choice_probability<-melt(mean_actual_choice_probability)
  colnames(mean_actual_choice_probability)<-c('Deck','Trial','Probability')
  mean_actual_choice_probability$Deck<-as.factor(mean_actual_choice_probability$Deck)
  
  se_actual_choice_probability<-melt(se_actual_choice_probability)
  colnames(se_actual_choice_probability)<-c('Deck','Trial','Probability')
  se_actual_choice_probability$Deck<-as.factor(se_actual_choice_probability$Deck)
  ##visualiization
  #simulated
  p1<-ggplot()+
    geom_smooth(data=mean_simulated_choice_probability,aes(x=Trial,y=Probability,color=Deck,fill=Deck),alpha=0.3)+
    scale_y_continuous(limits=c(0,0.7),breaks=seq(0,0.8,0.2))+
    pic_theme
  
  p2<-ggplot()+
    geom_smooth(data=mean_actual_choice_probability,aes(x=Trial,y=Probability,color=Deck,fill=Deck),alpha=0.3)+
    scale_y_continuous(limits=c(0,0.7),breaks=seq(0,0.8,0.2))+
    pic_theme
  
  pic_list<-list(p1,p2)
  return(pic_list)
}

extract_simulated_data<-function(tmp_leader,tmp_discuss,type){
  if (type=='s_decision'){
    tmp_id_list<-subset(parameter_S,leader_power_list==tmp_leader & discuss_list==tmp_discuss)$id_list
    simulated_choice_probability<-array(data = rep(0,length(tmp_id_list)*4*100),dim=c(length(tmp_id_list),4,100))
   for (i in 1:length(tmp_id_list)){
    tmp_data=subset(raw_data,id==tmp_id_list[i])
    tmp_choice=tmp_data$s_decision
    for (t in 1:100){
      simulated_choice_probability[i,tmp_choice[t],t]=1
    }
   }
  }else if(type=='g_decision'){
    group_list<-c()
       for(g in 1:dim(group_tag)[1]){
         if ((group_class(group_tag[g,1])[1]==tmp_discuss & group_class(group_tag[g,1])[2]==tmp_leader)){
           group_list<-c(group_list,g)
         }
       }
      simulated_choice_probability<-array(data = rep(0,length(group_list)*4*100),dim=c(length(group_list),4,100))
      for (g in 1:length(group_list)){
        if(tmp_leader<1){
          tmp_choice_matrix<-matrix(rep(0,5*100),nrow=100,ncol=5)
          for(s in 1:5){
            tmp_id=group_tag[group_list[g],s]
            tmp_data=subset(raw_data,id==tmp_id)$s_decision
            tmp_choice_matrix[,s]=tmp_data
          }
          for(t in 1:100){
            tmp_g_decision=group_majority(tmp_choice_matrix[t,])
            simulated_choice_probability[g,tmp_g_decision,t]=1
           }
          }else{
            tmp_id=group_tag[group_list[g],5]
            tmp_data=subset(raw_data,id==tmp_id)$g_decision
            for (t in 1:100){
              simulated_choice_probability[g,tmp_data[t],t]=1
            }
          }
        }
      }

  return(simulated_choice_probability)
}


extract_actual_data<-function(tmp_leader,tmp_discuss,type){
  if (type=='s_decision'){
     tmp_id_list<-subset(parameter_S,leader_power_list==tmp_leader & discuss_list==tmp_discuss)$id_list
     actual_choice_probability<-array(data = rep(0,length(tmp_id_list)*4*100),dim=c(length(tmp_id_list),4,100))
     for (i in 1:length(tmp_id_list)){
      if(tmp_leader<2){
       tmp_data=no_leader_data[which(no_leader_data[,1]==tmp_id_list[i]),]
       tmp_choice=tmp_data[,11]-10
      }else{
       tmp_data=leader_data[which(leader_data[,1]==tmp_id_list[i]),]
       tmp_choice=tmp_data[,11]-10
      }
       for (t in 1:100){
         actual_choice_probability[i,tmp_choice[t],t]=1
       }
     }
  }else if (type=='g_decision'){
    group_list<-c()
    for(g in 1:dim(group_tag)[1]){
      if ((group_class(group_tag[g,1])[1]==tmp_discuss & group_class(group_tag[g,1])[2]==tmp_leader)){
        group_list<-c(group_list,g)
      }
    }
    actual_choice_probability<-array(data = rep(0,length(group_list)*4*100),dim=c(length(group_list),4,100))
    
    for (g in 1:length(group_list)){
      tmp_id=group_tag[group_list[g],5]
    if(tmp_leader<1){
      tmp_data=no_leader_data[which(no_leader_data[,1]==tmp_id),]
      tmp_choice=tmp_data[,13]-10
    }else{
      tmp_data=leader_data[which(leader_data[,1]==tmp_id),]
      tmp_choice=tmp_data[,16]-10
    }
      for (t in 1:100){
        actual_choice_probability[g,tmp_choice[t],t]=1
      }
    }
  }
  return(actual_choice_probability)
}

##individual learning

#VSE model predictions
simulated_choice_probability<-readMat('F:\\IGT_Data\\R\\Formal_Figures\\Figure2_simulated_choice_probability.mat')
simulated_choice_probability<-simulated_choice_probability[[1]]

#real data observations
individual_id_list<-unique(IGT_data_individual$subNo)
actual_choice_probability<-array(data = rep(0,length(individual_id_list)*4*100),dim=c(length(individual_id_list),4,100))
for (i in 1:length(individual_id_list)){
  tmp_data=subset(IGT_data_individual,subNo==individual_id_list[i])
  tmp_choice=tmp_data$chose_performance-10
  for(t in 1:100){
    actual_choice_probability[i,tmp_choice[t],t]=1
  }
}
#get the visualization
tmp_pic_list<-visualize_simulation(simulated_choice_probability,actual_choice_probability)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]



raw_data <- read.csv("F:\\IGT_Data\\Matlab\\Simulation_Data.csv")


##group learning-individual_decision, this generates choice probability pattern plot 
#control
#model_prediction
control_sim_prob=extract_simulated_data(0,0,'s_decision')
control_act_prob=extract_actual_data(0,0,'s_decision')
tmp_pic_list<-visualize_simulation(control_sim_prob,control_act_prob)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]

#discuss
discuss_sim_prob=extract_simulated_data(0,1,'s_decision')
discuss_act_prob=extract_actual_data(0,1,'s_decision')
tmp_pic_list<-visualize_simulation(discuss_sim_prob,discuss_act_prob)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]

#leaderM
leaderM_sim_prob=extract_simulated_data(1,0,'s_decision')
leaderM_act_prob=extract_actual_data(1,0,'s_decision')
tmp_pic_list<-visualize_simulation(leaderM_sim_prob,leaderM_act_prob)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]

#mixM
mixM_sim_prob=extract_simulated_data(1,1,'s_decision')
mixM_act_prob=extract_actual_data(1,1,'s_decision')
tmp_pic_list<-visualize_simulation(mixM_sim_prob,mixM_act_prob)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]

#leaderL
leaderL_sim_prob=extract_simulated_data(2,0,'s_decision')
leaderL_act_prob=extract_actual_data(2,0,'s_decision')
tmp_pic_list<-visualize_simulation(leaderL_sim_prob,leaderL_act_prob)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]

#mixL
mixL_sim_prob=extract_simulated_data(2,1,'s_decision')
mixL_act_prob=extract_actual_data(2,1,'s_decision')
tmp_pic_list<-visualize_simulation(mixL_sim_prob,mixL_act_prob)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]

#group decisions
#no leader no discuss
group_control_sim_prob=extract_simulated_data(0,0,'g_decision')
group_control_act_prob=extract_actual_data(0,0,'g_decision')
tmp_pic_list<-visualize_simulation(group_control_sim_prob,group_control_act_prob)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]

#no leader with discuss
group_discuss_sim_prob=extract_simulated_data(0,1,'g_decision')
group_discuss_act_prob=extract_actual_data(0,1,'g_decision')
tmp_pic_list<-visualize_simulation(group_discuss_sim_prob,group_discuss_act_prob)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]

#leader without discussion
group_leader_sim_prob=extract_simulated_data(1,0,'g_decision')
group_leader_act_prob=extract_actual_data(1,0,'g_decision')
tmp_pic_list<-visualize_simulation(group_leader_sim_prob,group_leader_act_prob)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]

#leader with discussion
group_mix_sim_prob=extract_simulated_data(1,1,'g_decision')
group_mix_act_prob=extract_actual_data(1,1,'g_decision')
tmp_pic_list<-visualize_simulation(group_mix_sim_prob,group_mix_act_prob)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]




####quick codes to get simulation accuracy
individual_simulation_acc<-c()
for (i in unique(parameter_S$id_list)){
  tmp_data=subset(parameter_S,id_list==i)
  if (tmp_data$leader_power_list!=2){
    tmp_real_data<-no_leader_data[which(no_leader_data[,1]==i),11]-10
    tmp_s_data<-subset(raw_data,id==i)$s_decision
  }else{
    tmp_real_data<-leader_data[which(leader_data[,1]==i),11]-10
    tmp_s_data<-subset(raw_data,id==i)$s_decision
  }
  tmp_acc<-length(which(tmp_s_data==tmp_real_data))/length(tmp_s_data)
  tmp_info<-c(i,tmp_acc,tmp_data$discuss_list,tmp_data$leader_power_list)
  individual_simulation_acc<-rbind(individual_simulation_acc,tmp_info)
}

colnames(individual_simulation_acc)<-c('id','accuracy','discuss_list','leader_power_list')




##calculating an overall simulation accuracy for individual decisions 
individual_simulation_acc<-as.data.frame(individual_simulation_acc)
summary_acc<-data.frame(aggregate(accuracy~discuss_list+leader_power_list,FUN='mean',data=individual_simulation_acc),(aggregate(accuracy~discuss_list+leader_power_list,FUN='sd',data=individual_simulation_acc))[3])
n<-c(155,140,128,32,112,28)
colnames(summary_acc)[c(3,4)]=c('mean','se')
summary_acc$se<-summary_acc$se/sqrt(n)

individual_simulation_acc$discuss_list<-as.factor(individual_simulation_acc$discuss_list)
individual_simulation_acc$leader_power_list<-as.factor(individual_simulation_acc$leader_power_list)
summary_acc$discuss_list<-as.factor(summary_acc$discuss_list)
summary_acc$leader_power_list<-as.factor(summary_acc$leader_power_list)

figure_s7a<-ggplot()+
  geom_col(data=summary_acc,aes(x=discuss_list,y=mean,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.75,width=0.7)+
  geom_point(data=individual_simulation_acc,aes(x=discuss_list,y=accuracy,color=leader_power_list),alpha=0.8,size=4,position=position_jitterdodge(jitter.width=0.05,dodge.width=0.8))+guides(color='none')+
  geom_errorbar(data=summary_acc,aes(x=discuss_list,ymin=mean-se,ymax=mean+se,group=leader_power_list),position=position_dodge(width=0.8),size=1,alpha=1,width=0.06)+
  labs(x='Discussion',y='Accuracy',title='Simulation Accuracy-individual decisions ')+
  scale_color_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_fill_manual(values=c('#91bfdb','#A8D5BA','#fc8d59'),labels=c('No Leader Group','Non-Leading Member','Leader'))+
  scale_x_discrete(breaks=c(0,1),labels=c('No Discussion','Discussion'))+
  scale_y_continuous(expand = c(0,0),limits=c(0,1), breaks=seq(0, 1, 0.2))+
  pic_theme+
  theme(legend.position = c(0.9, 0.97),
        legend.title=element_blank())





####quick codes to get group decision accuracy
##for this simulation data, the group decisions are copied from the original data, so we need to 
#transform the group decision data into a simulation context that is majority rule (note that, the simulated group decision is not used recovery )
group_simulation_acc<-c()
for (i in 1:dim(group_tag)[1]){
  tmp_data=subset(parameter_S,id_list==group_tag[i,5])
  if (tmp_data$leader_power_list==0){
    tmp_sub_data = c()
    for (s in 1:5){
      tmp_sub_choice_data<-no_leader_data[which(no_leader_data[,1]==group_tag[i,s]),11]-10
      tmp_sub_data <- rbind(tmp_sub_data,tmp_sub_choice_data)
    }
    tmp_real_data = rep(-999,100)
    for(t in 1:100){
      tmp_real_data[t] = group_majority(tmp_sub_data[,t])
    }
    tmp_s_data<-subset(raw_data,id==group_tag[i,5])$g_decision
  }else{
    tmp_real_data<-leader_data[which(leader_data[,1]==group_tag[i,5]),16]-10
    tmp_s_data<-subset(raw_data,id==group_tag[i,5])$g_decision
  }
  tmp_acc<-length(which(tmp_s_data==tmp_real_data))/length(tmp_s_data)
  tmp_info<-c(i,tmp_acc,tmp_data$discuss_list,tmp_data$leader_power_list)
  group_simulation_acc<-rbind(group_simulation_acc,tmp_info)
}

colnames(group_simulation_acc)<-c('id','accuracy','discuss_list','leader_power_list')


##calculating an overall simulation accuracy for individual decisions 
group_simulation_acc<-as.data.frame(group_simulation_acc)
summary_acc<-data.frame(aggregate(accuracy~discuss_list+leader_power_list,FUN='mean',data=group_simulation_acc),(aggregate(accuracy~discuss_list+leader_power_list,FUN='sd',data=group_simulation_acc))[3])
n<-c(31,28,31,28)
colnames(summary_acc)[c(3,4)]=c('mean','se')
summary_acc$se<-summary_acc$se/sqrt(n)

group_simulation_acc$discuss_list<-as.factor(group_simulation_acc$discuss_list)
group_simulation_acc$leader_power_list<-as.factor(group_simulation_acc$leader_power_list)
summary_acc$discuss_list<-as.factor(summary_acc$discuss_list)
summary_acc$leader_power_list<-as.factor(summary_acc$leader_power_list)

figure_s7b<-ggplot()+
  geom_col(data=summary_acc,aes(x=discuss_list,y=mean,fill=leader_power_list),position=position_dodge(width=0.8),alpha=0.75,width=0.7)+
  geom_point(data=group_simulation_acc,aes(x=discuss_list,y=accuracy,color=leader_power_list),alpha=0.8,size=4,position=position_jitterdodge(jitter.width=0.05,dodge.width=0.8))+guides(color='none')+
  geom_errorbar(data=summary_acc,aes(x=discuss_list,ymin=mean-se,ymax=mean+se,group=leader_power_list),position=position_dodge(width=0.8),size=1,alpha=1,width=0.06)+
  labs(x='Discussion',y='Accuracy',title='Simulation Accuracy-individual decisions ')+
  scale_color_manual(values=c('#91bfdb','#fc8d59'),labels=c('No Leader Group','Leader Group'))+
  scale_fill_manual(values=c('#91bfdb','#fc8d59'),labels=c('No Leader Group','Leader Group'))+
  scale_x_discrete(breaks=c(0,1),labels=c('No Discussion','Discussion'))+
  scale_y_continuous(expand = c(0,0),limits=c(0,1), breaks=seq(0, 1, 0.2))+
  pic_theme+
  theme(legend.position = c(0.9, 0.97),
        legend.title=element_blank())