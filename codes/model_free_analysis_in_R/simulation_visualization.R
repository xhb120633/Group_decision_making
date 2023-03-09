#visualization on model predictions
library(ggplot2)
library(R.matlab)
library(reshape2)

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
individual_id_list<-unique(individual_data[,1])
actual_choice_probability<-array(data = rep(0,length(individual_id_list)*4*100),dim=c(length(individual_id_list),4,100))
for (i in 1:length(individual_id_list)){
  tmp_data=individual_data[which(individual_data[,1]==individual_id_list[i]),]
  tmp_choice=tmp_data[,11]-10
  for(t in 1:100){
    actual_choice_probability[i,tmp_choice[t],t]=1
  }
}
#get the visualization
tmp_pic_list<-visualize_simulation(simulated_choice_probability,actual_choice_probability)
tmp_p1<-tmp_pic_list[[1]]
tmp_p2<-tmp_pic_list[[2]]


##group learning-individual_decision
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