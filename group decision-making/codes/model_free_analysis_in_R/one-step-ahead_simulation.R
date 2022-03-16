#conduct one step ahead simulation
library(ggplot2)
library(R.matlab)
no_leader_data<-readMat('D:\\IGT_Data\\Matlab\\no_leader_data.mat')
no_leader_data<-as.data.frame(no_leader_data)
no_leader_group_tag<-rbind(control_group_tag,discuss_group_tag)
group_tag=rbind(no_leader_group_tag,leader_tag)
raw_data=c()
for (g in 1:dim(group_tag)[1]){
  #  parameter_list=subset(parameter_s,id_list==group_tag[g,1]|id_list==group_tag[g,2]|id_list==group_tag[g,3]|id_list==group_tag[g,4]|id_list==group_tag[g,5])
  parameter_list=subset(parameter_S,id_list==group_tag[g,1]|id_list==group_tag[g,2]|id_list==group_tag[g,3]|id_list==group_tag[g,4]|id_list==group_tag[g,5])
  #parameter_list=subset(recovered_parameter_s,id_list==group_tag[g,1]|id_list==group_tag[g,2]|id_list==group_tag[g,3]|id_list==group_tag[g,4]|id_list==group_tag[g,5])
  fill_data=subset(no_leader_data,id_list==group_tag[g,1]|id_list==group_tag[g,2]|id_list==group_tag[g,3]|id_list==group_tag[g,4]|id_list==group_tag[g,5])
  s_decision_list=matrix(rep(0,500),ncol=5,nrow=100)
  g_decision_list=matrix(rep(0,800),ncol=8,nrow=100)
  if (group_tag[g,1]<3000){
  for (i in (1:5)){
  theta=parameter_list[i,1]
  decay=parameter_list[i,2]
  lr   =parameter_list[i,3]
  sigma=parameter_list[i,4]
  beta =parameter_list[i,5]
  cons =3**parameter_list[i,6]-1
  ev=rep(0,4)
  ep=rep(1,4)*sigma
  p_choice=c(0.25,0.25,0.25,0.25)
  n<-c(0,0,0,0)
  deck_n<-rep(0,4)
  outcome=2000
  for (t in (1:100)){
      if (t>1){
        v=gain**theta-abs(loss)**theta+n[g_decision]**beta
        for (d in (1:4)){
          if (d==g_decision){
            if (g_decision==s_decision_s){
              x=0
            }else{
              x=1
            }
            ev[d]=ev[d]*decay+(beta**x)*v
            ep[d]=0
          }else{
            if (s_decision_s==d){
              x=0
            }else{
              x=1
            }
            ev[d]=ev[d]*decay
            ep[d]=ep[d]+(beta**x)*lr*((n[d]**beta)+sigma-ep[d])
          }
        }
      }
    x_list=c(0,0,0,0)
    for (d in (1:4)){
      p_choice[d]=(exp(ev[d]+ep[d]+n[d]**beta)*cons)/sum(exp((ev+ep+n**beta)*cons))
      if(is.nan(p_choice[d])){
        p_choice[d]=1
      }
    }
      if (t==1){
        id=group_tag[g,i]
        tmp_data<-subset(no_leader_data,no_leader_data[,1]==id)
        s_decision=tmp_data[1,11]-10
      }else{
        #s_decision=sample(c(1:4), size = 1, replace = T, prob = p_choice)
        s_decision=which(p_choice==max(p_choice))
        if(length(s_decision>1)){
          s_decision=s_decision[sample(1:length(s_decision),1)]
        }
      }
      n=rep(0,4)
      for (s in(1:5)){
        if (s!=i){
          o_id=group_tag[g,s]
          tmp_data<-subset(no_leader_data,no_leader_data[,1]==o_id)
          o_choice=tmp_data[t,11]-10
          n[o_choice]=n[o_choice]+1
        }else{
          o_id=group_tag[g,s]
          tmp_data<-subset(no_leader_data,no_leader_data[,1]==o_id)
          s_decision_s=tmp_data[t,11]-10
        }
      }
      n[s_decision_s]=n[s_decision_s]+1
      s_decision_list[t,i]=s_decision
      #g_decision=which(n==max(n))
      tmp_data<-subset(no_leader_data,no_leader_data[,1]==id)
      g_decision=tmp_data[t,13]-10
    #if (length(g_decision)>1){
    #  g_decision=g_decision[sample(1:length(g_decision),1)]
    #}
    deck_n[g_decision]=deck_n[g_decision]+1
    #gain <- gain_list[g_decision,deck_n[g_decision]]
    #loss <- loss_list[g_decision,deck_n[g_decision]]
    gain <- tmp_data[t,7]/100
    loss <- tmp_data[t,8]/100
    outcome<-outcome+(gain+loss)*100
    g_decision_list[t,]=c(g_decision,gain*100,loss*100,outcome,n)
  }
}
  for(i in 1:5){
    s_data=data.frame(rep(group_tag[g,i],100),c(1:100),s_decision_list[,i],g_decision_list)
    colnames(s_data)=c('id','TrialNum','s_decision','g_decision','gain','loss','outcome','n1','n2','n3','n4')
    raw_data=rbind(raw_data,s_data)
  }
    
  }else{
    for (i in (1:5)){   
      if (i!=5){      
      theta=parameter_list[i,1]
      decay=parameter_list[i,2]
      lr   =parameter_list[i,3]
      sigma=parameter_list[i,4]
      beta =parameter_list[i,5]
      cons =3**parameter_list[i,6]-1
      ev=rep(0,4)
      ep=rep(1,4)*sigma
      p_choice=c(0.25,0.25,0.25,0.25)
      n<-c(0,0,0,0)
      deck_n<-rep(0,4)
      outcome=2000
        for (t in (1:100)){
            n=c(0,0,0,0)
              if (t>1){
                  v=gain**theta-abs(loss)**theta+n[g_decision]**beta
                  for (d in (1:4)){
                    if(g_decision==s_decision_s){
                      x=0
                    }else{
                      x=1
                    }
                    if (d==g_decision){
                      ev[d]=ev[d]*decay+(beta**x)*v
                      ep[d]=0
                    }else{
                      if (d==s_decision_s){
                        x=0
                      }else{
                        x=1
                      }
                      ev[d]=ev[d]*decay
                      ep[d]=ep[d]+(beta**x)*lr*(n[d]**beta+sigma-ep[d])
                    }
                  }
              }
            x_list=c(0,0,0,0)
            for (d in (1:4)){
              p_choice[d]=(exp(ev[d]+ep[d]+n[d]**beta)*cons)/sum(exp((ev+ep+n**beta)*cons))
              if(is.nan(p_choice[d])){
                p_choice[d]=1
              }
            }
            if (t==1){
              id=group_tag[g,i]
              tmp_data<-subset(no_leader_data,no_leader_data[,1]==id)
              s_decision=tmp_data[1,11]-10
            }else{
              #s_decision=sample(c(1:4), size = 1, replace = T, prob = p_choice)
              s_decision=which(p_choice==max(p_choice))
              if(length(s_decision>1)){
                s_decision=s_decision[sample(1:length(s_decision),1)]
              }
            }
            n=rep(0,4)
            for (s in(1:5)){
              if (s!=i){
                if(s!=5){
                o_id=group_tag[g,s]
                tmp_data<-subset(no_leader_data,no_leader_data[,1]==o_id)
                o_choice=tmp_data[t,11]-10
                n[o_choice]=n[o_choice]+1
                }else{
                  o_id=group_tag[g,s]
                  tmp_data<-subset(leader_data,leader_data[,1]==o_id)
                  o_choice=tmp_data[t,11]-10
                  n[o_choice]=n[o_choice]+1
                }
              }else{
                o_id=group_tag[g,s]
                tmp_data<-subset(no_leader_data,no_leader_data[,1]==o_id)
                s_decision_s=tmp_data[t,11]-10
              }
            }
            n[s_decision_s]=n[s_decision_s]+1
            s_decision_list[t,i]=s_decision
            
            tmp_data<-subset(no_leader_data,no_leader_data[,1]==id)
            g_decision=tmp_data[t,13]-10

            deck_n[g_decision]=deck_n[g_decision]+1
            #gain <- gain_list[g_decision,deck_n[g_decision]]
            #loss <- loss_list[g_decision,deck_n[g_decision]]
            gain <- tmp_data[t,7]/100
            loss <- tmp_data[t,8]/100
            outcome<-outcome+(gain+loss)*100
            g_decision_list[t,]=c(g_decision,gain*100,loss*100,outcome,n)
        }
            
      }else{
        id=group_tag[g,i]
        tmp_parameter_list<-subset(parameter_as,id_list==id)
        theta=as.numeric(tmp_parameter_list[1])
        decay=as.numeric(tmp_parameter_list[2])
        lr   =as.numeric(tmp_parameter_list[3])
        sigma=as.numeric(tmp_parameter_list[4])
        beta =as.numeric(tmp_parameter_list[5])
        cons =3**as.numeric(tmp_parameter_list[6])-1
        ev=rep(0,4)
        ep=rep(1,4)*sigma
        p_choice=c(0.25,0.25,0.25,0.25)
        n<-c(0,0,0,0)
        deck_n<-rep(0,4)
        outcome=2000
        for (t in (1:100)){
          n=c(0,0,0,0)
          if (t>1){
            v=gain**theta-abs(loss)**theta+n[g_decision]**beta
            for (d in (1:4)){
              if (d==g_decision){
                ev[d]=ev[d]*decay+v
                ep[d]=0
              }else{
                ev[d]=ev[d]*decay
                ep[d]=ep[d]
              }
            }
          }
          for (d in (1:4)){
            p_choice[d]=(exp((ev[d]+ep[d])*cons))/sum(exp((ev+ep)*cons))
            if(is.nan(p_choice[d])){
              p_choice[d]=1
            }
          }
          if (t==1){
            id=group_tag[g,i]
            tmp_data<-subset(leader_data,leader_data[,1]==id)
            s_decision=tmp_data[1,11]-10
            s_decision_s=s_decision
          }else{
            #s_decision=sample(c(1:4), size = 1, replace = T, prob = p_choice)
            s_decision=which(p_choice==max(p_choice))
            if(length(s_decision>1)){
              s_decision=s_decision[sample(1:length(s_decision),1)]
            }
          }
          n=rep(0,4)
          for (s in(1:5)){
            if (s!=i){
              o_id=group_tag[g,s]
              tmp_data<-subset(no_leader_data,no_leader_data[,1]==o_id)
              o_choice=tmp_data[t,11]-10
              n[o_choice]=n[o_choice]+1
            }else{
              o_id=group_tag[g,s]
              tmp_data<-subset(leader_data,leader_data[,1]==o_id)
              s_decision_s=tmp_data[t,11]-10
            }
          }
          n[s_decision_s]=n[s_decision_s]+1
          s_decision_list[t,i]=s_decision          
          tmp_data<-subset(leader_data,leader_data[,1]==id)
          g_decision_s=tmp_data[t,16]-10
          for(d in 1:4){
            if(d==s_decision_s){
              x=0
            }else{
              x=1
            }
            ep[d]=ep[d]+(beta**x)*lr*(sigma+n[d]**beta-ep[d])
            ev[d]=ev[d]+n[d]**beta
          }
          x_list=c(0,0,0,0)
          for (d in (1:4)){
            p_choice[d]=(exp(ev[d]+ep[d]+n[d]**beta)*cons)/sum(exp((ev+ep+n**beta)*cons))
            if(is.nan(p_choice[d])){
              p_choice[d]=1
            }
          }
          g_decision=which(p_choice==max(p_choice))
          if (length(g_decision)>1){
            g_decision=g_decision[sample(1:length(g_decision),1)]
          }
          


          deck_n[g_decision]=deck_n[g_decision]+1
          #gain <- gain_list[g_decision,deck_n[g_decision]]
          #loss <- loss_list[g_decision,deck_n[g_decision]]
          gain <- tmp_data[t,7]/100
          loss <- tmp_data[t,8]/100
          outcome<-outcome+(gain+loss)*100
          g_decision_list[t,]=c(g_decision,gain*100,loss*100,outcome,n)
        }
      }

      }
   
  for(i in 1:5){
    s_data=data.frame(rep(group_tag[g,i],100),c(1:100),s_decision_list[,i],g_decision_list)
    colnames(s_data)=c('id','TrialNum','s_decision','g_decision','gain','loss','outcome','n1','n2','n3','n4')
    raw_data=rbind(raw_data,s_data)
   }
  }
}


write.table(file='Simulation_Data.txt',raw_data,col.names = F,row.names = F)





simul_control_data<-c()
for(s in 1:length(control_group_tag[,1])){
  tmp_data<-subset(raw_data,id==control_group_tag[s,1])
  simul_control_data<-rbind(simul_control_data,tmp_data)
}

outcome_list_c<-c()
for(s in 1:length(control_group_tag[,1])){
  tmp_data<-subset(raw_data,id==control_group_tag[s,1])
  outcome<-tmp_data[100,7]
  outcome_list_c<-c(outcome_list_c,outcome)
}

simul_discuss_data<-c()
for(s in 1:length(discuss_group_tag[,1])){
  tmp_data<-subset(raw_data,id==discuss_group_tag[s,1])
  simul_discuss_data<-rbind(simul_discuss_data,tmp_data)
}

outcome_list_d<-c()
for(s in 1:length(discuss_group_tag[,1])){
  tmp_data<-subset(raw_data,id==discuss_group_tag[s,1])
  outcome<-tmp_data[100,7]
  outcome_list_d<-c(outcome_list_d,outcome)
}

control_decision<-c()
for(s in 1:length(control_group_tag[,1])){
  tmp_data<-subset(raw_data,id==control_group_tag[s,1])$g_decision
  control_decision<-cbind(control_decision,tmp_data)
}

real_control_decision<-c()
for(s in 1:length(control_group_tag[,1])){
  tmp_data<-subset(no_leader_data,no_leader_data[,1]==control_group_tag[s,1])[,13]-10
  real_control_decision<-cbind(real_control_decision,tmp_data)
}

control_vote<-c()
for(s in 1:length(control_group_tag)){
  tmp_data<-subset(raw_data,id==control_group_tag[s])$s_decision
  control_vote<-rbind(control_vote,tmp_data)
}

real_control_vote<-c()
for(s in 1:length(control_group_tag)){
  tmp_data<-subset(no_leader_data,no_leader_data[,1]==control_group_tag[s])[,11]-10
  real_control_vote<-rbind(real_control_vote,tmp_data)
}

real_discuss_decision<-c()
for(s in 1:length(discuss_group_tag[,1])){
  tmp_data<-subset(no_leader_data,no_leader_data[,1]==discuss_group_tag[s,1])[,13]-10
  real_discuss_decision<-cbind(real_discuss_decision,tmp_data)
}

discuss_decision<-c()
for(s in 1:length(discuss_group_tag[,1])){
  tmp_data<-subset(raw_data,id==discuss_group_tag[s,1])$g_decision
  discuss_decision<-cbind(discuss_decision,tmp_data)
}

discuss_vote<-c()
for(s in 1:length(discuss_group_tag)){
  tmp_data<-subset(raw_data,id==discuss_group_tag[s])$s_decision
  discuss_vote<-rbind(discuss_vote,tmp_data)
}

real_discuss_vote<-c()
for(s in 1:length(discuss_group_tag)){
  tmp_data<-subset(no_leader_data,no_leader_data[,1]==discuss_group_tag[s])[,11]-10
  real_discuss_vote<-rbind(real_discuss_vote,tmp_data)
}

real_leader_vote<-c()
for(g in 1:dim(leader_group_tag)[1]){
  for(s in 1:dim(leader_group_tag)[2]){
    if(s!=5){
       tmp_data<-subset(no_leader_data,no_leader_data[,1]==leader_group_tag[g,s])[,11]-10
       real_leader_vote<-rbind(real_leader_vote,tmp_data)
    }else{
      tmp_data<-subset(leader_data,leader_data[,1]==leader_group_tag[g,s])[,11]-10
      real_leader_vote<-rbind(real_leader_vote,tmp_data)
    }
  }
}

leader_vote<-c()
for(g in 1:dim(leader_group_tag)[1]){
  for(s in 1:dim(leader_group_tag)[2]){
      tmp_data<-subset(raw_data,raw_data[,1]==leader_group_tag[g,s])$s_decision
      leader_vote<-rbind(leader_vote,tmp_data)
    }
}

real_leader_decision<-c()
for(g in 1:dim(leader_group_tag)[1]){
      tmp_data<-subset(leader_data,leader_data[,1]==leader_group_tag[g,5])[,16]-10
      real_leader_decision<-rbind(real_leader_decision,tmp_data)
}

leader_decision<-c()
for(g in 1:dim(leader_group_tag)[1]){
  tmp_data<-subset(raw_data,raw_data[,1]==leader_group_tag[g,5])$g_decision
  leader_decision<-rbind(leader_decision,tmp_data)
}
  
real_mix_vote<-c()
for(g in 1:dim(mix_group_tag)[1]){
  for(s in 1:dim(mix_group_tag)[2]){
    if(s!=5){
      tmp_data<-subset(no_leader_data,no_leader_data[,1]==mix_group_tag[g,s])[,11]-10
      real_mix_vote<-rbind(real_mix_vote,tmp_data)
    }else{
      tmp_data<-subset(leader_data,leader_data[,1]==mix_group_tag[g,s])[,11]-10
      real_mix_vote<-rbind(real_mix_vote,tmp_data)
    }
  }
}

mix_vote<-c()
for(g in 1:dim(mix_group_tag)[1]){
  for(s in 1:dim(mix_group_tag)[2]){
    tmp_data<-subset(raw_data,raw_data[,1]==mix_group_tag[g,s])$s_decision
    mix_vote<-rbind(mix_vote,tmp_data)
  }
}

real_mix_decision<-c()
for(g in 1:dim(mix_group_tag)[1]){
  tmp_data<-subset(leader_data,leader_data[,1]==mix_group_tag[g,5])[,16]-10
  real_mix_decision<-rbind(real_mix_decision,tmp_data)
}

mix_decision<-c()
for(g in 1:dim(mix_group_tag)[1]){
  tmp_data<-subset(raw_data,raw_data[,1]==mix_group_tag[g,5])$g_decision
  mix_decision<-rbind(mix_decision,tmp_data)
}

p_sim<-c()
for(t in 1:100){
  p1<-length((which(control_decision[t,]==1)))/length(control_decision[t,])
  p2<-length((which(control_decision[t,]==2)))/length(control_decision[t,])
  p3<-length((which(control_decision[t,]==3)))/length(control_decision[t,])
  p4<-length((which(control_decision[t,]==4)))/length(control_decision[t,])
  p_sim<-rbind(p_sim,c(p1,p2,p3,p4))
}
TrialNum=c(1:100)
p_sim<-data.frame(TrialNum,p_sim)
colnames(p_sim)<-c('TrialNum','p','p','p','p')
p<-rbind(p_sim[,c(1,2)],p_sim[,c(1,3)],p_sim[,c(1,4)],p_sim[,c(1,5)])
Deck<-c(rep('DeckA',100),rep('DeckB',100),rep('DeckC',100),rep('DeckD',100))
p<-data.frame(p,Deck)
p$Deck<-as.factor(p$Deck)
ggplot(p,aes(x=TrialNum,y=p,color=Deck))+geom_line()+geom_smooth()

p_sim<-c()
for(t in 1:100){
  p1<-length((which(discuss_decision[t,]==1)))/length(discuss_decision[t,])
  p2<-length((which(discuss_decision[t,]==2)))/length(discuss_decision[t,])
  p3<-length((which(discuss_decision[t,]==3)))/length(discuss_decision[t,])
  p4<-length((which(discuss_decision[t,]==4)))/length(discuss_decision[t,])
  p_sim<-rbind(p_sim,c(p1,p2,p3,p4))
}
TrialNum=c(1:100)
p_sim<-data.frame(TrialNum,p_sim)
colnames(p_sim)<-c('TrialNum','p','p','p','p')
p<-rbind(p_sim[,c(1,2)],p_sim[,c(1,3)],p_sim[,c(1,4)],p_sim[,c(1,5)])
Deck<-c(rep('DeckA',100),rep('DeckB',100),rep('DeckC',100),rep('DeckD',100))
p<-data.frame(p,Deck)
p$Deck<-as.factor(p$Deck)
ggplot(p,aes(x=TrialNum,y=p,color=Deck))+geom_line()+geom_smooth()

outcome_list=c(outcome_list_c,outcome_list_d)
outcome_list<-rbind(outcome_list,c(rep('Control',31),rep('Discuss',28)))
outcome_list<-t(outcome_list)
colnames(outcome_list)<-c('Outcome','Condition')
outcome_list<-as.data.frame(outcome_list)
outcome_list$Condition<-as.factor(outcome_list$Condition)
ggplot(outcome_list,aes(x=Condition,y=Outcome,group=Condition))+geom_boxplot(notch=TRUE)


#calculate group-level simulation accuracy
control_correction_matrix<-matrix(rep(0,3100),nrow=100,ncol=31)
for (i in 1:dim(real_control_decision)[1]){
  for(t in 1:dim(real_control_decision)[2]){
    if (control_decision[i,t]==real_control_decision[i,t]){
      control_correction_matrix[i,t]=1
    }else{
      control_correction_matrix[i,t]=0
    }
  }
}

discuss_correction_matrix<-matrix(rep(0,2800),nrow=100,ncol=28)
for (i in 1:dim(real_discuss_decision)[1]){
  for(t in 1:dim(real_discuss_decision)[2]){
    if (discuss_decision[i,t]==real_discuss_decision[i,t]){
      discuss_correction_matrix[i,t]=1
    }else{
      discuss_correction_matrix[i,t]=0
    }
  }
}

control_v_correction_matrix<-matrix(rep(-1,15500),nrow=155,ncol=100)
for (i in 1:dim(real_control_vote)[1]){
  for(t in 1:dim(real_control_vote)[2]){
    if (control_vote[i,t]==real_control_vote[i,t]){
      control_v_correction_matrix[i,t]=1
    }else{
      control_v_correction_matrix[i,t]=0
    }
  }
}

discuss_v_correction_matrix<-matrix(rep(-1,14000),nrow=140,ncol=100)
for (i in 1:dim(real_discuss_vote)[1]){
  for(t in 1:dim(real_discuss_vote)[2]){
    if (discuss_vote[i,t]==real_discuss_vote[i,t]){
      discuss_v_correction_matrix[i,t]=1
    }else{
      discuss_v_correction_matrix[i,t]=0
    }
  }
}

leader_v_correction_matrix<-matrix(rep(-1,16000),nrow=160,ncol=100)
for (i in 1:dim(real_leader_vote)[1]){
  for(t in 1:dim(real_leader_vote)[2]){
    if (leader_vote[i,t]==real_leader_vote[i,t]){
      leader_v_correction_matrix[i,t]=1
    }else{
      leader_v_correction_matrix[i,t]=0
    }
  }
}

mix_v_correction_matrix<-matrix(rep(-1,14000),nrow=140,ncol=100)
for (i in 1:dim(real_mix_vote)[1]){
  for(t in 1:dim(real_mix_vote)[2]){
    if (mix_vote[i,t]==real_mix_vote[i,t]){
      mix_v_correction_matrix[i,t]=1
    }else{
      mix_v_correction_matrix[i,t]=0
    }
  }
}

leader_correction_matrix<-matrix(rep(0,3200),nrow=32,ncol=100)
for (i in 1:dim(real_leader_decision)[1]){
  for(t in 1:dim(real_leader_decision)[2]){
    if (leader_decision[i,t]==real_leader_decision[i,t]){
      leader_correction_matrix[i,t]=1
    }else{
      leader_correction_matrix[i,t]=0
    }
  }
}

mix_correction_matrix<-matrix(rep(0,2800),nrow=28,ncol=100)
for (i in 1:dim(real_mix_decision)[1]){
  for(t in 1:dim(real_mix_decision)[2]){
    if (mix_decision[i,t]==real_mix_decision[i,t]){
      mix_correction_matrix[i,t]=1
    }else{
      mix_correction_matrix[i,t]=0
    }
  }
}

p_time<-apply(leader_correction_matrix,2,mean)
p_group<-apply(leader_correction_matrix,1,mean)
p_time<-as.data.frame(p_time)
p_group<-as.data.frame(p_group)
ggplot(p_time,aes(x=c(1:100),y=p_time))+geom_line()
ggplot(p_group,aes(x=p_group))+geom_histogram(bins=10)

sim_g_decision_list=matrix(rep(0,3100),ncol=100,nrow=31)
for(g in 1:dim(control_group_tag)[1]){
  for(t in 1:100){
     n<-c(0,0,0,0)
    for(i in 1:5){
      tmp_data<-subset(raw_data,id==control_group_tag[g,i])
      s_decision<-tmp_data$s_decision[t]
      n[s_decision]=n[s_decision]+1
     }
      g_decision=which(n==max(n))
      if(length(g_decision)>1){
        g_decision=g_decision[sample(1:length(g_decision),1)]
      }
      sim_g_decision_list[g,t]=g_decision
  }
}

sim_g_decision_list<-t(sim_g_decision_list)
control_correction_matrix<-matrix(rep(0,3100),nrow=100,ncol=31)
for (i in 1:dim(real_control_decision)[1]){
  for(t in 1:dim(real_control_decision)[2]){
    if (sim_g_decision_list[i,t]==real_control_decision[i,t]){
      control_correction_matrix[i,t]=1
    }else{
      control_correction_matrix[i,t]=0
    }
  }
}
p_time<-apply(control_correction_matrix,1,mean)
p_group<-apply(control_correction_matrix,2,mean)
p_time<-as.data.frame(p_time)
p_group<-as.data.frame(p_group)
ggplot(p_time,aes(x=c(1:100),y=p_time))+geom_line()
ggplot(p_group,aes(x=p_group))+geom_histogram(bins=10)





sim_g_decision_list=matrix(rep(0,2800),ncol=100,nrow=28)
for(g in 1:dim(discuss_group_tag)[1]){
  for(t in 1:100){
    n<-c(0,0,0,0)
    for(i in 1:5){
      tmp_data<-subset(raw_data,id==discuss_group_tag[g,i])
      s_decision<-tmp_data$s_decision[t]
      n[s_decision]=n[s_decision]+1
    }
    g_decision=which(n==max(n))
    if(length(g_decision)>1){
      g_decision=g_decision[sample(1:length(g_decision),1)]
    }
    sim_g_decision_list[g,t]=g_decision
  }
}

sim_g_decision_list<-t(sim_g_decision_list)
discuss_correction_matrix<-matrix(rep(0,2800),nrow=100,ncol=28)
for (i in 1:dim(real_discuss_decision)[1]){
  for(t in 1:dim(real_discuss_decision)[2]){
    if (sim_g_decision_list[i,t]==real_discuss_decision[i,t]){
      discuss_correction_matrix[i,t]=1
    }else{
      discuss_correction_matrix[i,t]=0
    }
  }
}
p_time<-apply(discuss_correction_matrix,1,mean)
p_group<-apply(discuss_correction_matrix,2,mean)
p_time<-as.data.frame(p_time)
p_group<-as.data.frame(p_group)
ggplot(p_time,aes(x=c(1:100),y=p_time))+geom_line()
ggplot(p_group,aes(x=p_group))+geom_histogram(bins=10)


control_v_p<-apply(control_v_correction_matrix,1,mean)
mean_control_v_p<-data.frame(mean(control_v_p),sd(control_v_p)/sqrt(length(control_v_p)),0,0)
control_v_p<-data.frame(control_v_p,rep(0,dim(control_v_correction_matrix)[1]),rep(0,dim(control_v_correction_matrix)[1]))
colnames(control_v_p)=c('Individual_Accuracy','Discuss','Leader')
colnames(mean_control_v_p)=c('Mean_Accuracy','sd','Discuss','Leader')

discuss_v_p<-apply(discuss_v_correction_matrix,1,mean)
mean_discuss_v_p<-data.frame(mean(discuss_v_p),sd(discuss_v_p)/sqrt(length(discuss_v_p)),1,0)
discuss_v_p<-data.frame(discuss_v_p,rep(1,dim(discuss_v_correction_matrix)[1]),rep(0,dim(discuss_v_correction_matrix)[1]))
colnames(discuss_v_p)=c('Individual_Accuracy','Discuss','Leader')
colnames(mean_discuss_v_p)=c('Mean_Accuracy','sd','Discuss','Leader')

leader_v_p<-apply(leader_v_correction_matrix,1,mean)
mean_leader_v_p<-data.frame(mean(leader_v_p),sd(leader_v_p)/sqrt(length(leader_v_p)),0,1)
leader_v_p<-data.frame(leader_v_p,rep(0,dim(leader_v_correction_matrix)[1]),rep(1,dim(leader_v_correction_matrix)[1]))
colnames(leader_v_p)=c('Individual_Accuracy','Discuss','Leader')
colnames(mean_leader_v_p)=c('Mean_Accuracy','sd','Discuss','Leader')


mix_v_p<-apply(mix_v_correction_matrix,1,mean)
mean_mix_v_p<-data.frame(mean(mix_v_p),sd(mix_v_p)/sqrt(length(mix_v_p)),1,1)
mix_v_p<-data.frame(mix_v_p,rep(1,dim(mix_v_correction_matrix)[1]),rep(1,dim(mix_v_correction_matrix)[1]))
colnames(mix_v_p)=c('Individual_Accuracy','Discuss','Leader')
colnames(mean_mix_v_p)=c('Mean_Accuracy','sd','Discuss','Leader')

v_p<-rbind(control_v_p,discuss_v_p,leader_v_p,mix_v_p)
mean_v_p<-rbind(mean_control_v_p,mean_discuss_v_p,mean_leader_v_p,mean_mix_v_p)


control_g_p<-apply(control_correction_matrix,2,mean)
mean_control_g_p<-data.frame(mean(control_g_p),sd(control_g_p)/sqrt(length(control_g_p)),0,0)
control_g_p<-data.frame(control_g_p,rep(0,dim(control_correction_matrix)[2]),rep(0,dim(control_correction_matrix)[2]))
colnames(control_g_p)=c('Group_Accuracy','Discuss','Leader')
colnames(mean_control_g_p)=c('Mean_Accuracy','sd','Discuss','Leader')


discuss_g_p<-apply(discuss_correction_matrix,2,mean)
mean_discuss_g_p<-data.frame(mean(discuss_g_p),sd(discuss_g_p)/sqrt(length(discuss_g_p)),1,0)
discuss_g_p<-data.frame(discuss_g_p,rep(1,dim(discuss_correction_matrix)[2]),rep(0,dim(discuss_correction_matrix)[2]))
colnames(discuss_g_p)=c('Group_Accuracy','Discuss','Leader')
colnames(mean_discuss_g_p)=c('Mean_Accuracy','sd','Discuss','Leader')


leader_g_p<-apply(leader_correction_matrix,2,mean)
mean_leader_g_p<-data.frame(mean(leader_g_p),sd(leader_g_p)/sqrt(length(leader_g_p)),0,1)
leader_g_p<-data.frame(leader_g_p,rep(0,dim(leader_correction_matrix)[2]),rep(1,dim(leader_correction_matrix)[2]))
colnames(leader_g_p)=c('Group_Accuracy','Discuss','Leader')
colnames(mean_leader_g_p)=c('Mean_Accuracy','sd','Discuss','Leader')


mix_g_p<-apply(mix_correction_matrix,2,mean)
mean_mix_g_p<-data.frame(mean(mix_g_p),sd(mix_g_p)/sqrt(length(mix_g_p)),1,1)
mix_g_p<-data.frame(mix_g_p,rep(1,dim(mix_correction_matrix)[2]),rep(1,dim(mix_correction_matrix)[2]))
colnames(mix_g_p)=c('Group_Accuracy','Discuss','Leader')
colnames(mean_mix_g_p)=c('Mean_Accuracy','sd','Discuss','Leader')


g_p<-rbind(control_g_p,discuss_g_p,leader_g_p,mix_g_p)
mean_g_p<-rbind(mean_control_g_p,mean_discuss_g_p,mean_leader_g_p,mean_mix_g_p)