
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

##the effect of social influence on exploration
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
tmp_data<-subset(plot_raw_data2, leader==0&discuss==1) # two conditions: leader==0 & discuss == 0 or 1


ggplot(data=tmp_data)+
  geom_point(aes(x=Type,y=Probability),size=10,color='skyblue',alpha=0.8)+
  geom_line(aes(x=(Type),y=Probability,group=tmp_id),linetype='dashed',size=1,alpha=0.1)+
  scale_x_discrete(labels=c('Minimum N','Maximum N'))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  ylab("Probablitiy of exploration")+
  pic_theme

t.test(subset(tmp_data,Type==0)$Probability,subset(tmp_data,Type==1)$Probability,paired = T)

t_to_d(16.798,154)
t_to_d(19.259,138)

##########################################
##non-leading members, social influence on exploration
i_data_new <-c()
no_leader_id_list<-unique(no_leader_data[,1])
no_leader_id_list<-no_leader_id_list[no_leader_id_list>3000] # non-leading members of leader groups
for (i in 1:length(no_leader_id_list)){
  tmp_id=no_leader_id_list[i]
  tmp_data<-no_leader_data[which(no_leader_data[,1]==tmp_id),]
  discuss=group_individual_class(tmp_id,4)[1]
  leader=group_individual_class(tmp_id,4)[2]
  tmp_i_data<-c()
  for (t in 2:100){
    
    tmp_four_social <- c(tmp_data[t-1,14],tmp_data[t-1,15],tmp_data[t-1,16],tmp_data[t-1,17])
    first_max <- sort(tmp_four_social,decreasing = T)[1]
    second_max <- sort(tmp_four_social,decreasing = T)[2]
    third_max <- sort(tmp_four_social,decreasing = T)[3]
    forth_max <- sort(tmp_four_social,decreasing = T)[4]
    first_max_position <- which(tmp_four_social==first_max)
    second_max_position <- which(tmp_four_social == second_max)
    third_max_position <- which(tmp_four_social == third_max)
    forth_max_position <- which(tmp_four_social == forth_max)
    
    if(first_max==5){
      if(tmp_data[t-1,12]==first_max_position){ # 5,0,0,0; leader chose N=5
        Highsocial_chose <- -666
        Lowsocial_chose <- -666
      }else if(tmp_data[t-1,12]==second_max_position[1]){# 5,0,0,0; leader chose N=0
        if(tmp_data[t,3]==second_max_position[1]){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,3]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,3]==third_max_position[2] | tmp_data[t,3]==forth_max_position[3]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(tmp_data[t-1,12]==second_max_position[2]){
        if(tmp_data[t,3]==second_max_position[2]){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,3]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,3]==third_max_position[1] | tmp_data[t,3]==forth_max_position[3]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(tmp_data[t-1,12]==second_max_position[3]){
        if(tmp_data[t,3]==second_max_position[3]){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,3]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,3]==third_max_position[1] | tmp_data[t,3]==forth_max_position[2]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }
    }else if(first_max==4){
      if(tmp_data[t-1,12]==first_max_position){ # 4,1,0,0; leader chose N=4
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
      }else if(tmp_data[t-1,12]==second_max_position){
        if(tmp_data[t,3]==second_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,3]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,3]==third_max_position[1] | tmp_data[t,3]==forth_max_position[2]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(tmp_data[t-1,12]==third_max_position[1]){
        if(tmp_data[t,3]==third_max_position[1] | tmp_data[t,3]==second_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,3]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,3]==forth_max_position[2]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(tmp_data[t-1,12]==third_max_position[2]){
        if(tmp_data[t,3]==third_max_position[2] | tmp_data[t,3]==second_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,3]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,3]==forth_max_position[1]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }
    }else if(first_max==3){
      if(length(second_max_position)==1){# 3,2,0,0, 
        if(tmp_data[t-1,12]==first_max_position){ 
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
        }else if(tmp_data[t-1,12]==second_max_position){
          if(tmp_data[t,3]==second_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,3]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,3]==third_max_position[1] | tmp_data[t,3]==forth_max_position[2]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==third_max_position[1]){
          if(tmp_data[t,3]==third_max_position[1] | tmp_data[t,3]==second_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,3]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,3]==forth_max_position[2]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==third_max_position[2]){
          if(tmp_data[t,3]==third_max_position[2] | tmp_data[t,3]==second_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,3]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,3]==forth_max_position[1]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }
      }else if(length(second_max_position)==2){ #3,1,1,0
        if(tmp_data[t-1,12]==first_max_position){
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
        }else if(tmp_data[t-1,12]==second_max_position[1]){
          if(tmp_data[t,3]==second_max_position[1] | tmp_data[t,3]==third_max_position[2]){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,3]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,3]==forth_max_position){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==second_max_position[2]){
          if(tmp_data[t,3]==second_max_position[2] | tmp_data[t,3]==third_max_position[1]){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,3]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,3]==forth_max_position){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==forth_max_position){
          if(tmp_data[t,3]==forth_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,3]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,3]==second_max_position[1] | tmp_data[t,3]==third_max_position[2]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }
      }
    }else if(first_max==2){
      if(length(first_max_position)==1){# 2,1,1,1, 
        if(tmp_data[t-1,12]==first_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t-1,12]==second_max_position[1]){
          if(tmp_data[t,3]==second_max_position[1]){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,3]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,3]==second_max_position[2] | tmp_data[t,3]==second_max_position[3]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==second_max_position[2]){
          if(tmp_data[t,3]==second_max_position[2]){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,3]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,3]==second_max_position[1] | tmp_data[t,3]==second_max_position[3]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==second_max_position[3]){
          if(tmp_data[t,3]==second_max_position[3]){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,3]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,3]==second_max_position[1] | tmp_data[t,3]==second_max_position[2]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }
      }else if(length(first_max_position)==2){# 2,2,1,0, 
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
        }else if(tmp_data[t-1,12]==third_max_position){
          if(tmp_data[t,3]==third_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,3]==first_max_position[1] | tmp_data[t,3]==second_max_position[2]){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,3]==forth_max_position){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==forth_max_position){
          if(tmp_data[t,3]==forth_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,3]==first_max_position[1] | tmp_data[t,3]==second_max_position[2]){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,3]==third_max_position){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }
      }
    }
    
    
    individual_choice = tmp_data[t,3]
    group_choice_last = tmp_data[t-1,12]
    social_number_last=tmp_data[t-1,14:17]
    tmp_i_data<-rbind(tmp_i_data,data.frame(tmp_id,discuss,leader,individual_choice,group_choice_last,social_number_last,Highsocial_chose,Lowsocial_chose))
  }
  i_data_new<-rbind(i_data_new,tmp_i_data)
}


##the effect of social influence on exploration, non-leading members
plot_raw_data2<-subset(i_data_new,Highsocial_chose!=-666 & Lowsocial_chose!=-666)
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
#non-leading  members
tmp_data<-subset(plot_raw_data2, leader==1&discuss==1) # two conditions, leader==1 & discuss == 0 or 1

ggplot(data=tmp_data)+
  geom_point(aes(x=Type,y=Probability),size=10,color='skyblue',alpha=0.8)+
  geom_line(aes(x=(Type),y=Probability,group=tmp_id),linetype='dashed',size=1,alpha=0.1)+
  scale_x_discrete(labels=c('Minimum N','Maximum N'))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  ylab("Probablitiy of exploration")+
  pic_theme

t.test(subset(tmp_data,Type==0)$Probability,subset(tmp_data,Type==1)$Probability, paired = T)

t_to_d(19.06,123)
t_to_d(15.667,111)




##########################################
##leader, social influence on exploration
i_data_new<-c()
leader_id_list<-unique(leader_data[,1])
for (i in 1:length(leader_id_list)){
  tmp_id=leader_id_list[i]
  tmp_data<-leader_data[which(leader_data[,1]==tmp_id),]
  discuss=group_individual_class(tmp_id,5)[1]
  leader=group_individual_class(tmp_id,5)[2]
  tmp_i_data<-c()
  for (t in 2:100){
    
    tmp_four_social <- c(tmp_data[t,17],tmp_data[t,18],tmp_data[t,19],tmp_data[t,20])
    first_max <- sort(tmp_four_social,decreasing = T)[1]
    second_max <- sort(tmp_four_social,decreasing = T)[2]
    third_max <- sort(tmp_four_social,decreasing = T)[3]
    forth_max <- sort(tmp_four_social,decreasing = T)[4]
    first_max_position <- which(tmp_four_social==first_max)
    second_max_position <- which(tmp_four_social == second_max)
    third_max_position <- which(tmp_four_social == third_max)
    forth_max_position <- which(tmp_four_social == forth_max)
    
    if(first_max==5){
      if(tmp_data[t-1,12]==first_max_position){ # 5,0,0,0; leader chose N=5
        Highsocial_chose <- -666
        Lowsocial_chose <- -666
      }else if(tmp_data[t-1,12]==second_max_position[1]){# 5,0,0,0; leader chose N=0
        if(tmp_data[t,12]==second_max_position[1]){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,12]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,12]==third_max_position[2] | tmp_data[t,12]==forth_max_position[3]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(tmp_data[t-1,12]==second_max_position[2]){
        if(tmp_data[t,12]==second_max_position[2]){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,12]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,12]==third_max_position[1] | tmp_data[t,12]==forth_max_position[3]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(tmp_data[t-1,12]==second_max_position[3]){
        if(tmp_data[t,12]==second_max_position[3]){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,12]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,12]==third_max_position[1] | tmp_data[t,12]==forth_max_position[2]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }
    }else if(first_max==4){
      if(tmp_data[t-1,12]==first_max_position){ # 4,1,0,0; leader chose N=4
        if(tmp_data[t,12]==first_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,12]==second_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,12]==third_max_position[1] | tmp_data[t,12]==forth_max_position[2]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(tmp_data[t-1,12]==second_max_position){
        if(tmp_data[t,12]==second_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,12]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,12]==third_max_position[1] | tmp_data[t,12]==forth_max_position[2]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(tmp_data[t-1,12]==third_max_position[1]){
        if(tmp_data[t,12]==third_max_position[1] | tmp_data[t,12]==second_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,12]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,12]==forth_max_position[2]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }else if(tmp_data[t-1,12]==third_max_position[2]){
        if(tmp_data[t,12]==third_max_position[2] | tmp_data[t,12]==second_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t,12]==first_max_position){
          Highsocial_chose <- 1
          Lowsocial_chose <- 0
        }else if(tmp_data[t,12]==forth_max_position[1]){
          Highsocial_chose <- 0
          Lowsocial_chose <- 1
        }
      }
    }else if(first_max==3){
      if(length(second_max_position)==1){# 3,2,0,0, 
        if(tmp_data[t-1,12]==first_max_position){ 
          if(tmp_data[t,12]==first_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==second_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==third_max_position[1] | tmp_data[t,12]==forth_max_position[2]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==second_max_position){
          if(tmp_data[t,12]==second_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==third_max_position[1] | tmp_data[t,12]==forth_max_position[2]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==third_max_position[1]){
          if(tmp_data[t,12]==third_max_position[1] | tmp_data[t,12]==second_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==forth_max_position[2]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==third_max_position[2]){
          if(tmp_data[t,12]==third_max_position[2] | tmp_data[t,12]==second_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==forth_max_position[1]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }
      }else if(length(second_max_position)==2){ #3,1,1,0
        if(tmp_data[t-1,12]==first_max_position){
          if(tmp_data[t,12]==first_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==second_max_position[1] | tmp_data[t,12]==third_max_position[2]){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==forth_max_position){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==second_max_position[1]){
          if(tmp_data[t,12]==second_max_position[1] | tmp_data[t,12]==third_max_position[2]){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==forth_max_position){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==second_max_position[2]){
          if(tmp_data[t,12]==second_max_position[2] | tmp_data[t,12]==third_max_position[1]){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==forth_max_position){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==forth_max_position){
          if(tmp_data[t,12]==forth_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==second_max_position[1] | tmp_data[t,12]==third_max_position[2]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }
      }
    }else if(first_max==2){
      if(length(first_max_position)==1){# 2,1,1,1, 
        if(tmp_data[t-1,12]==first_max_position){
          Highsocial_chose <- -666
          Lowsocial_chose <- -666
        }else if(tmp_data[t-1,12]==second_max_position[1]){
          if(tmp_data[t,12]==second_max_position[1]){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==second_max_position[2] | tmp_data[t,12]==second_max_position[3]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==second_max_position[2]){
          if(tmp_data[t,12]==second_max_position[2]){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==second_max_position[1] | tmp_data[t,12]==second_max_position[3]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==second_max_position[3]){
          if(tmp_data[t,12]==second_max_position[3]){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==second_max_position[1] | tmp_data[t,12]==second_max_position[2]){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }
      }else if(length(first_max_position)==2){# 2,2,1,0, 
        if(tmp_data[t-1,12]==first_max_position[1]){
          if(tmp_data[t,12]==first_max_position[1] | tmp_data[t,12]==third_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position[2]){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==forth_max_position){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==first_max_position[2]){
          if(tmp_data[t,12]==first_max_position[2] | tmp_data[t,12]==third_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position[1]){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==forth_max_position){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==third_max_position){
          if(tmp_data[t,12]==third_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position[1] | tmp_data[t,12]==second_max_position[2]){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==forth_max_position){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }else if(tmp_data[t-1,12]==forth_max_position){
          if(tmp_data[t,12]==forth_max_position){
            Highsocial_chose <- -666
            Lowsocial_chose <- -666
          }else if(tmp_data[t,12]==first_max_position[1] | tmp_data[t,12]==second_max_position[2]){
            Highsocial_chose <- 1
            Lowsocial_chose <- 0
          }else if(tmp_data[t,12]==third_max_position){
            Highsocial_chose <- 0
            Lowsocial_chose <- 1
          }
        }
      }
    }
    
    
    individual_choice = tmp_data[t,12] # leader's second choice on the current trial
    group_choice_last = tmp_data[t-1,12] # group's decision on the last trial
    social_number_last=tmp_data[t,17:20] # leader's social influence on the current trial
    tmp_i_data<-rbind(tmp_i_data,data.frame(tmp_id,discuss,leader,individual_choice,group_choice_last,social_number_last,Highsocial_chose,Lowsocial_chose))
  }
  i_data_new<-rbind(i_data_new,tmp_i_data)
}


##the effect of social influence on exploration, leader
plot_raw_data2<-subset(i_data_new,Highsocial_chose!=-666 & Lowsocial_chose!=-666)
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
#leader
tmp_data<-subset(plot_raw_data2, leader==2&discuss==1) # two conditions, leader=2 & discuss = 0 or 1

ggplot(data=tmp_data)+
  geom_point(aes(x=Type,y=Probability),size=10,color='skyblue',alpha=0.8)+
  geom_line(aes(x=(Type),y=Probability,group=tmp_id),linetype='dashed',size=1,alpha=0.1)+
  scale_x_discrete(labels=c('Minimum N','Maximum N'))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  ylab("Probablitiy of exploration")+
  pic_theme

t.test(subset(tmp_data,Type==0)$Probability,subset(tmp_data,Type==1)$Probability, paired = T)

t_to_d(28.801,30)
t_to_d(37.573,27)
