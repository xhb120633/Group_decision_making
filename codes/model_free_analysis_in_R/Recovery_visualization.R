library(ggplot2)
library(R.matlab)
library(ggstatsplot)
library(cowplot)
library(Rmisc)
library(gridExtra)
library(viridis)

pic_theme <- theme(axis.title = element_text(face="bold", size = 45),
                   axis.text = element_text(face="bold", size = 36),
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


###prepare the dataset
recovered_control_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_control_parameter.mat')
recovered_control_parameter<-as.data.frame(recovered_control_parameter)

recovered_discuss_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_discuss_parameter.mat')
recovered_discuss_parameter<-as.data.frame(recovered_discuss_parameter)

recovered_leaderM_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_leaderM_parameter.mat')
recovered_leaderM_parameter<-as.data.frame(recovered_leaderM_parameter)

recovered_leaderL_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_leaderL_parameter.mat')
recovered_leaderL_parameter<-as.data.frame(recovered_leaderL_parameter)

recovered_mixM_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_mixM_parameter.mat')
recovered_mixM_parameter<-as.data.frame(recovered_mixM_parameter)

recovered_mixL_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_mixL_parameter.mat')
recovered_mixL_parameter<-as.data.frame(recovered_mixL_parameter)

parameter_as_recovered<-parameter_as
colnames(parameter_as_recovered)[1:6]<-c('Sensitivity_R','Decay_R','LearningRate_R','ExplorBonus_R','BetaOthers_R','Consistency_R')

parameter_as_recovered[1:155,1:6]=recovered_control_parameter
parameter_as_recovered[156:295,1:6]=recovered_discuss_parameter
parameter_as_recovered[296:423,1:6]=recovered_leaderM_parameter
parameter_as_recovered[456:567,1:6]=recovered_mixM_parameter
parameter_as_recovered[ll_loc,1:6]=recovered_leaderL_parameter
parameter_as_recovered[ml_loc,1:6]=recovered_mixL_parameter

parameter_as_recovered<-data.frame(parameter_as,parameter_as_recovered[,1:6])

##ggstatsplot
rp1_control_1<-ggplot(data=subset(parameter_as_recovered, Group=='Control'),aes(x=Sensitivity,y=Sensitivity_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp1_control_2<-ggplot(data=subset(parameter_as_recovered, Group=='Control'),aes(x=Sensitivity_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='Control')$Sensitivity),color='red',size=1.2)+
  pic_theme


rp2_control_1<-ggplot(data=subset(parameter_as_recovered, Group=='Control'),aes(x=Decay,y=Decay_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp2_control_2<-ggplot(data=subset(parameter_as_recovered, Group=='Control'),aes(x=Decay_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='Control')$Decay),color='red',size=1.2)+
  pic_theme

rp3_control_1<-ggplot(data=subset(parameter_as_recovered, Group=='Control'),aes(x=LearningRate,y=LearningRate_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp3_control_2<-ggplot(data=subset(parameter_as_recovered, Group=='Control'),aes(x=LearningRate_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='Control')$LearningRate),color='red',size=1.2)+
  pic_theme


rp4_control_1<-ggplot(data=subset(parameter_as_recovered, Group=='Control'),aes(x=ExplorBonus,y=ExplorBonus_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp4_control_2<-ggplot(data=subset(parameter_as_recovered, Group=='Control'),aes(x=ExplorBonus_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='Control')$ExplorBonus),color='red',size=1.2)+
  pic_theme


rp5_control_1<-ggplot(data=subset(parameter_as_recovered, Group=='Control'),aes(x=BetaOthers,y=BetaOthers_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp5_control_2<-ggplot(data=subset(parameter_as_recovered, Group=='Control'),aes(x=BetaOthers_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='Control')$BetaOthers),color='red',size=1.2)+
  pic_theme


rp1_discuss_1<-ggplot(data=subset(parameter_as_recovered, Group=='Discuss'),aes(x=Sensitivity,y=Sensitivity_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp1_discuss_2<-ggplot(data=subset(parameter_as_recovered, Group=='Discuss'),aes(x=Sensitivity_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='Discuss')$Sensitivity),color='red',size=1.2)+
  pic_theme


rp2_discuss_1<-ggplot(data=subset(parameter_as_recovered, Group=='Discuss'),aes(x=Decay,y=Decay_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp2_discuss_2<-ggplot(data=subset(parameter_as_recovered, Group=='Discuss'),aes(x=Decay_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='Discuss')$Decay),color='red',size=1.2)+
  pic_theme

rp3_discuss_1<-ggplot(data=subset(parameter_as_recovered, Group=='Discuss'),aes(x=LearningRate,y=LearningRate_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp3_discuss_2<-ggplot(data=subset(parameter_as_recovered, Group=='Discuss'),aes(x=LearningRate_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='Discuss')$LearningRate),color='red',size=1.2)+
  pic_theme


rp4_discuss_1<-ggplot(data=subset(parameter_as_recovered, Group=='Discuss'),aes(x=ExplorBonus,y=ExplorBonus_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp4_discuss_2<-ggplot(data=subset(parameter_as_recovered, Group=='Discuss'),aes(x=ExplorBonus_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='Discuss')$ExplorBonus),color='red',size=1.2)+
  pic_theme


rp5_discuss_1<-ggplot(data=subset(parameter_as_recovered, Group=='Discuss'),aes(x=BetaOthers,y=BetaOthers_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp5_discuss_2<-ggplot(data=subset(parameter_as_recovered, Group=='Discuss'),aes(x=BetaOthers_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='Discuss')$BetaOthers),color='red',size=1.2)+
  pic_theme


rp1_leaderM_1<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderM'),aes(x=Sensitivity,y=Sensitivity_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp1_leaderM_2<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderM'),aes(x=Sensitivity_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='LeaderM')$Sensitivity),color='red',size=1.2)+
  pic_theme


rp2_leaderM_1<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderM'),aes(x=Decay,y=Decay_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp2_leaderM_2<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderM'),aes(x=Decay_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='LeaderM')$Decay),color='red',size=1.2)+
  pic_theme

rp3_leaderM_1<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderM'),aes(x=LearningRate,y=LearningRate_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp3_leaderM_2<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderM'),aes(x=LearningRate_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='LeaderM')$LearningRate),color='red',size=1.2)+
  pic_theme


rp4_leaderM_1<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderM'),aes(x=ExplorBonus,y=ExplorBonus_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp4_leaderM_2<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderM'),aes(x=ExplorBonus_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='LeaderM')$ExplorBonus),color='red',size=1.2)+
  pic_theme


rp5_leaderM_1<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderM'),aes(x=BetaOthers,y=BetaOthers_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp5_leaderM_2<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderM'),aes(x=BetaOthers_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='LeaderM')$BetaOthers),color='red',size=1.2)+
  pic_theme

rp1_mixM_1<-ggplot(data=subset(parameter_as_recovered, Group=='MixM'),aes(x=Sensitivity,y=Sensitivity_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp1_mixM_2<-ggplot(data=subset(parameter_as_recovered, Group=='MixM'),aes(x=Sensitivity_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='MixM')$Sensitivity),color='red',size=1.2)+
  pic_theme


rp2_mixM_1<-ggplot(data=subset(parameter_as_recovered, Group=='MixM'),aes(x=Decay,y=Decay_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp2_mixM_2<-ggplot(data=subset(parameter_as_recovered, Group=='MixM'),aes(x=Decay_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='MixM')$Decay),color='red',size=1.2)+
  pic_theme

rp3_mixM_1<-ggplot(data=subset(parameter_as_recovered, Group=='MixM'),aes(x=LearningRate,y=LearningRate_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp3_mixM_2<-ggplot(data=subset(parameter_as_recovered, Group=='MixM'),aes(x=LearningRate_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='MixM')$LearningRate),color='red',size=1.2)+
  pic_theme


rp4_mixM_1<-ggplot(data=subset(parameter_as_recovered, Group=='MixM'),aes(x=ExplorBonus,y=ExplorBonus_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp4_mixM_2<-ggplot(data=subset(parameter_as_recovered, Group=='MixM'),aes(x=ExplorBonus_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='MixM')$ExplorBonus),color='red',size=1.2)+
  pic_theme


rp5_mixM_1<-ggplot(data=subset(parameter_as_recovered, Group=='MixM'),aes(x=BetaOthers,y=BetaOthers_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp5_mixM_2<-ggplot(data=subset(parameter_as_recovered, Group=='MixM'),aes(x=BetaOthers_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='MixM')$BetaOthers),color='red',size=1.2)+
  pic_theme

rp1_leaderL_1<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderL'),aes(x=Sensitivity,y=Sensitivity_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp1_leaderL_2<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderL'),aes(x=Sensitivity_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='LeaderL')$Sensitivity),color='red',size=1.2)+
  pic_theme


rp2_leaderL_1<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderL'),aes(x=Decay,y=Decay_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp2_leaderL_2<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderL'),aes(x=Decay_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='LeaderL')$Decay),color='red',size=1.2)+
  pic_theme

rp3_leaderL_1<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderL'),aes(x=LearningRate,y=LearningRate_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp3_leaderL_2<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderL'),aes(x=LearningRate_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='LeaderL')$LearningRate),color='red',size=1.2)+
  pic_theme


rp4_leaderL_1<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderL'),aes(x=ExplorBonus,y=ExplorBonus_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp4_leaderL_2<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderL'),aes(x=ExplorBonus_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='LeaderL')$ExplorBonus),color='red',size=1.2)+
  pic_theme


rp5_leaderL_1<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderL'),aes(x=BetaOthers,y=BetaOthers_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp5_leaderL_2<-ggplot(data=subset(parameter_as_recovered, Group=='LeaderL'),aes(x=BetaOthers_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='LeaderL')$BetaOthers),color='red',size=1.2)+
  pic_theme

rp1_mixL_1<-ggplot(data=subset(parameter_as_recovered, Group=='MixL'),aes(x=Sensitivity,y=Sensitivity_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp1_mixL_2<-ggplot(data=subset(parameter_as_recovered, Group=='MixL'),aes(x=Sensitivity_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='MixL')$Sensitivity),color='red',size=1.2)+
  pic_theme


rp2_mixL_1<-ggplot(data=subset(parameter_as_recovered, Group=='MixL'),aes(x=Decay,y=Decay_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp2_mixL_2<-ggplot(data=subset(parameter_as_recovered, Group=='MixL'),aes(x=Decay_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='MixL')$Decay),color='red',size=1.2)+
  pic_theme

rp3_mixL_1<-ggplot(data=subset(parameter_as_recovered, Group=='MixL'),aes(x=LearningRate,y=LearningRate_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp3_mixL_2<-ggplot(data=subset(parameter_as_recovered, Group=='MixL'),aes(x=LearningRate_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='MixL')$LearningRate),color='red',size=1.2)+
  pic_theme


rp4_mixL_1<-ggplot(data=subset(parameter_as_recovered, Group=='MixL'),aes(x=ExplorBonus,y=ExplorBonus_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp4_mixL_2<-ggplot(data=subset(parameter_as_recovered, Group=='MixL'),aes(x=ExplorBonus_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 1)+
  scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,4))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='MixL')$ExplorBonus),color='red',size=1.2)+
  pic_theme


rp5_mixL_1<-ggplot(data=subset(parameter_as_recovered, Group=='MixL'),aes(x=BetaOthers,y=BetaOthers_R))+
  geom_point(size=5,color= "#13A4E5", alpha=0.8)+
  geom_abline(slope=1,intercept = 0,size=1)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  xlab(NULL)+
  ylab(NULL)+
  pic_theme

rp5_mixL_2<-ggplot(data=subset(parameter_as_recovered, Group=='MixL'),aes(x=BetaOthers_R))+
  geom_histogram(fill= "#13A4E5", alpha=0.8,binwidth = 0.05)+
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,0.25))+
  scale_y_continuous(expand=c(0,0))+
  xlab(NULL)+
  ylab(NULL)+
  geom_vline(xintercept =mean(subset(parameter_as_recovered, Group=='MixL')$BetaOthers),color='red',size=1.2)+
  pic_theme

all_rp1<-grid.arrange(rp1_control_1,rp2_control_1,rp3_control_1,rp4_control_1,rp5_control_1,rp1_discuss_1,rp2_discuss_1,rp3_discuss_1,rp4_discuss_1,rp5_discuss_1,rp1_leaderM_1,rp2_leaderM_1,rp3_leaderM_1,rp4_leaderM_1,rp5_leaderM_1,rp1_mixM_1,rp2_mixM_1,rp3_mixM_1,rp4_mixM_1,rp5_mixM_1,rp1_leaderL_1,rp2_leaderL_1,rp3_leaderL_1,rp4_leaderL_1,rp5_leaderL_1,rp1_mixL_1,rp2_mixL_1,rp3_mixL_1,rp4_mixL_1,rp5_mixL_1,ncol=5, widths=rep(10,5))
all_rp2<-grid.arrange(rp1_control_2,rp2_control_2,rp3_control_2,rp4_control_2,rp5_control_2,rp1_discuss_2,rp2_discuss_2,rp3_discuss_2,rp4_discuss_2,rp5_discuss_2,rp1_leaderM_2,rp2_leaderM_2,rp3_leaderM_2,rp4_leaderM_2,rp5_leaderM_2,rp1_mixM_2,rp2_mixM_2,rp3_mixM_2,rp4_mixM_2,rp5_mixM_2,rp1_leaderL_2,rp2_leaderL_2,rp3_leaderL_2,rp4_leaderL_2,rp5_leaderL_2,rp1_mixL_2,rp2_mixL_2,rp3_mixL_2,rp4_mixL_2,rp5_mixL_2,ncol=5)


#
cor.test(parameter_as_recovered$Sensitivity,parameter_as_recovered$Sensitivity_R)
cor.test(parameter_as_recovered$Decay,parameter_as_recovered$Decay_R)
cor.test(parameter_as_recovered$LearningRate,parameter_as_recovered$LearningRate_R)
cor.test(parameter_as_recovered$ExplorBonus,parameter_as_recovered$ExplorBonus_R)
cor.test(parameter_as_recovered$BetaOthers,parameter_as_recovered$BetaOthers_R)