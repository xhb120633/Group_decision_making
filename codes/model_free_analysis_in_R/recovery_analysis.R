##
library(ggplot2)
library(ggpubr)
library(RColorBrewer)
library(R.matlab)
library(reshape2)
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

GeomSplitViolin <- ggproto(
  "GeomSplitViolin", 
  GeomViolin, 
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <- transform(data, 
                      xminv = x - violinwidth * (x - xmin), 
                      xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1,'group']
    newdata <- plyr::arrange(
      transform(data, x = if(grp%%2==1) xminv else xmaxv), 
      if(grp%%2==1) y else -y
    )
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin", 
                       grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    } else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <- function (mapping = NULL, 
                               data = NULL, 
                               stat = "ydensity", 
                               position = "identity", ..., 
                               draw_quantiles = NULL, 
                               trim = TRUE, 
                               scale = "area", 
                               na.rm = FALSE, 
                               show.legend = NA, 
                               inherit.aes = TRUE) {
  layer(data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomSplitViolin, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(trim = trim, 
                      scale = scale, 
                      draw_quantiles = draw_quantiles, 
                      na.rm = na.rm, ...)
  )
}


control_fitting_acc<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\control_fitting_acc.mat')
control_fitting_acc<-as.data.frame(control_fitting_acc)
mean_control_fitting_acc<-data.frame(mean(control_fitting_acc[,1]),sd(control_fitting_acc[,1])/sqrt(dim(control_fitting_acc)[1]),0,0)
control_fitting_acc<-data.frame(control_fitting_acc,rep(0,dim(control_fitting_acc)[1]),rep(0,dim(control_fitting_acc)[1]))
colnames(control_fitting_acc)<-c('Accuracy','Discuss','Leader')
colnames(mean_control_fitting_acc)<-c('Mean','SE','Discuss','Leader')

discuss_fitting_acc<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\discuss_fitting_acc.mat')
discuss_fitting_acc<-as.data.frame(discuss_fitting_acc)
mean_discuss_fitting_acc<-data.frame(mean(discuss_fitting_acc[,1]),sd(discuss_fitting_acc[,1])/sqrt(dim(discuss_fitting_acc)[1]),1,0)
discuss_fitting_acc<-data.frame(discuss_fitting_acc,rep(1,dim(discuss_fitting_acc)[1]),rep(0,dim(discuss_fitting_acc)[1]))
colnames(discuss_fitting_acc)<-c('Accuracy','Discuss','Leader')
colnames(mean_discuss_fitting_acc)<-c('Mean','SE','Discuss','Leader')

leaderM_fitting_acc<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\leaderM_fitting_acc.mat')
leaderM_fitting_acc<-as.data.frame(leaderM_fitting_acc)
mean_leaderM_fitting_acc<-data.frame(mean(leaderM_fitting_acc[,1]),sd(leaderM_fitting_acc[,1])/sqrt(dim(leaderM_fitting_acc)[1]),0,1)
leaderM_fitting_acc<-data.frame(leaderM_fitting_acc,rep(0,dim(leaderM_fitting_acc)[1]),rep(1,dim(leaderM_fitting_acc)[1]))
colnames(leaderM_fitting_acc)<-c('Accuracy','Discuss','Leader')
colnames(mean_leaderM_fitting_acc)<-c('Mean','SE','Discuss','Leader')

leaderL_fitting_acc<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\leader_B_fitting_acc.mat')
leaderL_fitting_acc<-as.data.frame(leaderL_fitting_acc)
mean_leaderL_fitting_acc<-data.frame(mean(leaderL_fitting_acc[,1]),sd(leaderL_fitting_acc[,1])/sqrt(dim(leaderL_fitting_acc)[1]),0,2)
mean_leaderL2_fitting_acc<-data.frame(mean(leaderL_fitting_acc[,2]),sd(leaderL_fitting_acc[,2])/sqrt(dim(leaderL_fitting_acc)[1]),0,2)
leaderL_fitting_acc<-data.frame(leaderL_fitting_acc,rep(0,dim(leaderL_fitting_acc)[1]),rep(2,dim(leaderL_fitting_acc)[1]))
colnames(leaderL_fitting_acc)<-c('Accuracy','Accuracy2','Discuss','Leader')
colnames(mean_leaderL_fitting_acc)<-c('Mean','SE','Discuss','Leader')
colnames(mean_leaderL2_fitting_acc)<-c('Mean','SE','Discuss','Leader')

mixM_fitting_acc<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\mixM_fitting_acc.mat')
mixM_fitting_acc<-as.data.frame(mixM_fitting_acc)
mean_mixM_fitting_acc<-data.frame(mean(mixM_fitting_acc[,1]),sd(mixM_fitting_acc[,1])/sqrt(dim(mixM_fitting_acc)[1]),1,1)
mixM_fitting_acc<-data.frame(mixM_fitting_acc,rep(1,dim(mixM_fitting_acc)[1]),rep(1,dim(mixM_fitting_acc)[1]))
colnames(mixM_fitting_acc)<-c('Accuracy','Discuss','Leader')
colnames(mean_mixM_fitting_acc)<-c('Mean','SE','Discuss','Leader')

mixL_fitting_acc<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\mix_B_fitting_acc.mat')
mixL_fitting_acc<-as.data.frame(mixL_fitting_acc)
mean_mixL_fitting_acc<-data.frame(mean(mixL_fitting_acc[,1]),sd(mixL_fitting_acc[,1])/sqrt(dim(mixL_fitting_acc)[1]),1,2)
mean_mixL2_fitting_acc<-data.frame(mean(mixL_fitting_acc[,2]),sd(mixL_fitting_acc[,2])/sqrt(dim(mixL_fitting_acc)[1]),1,2)
mixL_fitting_acc<-data.frame(mixL_fitting_acc,rep(1,dim(mixL_fitting_acc)[1]),rep(2,dim(mixL_fitting_acc)[1]))
colnames(mixL_fitting_acc)<-c('Accuracy','Accuracy2','Discuss','Leader')
colnames(mean_mixL_fitting_acc)<-c('Mean','SE','Discuss','Leader')
colnames(mean_mixL2_fitting_acc)<-c('Mean','SE','Discuss','Leader')

fitting_acc<-rbind(control_fitting_acc,discuss_fitting_acc,leaderM_fitting_acc,leaderL_fitting_acc[,c(1,3,4)],mixM_fitting_acc,mixL_fitting_acc[,c(1,3,4)])
mean_fitting_acc<-rbind(mean_control_fitting_acc,mean_discuss_fitting_acc,mean_leaderM_fitting_acc,mean_leaderL_fitting_acc,mean_mixM_fitting_acc,mean_mixL_fitting_acc)

fitting_acc$Discuss<-as.factor(fitting_acc$Discuss)
fitting_acc$Leader<-as.factor(fitting_acc$Leader)
mean_fitting_acc$Discuss<-as.factor(mean_fitting_acc$Discuss)
mean_fitting_acc$Leader<-as.factor(mean_fitting_acc$Leader)
v_p$Discuss<-as.factor(v_p$Discuss)
v_p$Leader<-as.factor(v_p$Leader)
mean_v_p$Discuss<-as.factor(mean_v_p$Discuss)
mean_v_p$Leader<-as.factor(mean_v_p$Leader)
g_p$Discuss<-as.factor(g_p$Discuss)
g_p$Leader<-as.factor(g_p$Leader)
mean_g_p$Discuss<-as.factor(mean_g_p$Discuss)
mean_g_p$Leader<-as.factor(mean_g_p$Leader)

recovered_control_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_control_parameter.mat')
recovered_control_parameter<-as.data.frame(recovered_control_parameter)
colnames(recovered_control_parameter)<-colnames(control_parameter)[1:6]
recovered_control_parameter<-melt(recovered_control_parameter[,1:5])
control_parameter_c<-melt(control_parameter[,1:5])
control_parameter_c<-data.frame(control_parameter_c,recovered_control_parameter$value)
colnames(control_parameter_c)=c('Parameter_Type','Fitted_Parameter','Recovered_Parameter')

recovered_discuss_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_discuss_parameter.mat')
recovered_discuss_parameter<-as.data.frame(recovered_discuss_parameter)
colnames(recovered_discuss_parameter)<-colnames(discuss_parameter)[1:6]
recovered_discuss_parameter<-melt(recovered_discuss_parameter[,1:5])
discuss_parameter_c<-melt(discuss_parameter[,1:5])
discuss_parameter_c<-data.frame(discuss_parameter_c,recovered_discuss_parameter$value)
colnames(discuss_parameter_c)=c('Parameter_Type','Fitted_Parameter','Recovered_Parameter')

recovered_leaderM_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_leaderM_parameter.mat')
recovered_leaderM_parameter<-as.data.frame(recovered_leaderM_parameter)
colnames(recovered_leaderM_parameter)<-colnames(leaderM_parameter)[1:6]
recovered_leaderM_parameter<-melt(recovered_leaderM_parameter[,1:5])
leaderM_parameter_c<-melt(leaderM_parameter[,1:5])
leaderM_parameter_c<-data.frame(leaderM_parameter_c,recovered_leaderM_parameter$value)
colnames(leaderM_parameter_c)=c('Parameter_Type','Fitted_Parameter','Recovered_Parameter')

recovered_mixM_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_mixM_parameter.mat')
recovered_mixM_parameter<-as.data.frame(recovered_mixM_parameter)
colnames(recovered_mixM_parameter)<-colnames(mixM_parameter)[1:6]
recovered_mixM_parameter<-melt(recovered_mixM_parameter[,1:5])
mixM_parameter_c<-melt(mixM_parameter[,1:5])
mixM_parameter_c<-data.frame(mixM_parameter_c,recovered_mixM_parameter$value)
colnames(mixM_parameter_c)=c('Parameter_Type','Fitted_Parameter','Recovered_Parameter')

recovered_leaderL_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_leaderL_parameter.mat')
recovered_leaderL_parameter<-as.data.frame(recovered_leaderL_parameter)
colnames(recovered_leaderL_parameter)<-colnames(control_parameter)[1:6]
recovered_leaderL_parameter<-melt(recovered_leaderL_parameter[,1:5])
leaderL_parameter_c<-melt(updated_leaderL_parameter[,1:5])
leaderL_parameter_c<-data.frame(leaderL_parameter_c,recovered_leaderL_parameter$value)
colnames(leaderL_parameter_c)=c('Parameter_Type','Fitted_Parameter','Recovered_Parameter')

recovered_mixL_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\recovered_mixL_parameter.mat')
recovered_mixL_parameter<-as.data.frame(recovered_mixL_parameter)
colnames(recovered_mixL_parameter)<-colnames(control_parameter)[1:6]
recovered_mixL_parameter<-melt(recovered_mixL_parameter[,1:5])
mixL_parameter_c<-melt(updated_mixL_parameter[,1:5])
mixL_parameter_c<-data.frame(mixL_parameter_c,recovered_mixL_parameter$value)
colnames(mixL_parameter_c)=c('Parameter_Type','Fitted_Parameter','Recovered_Parameter')

control_parameter_c$Parameter_Type<-as.factor(control_parameter_c$Parameter_Type)
discuss_parameter_c$Parameter_Type<-as.factor(discuss_parameter_c$Parameter_Type)
leaderM_parameter_c$Parameter_Type<-as.factor(leaderM_parameter_c$Parameter_Type)
leaderL_parameter_c$Parameter_Type<-as.factor(leaderL_parameter_c$Parameter_Type)
mixM_parameter_c$Parameter_Type<-as.factor(mixM_parameter_c$Parameter_Type)
mixL_parameter_c$Parameter_Type<-as.factor(mixL_parameter_c$Parameter_Type)

control_parameter_sigma<-subset(control_parameter_c,Parameter_Type=='Exploration Bonus')
discuss_parameter_sigma<-subset(discuss_parameter_c,Parameter_Type=='Exploration Bonus')
leader_parameter_sigma<-subset(leader_parameter_c,Parameter_Type=='Exploration Bonus')
mix_parameter_sigma<-subset(mix_parameter_c,Parameter_Type=='Exploration Bonus')

control_parameter_c<-subset(control_parameter_c,Parameter_Type!='Exploration Bonus')
discuss_parameter_c<-subset(discuss_parameter_c,Parameter_Type!='Exploration Bonus')
leader_parameter_c<-subset(leader_parameter_c,Parameter_Type!='Exploration Bonus')
mix_parameter_c<-subset(mix_parameter_c,Parameter_Type!='Exploration Bonus')

leader_L1_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\leaderL1_parameter.mat')
mix_L1_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\mixL1_parameter.mat')
leader_L2_M4c_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\leaderL2_M4c_parameter.mat')
mix_L2_M4c_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\mixL2_M4c_parameter.mat')

leader_L1_parameter<-as.data.frame(leader_L1_parameter)
mix_L1_parameter<-as.data.frame(mix_L1_parameter)
leader_L2_M4c_parameter<-as.data.frame(leader_L2_M4c_parameter)
mix_L2_M4c_parameter<-as.data.frame(mix_L2_M4c_parameter)

control_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\control_parameter.mat')
control_parameter<-as.data.frame(control_parameter)
discuss_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\discuss_parameter.mat')
discuss_parameter<-as.data.frame(discuss_parameter)
leader_M_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\leaderM_parameter.mat')
leader_M_parameter<-as.data.frame(leader_M_parameter)
mix_M_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\mixM_parameter.mat')
mix_M_parameter<-as.data.frame(mix_M_parameter)

parameter_l1<-parameter_s
parameter_l2<-parameter_s

parameter_as<-parameter_s

parameter_l1[1:155,1:6]=control_parameter
parameter_l1[156:295,1:6]=discuss_parameter
parameter_l1[296:423,1:6]=leader_M_parameter
parameter_l1[456:567,1:6]=mix_M_parameter

parameter_as[1:155,1:6]=control_parameter
parameter_as[156:295,1:6]=discuss_parameter
parameter_as[296:423,1:6]=leader_M_parameter
parameter_as[456:567,1:6]=mix_M_parameter

parameter_l1[424:455,1:6]=leader_L1_parameter
parameter_l1[568:595,1:6]=mix_L1_parameter
parameter_l2[424:455,1:6]=leader_L2_M4c_parameter
parameter_l2[568:595,1:6]=mix_L2_M4c_parameter

leader_B_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\leader_B_parameter.mat')
leader_B_parameter<-as.data.frame(leader_B_parameter)
leader_B_parameter<-data.frame(leader_B_parameter,parameter_l1[424:455,8],rep(0,dim(leader_B_parameter)[1]))
colnames(leader_B_parameter)<-c('MonetarySensitivity','Decay','LearningRate','ExplorBonus','BetaOther','Consistency','id_list','Recovered')


mix_B_parameter<-readMat('D:\\Hanbo\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\mix_B_parameter.mat')
mix_B_parameter<-as.data.frame(mix_B_parameter)
mix_B_parameter<-data.frame(mix_B_parameter,parameter_l1[568:595,8],rep(0,dim(mix_B_parameter)[1]))
colnames(mix_B_parameter)<-c('MonetarySensitivity','Decay','LearningRate','ExplorBonus','BetaOther','Consistency','id_list','Recovered')

leader_mix_B_parameter<-rbind(leader_B_parameter,mix_B_parameter)

recovered_leader_B_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\leader_B_parameter_recovered.mat')
recovered_leader_B_parameter<-as.data.frame(recovered_leader_B_parameter)
recovered_leader_B_parameter<-data.frame(recovered_leader_B_parameter,parameter_l1[424:455,8],rep(1,dim(recovered_leader_B_parameter)[1]))
colnames(recovered_leader_B_parameter)<-c('MonetarySensitivity','Decay','LearningRate','ExplorBonus','BetaOther','Consistency','SigmaOther','id_list','Recovered')

recovered_mix_B_parameter<-readMat('D:\\IGT_Data\\Matlab\\igt-toolbox-master\\IGTmodelfit\\Parameter result\\mix_B_parameter_recovered.mat')
recovered_mix_B_parameter<-as.data.frame(recovered_mix_B_parameter)
recovered_mix_B_parameter<-data.frame(recovered_mix_B_parameter,parameter_l1[568:595,8],rep(1,dim(recovered_mix_B_parameter)[1]))
colnames(recovered_mix_B_parameter)<-c('MonetarySensitivity','Decay','LearningRate','ExplorBonus','BetaOther','Consistency','SigmaOther','id_list','Recovered')

recovered_leader_B_parameter<-rbind(leader_B_parameter,recovered_leader_B_parameter)
recovered_mix_B_parameter<-rbind(leader_B_parameter,recovered_mix_B_parameter)

recovered_leader_B_parameter<-melt(recovered_leader_B_parameter,id.vars=c('id_list','Recovered'),measure.vars=c(1:7))
recovered_mix_B_parameter<-melt(recovered_mix_B_parameter,id.vars=c('id_list','Recovered'),measure.vars=c(1:7))
colnames(recovered_leader_B_parameter)[c(3,4)]=c('Parameter_type','Value')
colnames(recovered_mix_B_parameter)[c(3,4)]=c('Parameter_type','Value')

recovered_leader_B_parameter$Recovered<-as.factor(recovered_leader_B_parameter$Recovered)
recovered_mix_B_parameter$Recovered<-as.factor(recovered_mix_B_parameter$Recovered)


Figure_4d<-
  ggplot(data = control_parameter_c, aes(x = Fitted_Parameter,y=Recovered_Parameter,group=Parameter_Type,color=Parameter_Type)) +  
  geom_point(alpha=0.6,size=6.5)+
  geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
  scale_x_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  scale_y_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  labs(x='Fitted Parameters',y = "Recovered Parameters")+
  pic_theme+
  theme(legend.title =element_blank(),
        legend.position = c(0.2, 0.90))
  
Figure_4e<-
  ggplot(data = discuss_parameter_c, aes(x = Fitted_Parameter,y=Recovered_Parameter,group=Parameter_Type,color=Parameter_Type)) +  
  geom_point(alpha=0.6,size=6.5)+
  geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
  scale_x_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  scale_y_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  labs(x='Fitted Parameters',y = "Recovered Parameters")+
  pic_theme+
  theme(legend.title =element_blank(),
        legend.position = c(0.2, 0.90))

Figure_4f<-
  ggplot(data = leaderM_parameter_c, aes(x = Fitted_Parameter,y=Recovered_Parameter,group=Parameter_Type,color=Parameter_Type)) +  
  geom_point(alpha=0.6,size=3.5)+
  geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
  scale_x_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  scale_y_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  labs(x='Fitted Parameters',y = "Recovered Parameters")+
  pic_theme+
  theme(legend.title =element_blank(),
        legend.position = c(0.2, 0.90))

Figure_4g<-
  ggplot(data = leader_parameter_c, aes(x = Fitted_Parameter,y=Recovered_Parameter,group=Parameter_Type,color=Parameter_Type)) +  
  geom_point(alpha=0.6,size=6.5)+
  geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
  scale_x_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  scale_y_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  labs(x='Fitted Parameters',y = "Recovered Parameters")+
  pic_theme+
  theme(legend.title =element_blank(),
        legend.position = c(0.2, 0.90))

Figure_4h<-
  ggplot(data = mixM_parameter_c, aes(x = Fitted_Parameter,y=Recovered_Parameter,group=Parameter_Type,color=Parameter_Type)) +  
  geom_point(alpha=0.6,size=3.5)+
  geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
  scale_x_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  scale_y_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  labs(x='Fitted Parameters',y = "Recovered Parameters")+
  pic_theme+
  theme(legend.title =element_blank(),
        legend.position = c(0.2, 0.90))

Figure_4i<-
  ggplot(data = mix_parameter_c, aes(x = Fitted_Parameter,y=Recovered_Parameter,group=Parameter_Type,color=Parameter_Type)) +  
  geom_point(alpha=0.6,size=6.5)+
  geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
  scale_x_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  scale_y_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
  labs(x='Fitted Parameters',y = "Recovered Parameters")+
  pic_theme+
  theme(legend.title =element_blank(),
        legend.position = c(0.2, 0.90))

Figure_4j<-
  ggplot(data = control_parameter_sigma, aes(x = Fitted_Parameter,y=Recovered_Parameter,group=Parameter_Type,color=Parameter_Type)) +  
  geom_point(alpha=0.6,size=6.5)+
  geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
  scale_x_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
  scale_y_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
  labs(x='Fitted Parameters',y = "Recovered Parameters")+
  pic_theme+
  theme(legend.title =element_blank(),
        legend.position = c(0.2, 0.90))

Figure_4k<-
  ggplot(data = discuss_parameter_sigma, aes(x = Fitted_Parameter,y=Recovered_Parameter,group=Parameter_Type,color=Parameter_Type)) +  
  geom_point(alpha=0.6,size=6.5)+
  geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
  scale_x_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
  scale_y_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
  labs(x='Fitted Parameters',y = "Recovered Parameters")+
  pic_theme+
  theme(legend.title =element_blank(),
        legend.position = c(0.2, 0.90))

Figure_4l<-
  ggplot(data = leader_parameter_sigma, aes(x = Fitted_Parameter,y=Recovered_Parameter,group=Parameter_Type,color=Parameter_Type)) +  
  geom_point(alpha=0.6,size=6.5)+
  geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
  scale_x_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
  scale_y_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
  labs(x='Fitted Parameters',y = "Recovered Parameters")+
  pic_theme+
  theme(legend.title =element_blank(),
        legend.position = c(0.2, 0.90))

Figure_4m<-
  ggplot(data = mix_parameter_sigma, aes(x = Fitted_Parameter,y=Recovered_Parameter,group=Parameter_Type,color=Parameter_Type)) +  
  geom_point(alpha=0.6,size=6.5)+
  geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
  scale_x_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
  scale_y_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
  labs(x='Fitted Parameters',y = "Recovered Parameters")+
  pic_theme+
  theme(legend.title =element_blank(),
        legend.position = c(0.2, 0.90))

control_parameter_cv1<-data.frame(control_parameter_c[,c(1,2)],rep(0,155))
control_parameter_cv2<-data.frame(control_parameter_c[,c(1,3)],rep(1,155))
colnames(control_parameter_cv1)<-c('Parameter_Type','Parameter','Recover')
colnames(control_parameter_cv2)<-c('Parameter_Type','Parameter','Recover')
control_parameter_cv<-rbind(control_parameter_cv1,control_parameter_cv2)
control_parameter_cv$Parameter_Type<-as.factor(control_parameter_cv$Parameter_Type)
control_parameter_cv$Recover<-as.factor(control_parameter_cv$Recover)


mse<-summarySE(data=control_parameter_cv,c('Monetary Sensitivity','Decay'),groupvars='Recover',conf.interval = .95, .drop = TRUE)
Figure_4n<-
  ggplot(data = control_parameter_cv,aes(x = Parameter_Type,y=Parameter,fill=Recover)) +  
  geom_split_violin(trim=FALSE,color="white")+theme_cowplot()+xlab('Parameter Type')+
  geom_point(alpha=0.6,size=6.5)+
  geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
  scale_x_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
  scale_y_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
  labs(x='Fitted Parameters',y = "Recovered Parameters")+
  pic_theme+
  theme(legend.title =element_blank(),
        legend.position = c(0.2, 0.90))
  
  
  
  discuss_parameter_cv1<-data.frame(discuss_parameter_c[,c(1,2)],rep(0,140))
  discuss_parameter_cv2<-data.frame(discuss_parameter_c[,c(1,3)],rep(1,140))
  colnames(discuss_parameter_cv1)<-c('Parameter_Type','Parameter','Recover')
  colnames(discuss_parameter_cv2)<-c('Parameter_Type','Parameter','Recover')
  discuss_parameter_cv<-rbind(discuss_parameter_cv1,discuss_parameter_cv2)
  discuss_parameter_cv$Parameter_Type<-as.factor(discuss_parameter_cv$Parameter_Type)
  discuss_parameter_cv$Recover<-as.factor(discuss_parameter_cv$Recover)

  
  leaderM_parameter_cv1<-data.frame(leaderM_parameter_c[,c(1,2)],rep(0,128))
  leaderM_parameter_cv2<-data.frame(leaderM_parameter_c[,c(1,3)],rep(1,128))
  colnames(leaderM_parameter_cv1)<-c('Parameter_Type','Parameter','Recover')
  colnames(leaderM_parameter_cv2)<-c('Parameter_Type','Parameter','Recover')
  leaderM_parameter_cv<-rbind(leaderM_parameter_cv1,leaderM_parameter_cv2)
  leaderM_parameter_cv$Parameter_Type<-as.factor(leaderM_parameter_cv$Parameter_Type)
  leaderM_parameter_cv$Recover<-as.factor(leaderM_parameter_cv$Recover)
  leaderM_parameter_cv<-subset(leaderM_parameter_cv,Parameter_Type!='Exploration Bonus')
  ggplot(data = leaderM_parameter_cv,aes(x = Parameter_Type,y=Parameter,fill=Recover)) +  
    geom_split_violin(trim=FALSE,color="white")
  
  
  leaderL_parameter_cv1<-data.frame(leaderL_parameter_c[,c(1,2)],rep(0,32))
  leaderL_parameter_cv2<-data.frame(leaderL_parameter_c[,c(1,3)],rep(1,32))
  colnames(leaderL_parameter_cv1)<-c('Parameter_Type','Parameter','Recover')
  colnames(leaderL_parameter_cv2)<-c('Parameter_Type','Parameter','Recover')
  leaderL_parameter_cv<-rbind(leaderL_parameter_cv1,leaderL_parameter_cv2)
  leaderL_parameter_cv$Parameter_Type<-as.factor(leaderL_parameter_cv$Parameter_Type)
  leaderL_parameter_cv$Recover<-as.factor(leaderL_parameter_cv$Recover)
  leaderL_parameter_cv<-subset(leaderL_parameter_cv,Parameter_Type!='Exploration Bonus')
  ggplot(data = leaderL_parameter_cv,aes(x = Parameter_Type,y=Parameter,fill=Recover)) +  
    geom_split_violin(trim=FALSE,color="white")
  
  
  mixM_parameter_cv1<-data.frame(mixM_parameter_c[,c(1,2)],rep(0,112))
  mixM_parameter_cv2<-data.frame(mixM_parameter_c[,c(1,3)],rep(1,112))
  colnames(mixM_parameter_cv1)<-c('Parameter_Type','Parameter','Recover')
  colnames(mixM_parameter_cv2)<-c('Parameter_Type','Parameter','Recover')
  mixM_parameter_cv<-rbind(mixM_parameter_cv1,mixM_parameter_cv2)
  mixM_parameter_cv$Parameter_Type<-as.factor(mixM_parameter_cv$Parameter_Type)
  mixM_parameter_cv$Recover<-as.factor(mixM_parameter_cv$Recover)
  mixM_parameter_cv<-subset(mixM_parameter_cv,Parameter_Type!='Exploration Bonus')
  ggplot(data = mixM_parameter_cv,aes(x = Parameter_Type,y=Parameter,fill=Recover)) +  
    geom_split_violin(trim=FALSE,color="white")
  
  mixL_parameter_cv1<-data.frame(mixL_parameter_c[,c(1,2)],rep(0,28))
  mixL_parameter_cv2<-data.frame(mixL_parameter_c[,c(1,3)],rep(1,28))
  colnames(mixL_parameter_cv1)<-c('Parameter_Type','Parameter','Recover')
  colnames(mixL_parameter_cv2)<-c('Parameter_Type','Parameter','Recover')
  mixL_parameter_cv<-rbind(mixL_parameter_cv1,mixL_parameter_cv2)
  mixL_parameter_cv$Parameter_Type<-as.factor(mixL_parameter_cv$Parameter_Type)
  mixL_parameter_cv$Recover<-as.factor(mixL_parameter_cv$Recover)
  mixL_parameter_cv<-subset(mixL_parameter_cv,Parameter_Type!='Exploration Bonus')
  ggplot(data = mixL_parameter_cv,aes(x = Parameter_Type,y=Parameter,fill=Recover)) +  
    geom_split_violin(trim=FALSE,color="white")
  
 
  
   ggplot(data = mixL_M6_parameter,aes(x = Parameter_Type,y=Parameter,fill=Recover)) +  
    geom_split_violin(trim=FALSE,color="white")
  
   ggplot(data = b,aes(x = Parameter_Type,y=Parameter,fill=Recover)) +  
     geom_split_violin(trim=FALSE,color="white")
   
   
   m<-subset(recovered_leader_B_parameter,Parameter_type!='ExplorBonus'&Parameter_type!='Consistency')
   ggplot(data =m,aes(x = Parameter_type,y=Value,fill=Recovered)) +  
     geom_split_violin(trim=FALSE,color="white")+theme_cowplot()+
     xlab('Parameter Type')
   
   m<-subset(recovered_mix_B_parameter,Parameter_type!='ExplorBonus'&Parameter_type!='Consistency')
   ggplot(data =m,aes(x = Parameter_type,y=Value,fill=Recovered)) +  
     geom_split_violin(trim=FALSE,color="white")+theme_cowplot()+
     xlab('Parameter Type')
   
   
   
   
   #control parameter recovery (individual-level)
   m<-subset(mixM_parameter_c,Parameter_Type=='Beta others')
   ggplot(data = m, aes(x = Fitted_Parameter,y=Recovered_Parameter)) +  
     geom_point(alpha=0.6,size=6.5,color='#3366cc')+
     geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
     scale_x_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
     scale_y_continuous( expand=c(0,0),limits=c(0,1),breaks=seq(0, 1, 0.2))+
     labs(x='Beta Others',y = "Recovered Parameters")+
     pic_theme+
     theme(legend.title =element_blank(),
           legend.position = c(0.2, 0.90))
   
   ggplot(data = discuss_parameter_sigma, aes(x = Fitted_Parameter,y=Recovered_Parameter)) +  
     geom_point(alpha=0.6,size=6.5,color='#3366cc')+
     geom_abline(intercept = 0, slope=1,size=0.6,linetype='dashed')+
     scale_x_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
     scale_y_continuous( expand=c(0,0),limits=c(-2,8),breaks=seq(-2, 8, 2))+
     labs(x='Exploration Bonus',y = "Recovered Parameters")+
     pic_theme+
     theme(legend.title =element_blank(),
           legend.position = c(0.2, 0.90))

  s1<-subset(leaderM_parameter_cv,Parameter_Type=='Beta others')
  cor.test(subset(s1,Recover==0)$Parameter,subset(s1,Recover==1)$Parameter)
  
  ####rain clouds plots
  ### This script creates an R function to generate raincloud plots, then simulates
  ### data for plots. If using for your own data, you only need lines 1-80.
  ### It relies largely on code previously written by David Robinson
  ### (https://gist.github.com/dgrtwo/eb7750e74997891d7c20)
  ### and the package ggplot2 by Hadley Wickham
  
  # Check if required packages are installed ----
  packages <- c("cowplot", "readr", "ggplot2", "dplyr", "lavaan", "smooth", "Hmisc")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))
  }
  
  # Load packages ----
  library(ggplot2)
  library(cowplot)
  library(dplyr)
  library(readr)
  
  # Defining the geom_flat_violin function ----
  # Note: the below code modifies the
  # existing github page by removing a parenthesis in line 50
  
  "%||%" <- function(a, b) {
    if (!is.null(a)) a else b
  }
  
  geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                               position = "dodge", trim = TRUE, scale = "area",
                               show.legend = NA, inherit.aes = TRUE, ...) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        trim = trim,
        scale = scale,
        ...
      )
    )
  }
  
  #' @rdname ggplot2-ggproto
  #' @format NULL
  #' @usage NULL
  #' @export
  GeomFlatViolin <-
    ggproto("GeomFlatViolin", Geom,
            setup_data = function(data, params) {
              data$width <- data$width %||%
                params$width %||% (resolution(data$x, FALSE) * 0.9)
              
              # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
              data %>%
                group_by(group) %>%
                mutate(
                  ymin = min(y),
                  ymax = max(y),
                  xmin = x,
                  xmax = x + width / 2
                )
            },
            
            draw_group = function(data, panel_scales, coord) {
              # Find the points for the line to go all the way around
              data <- transform(data,
                                xminv = x,
                                xmaxv = x + violinwidth * (xmax - x)
              )
              
              # Make sure it's sorted properly to draw the outline
              newdata <- rbind(
                plyr::arrange(transform(data, x = xminv), y),
                plyr::arrange(transform(data, x = xmaxv), -y)
              )
              
              # Close the polygon: set first and last point the same
              # Needed for coord_polar and such
              newdata <- rbind(newdata, newdata[1, ])
              
              ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
            },
            
            draw_key = draw_key_polygon,
            
            default_aes = aes(
              weight = 1, colour = "grey20", fill = "white", size = 0.5,
              alpha = NA, linetype = "solid"
            ),
            
            required_aes = c("x", "y")
    )
  
  m<-control_parameter[,1:6]
  m<-data.frame(m,rep(0,dim(m)[1]),c(1:dim(m)[1]))
  colnames(m)[c(7,8)]=c('Recovered','id')
  n<-data.frame(recovered_control_parameter,rep(1,dim(recovered_control_parameter)[1]),c(1:dim(n)[1]))
  colnames(n)[c(7,8)]=c('Recovered','id')
  adjusted_m<-m
  adjusted_m$Recovered<-as.factor(as.numeric(adjusted_m$Recovered)+0.2)
  adjusted_n<-n
  adjusted_n$Recovered<-as.factor(as.numeric(adjusted_n$Recovered)-0.2)
  m<-rbind(m,n)
  m$Recovered<-as.factor(m$Recovered)
  adjusted_m<-rbind(adjusted_m,adjusted_n)
  colnames(adjusted_m)[7]=c('Recovery')
  adjusted_m$Recovery<-as.numeric(adjusted_m$Recovery)
 
  
  
  mse<-summarySE(data=m,'Monetary.Sensitivity',groupvars='Recovered',conf.interval = .95, .drop = TRUE)
  ggplot(data=m,aes(x=Recovered,y=Monetary.Sensitivity))+
    geom_flat_violin(data=m,aes(fill = Recovered),position = position_nudge(x = .1, y = 0), adjust = 1.5, trim = FALSE, alpha = .5, colour = NA)+
    geom_point(data=m,aes(x = Recovered, y = Monetary.Sensitivity, colour =Recovered),position = position_jitter(width = .05), size = 3, shape = 20)+
    #geom_boxplot(data=m,aes(x = Recovered, y = Monetary.Sensitivity, colour =Recovered,fill=Recovered),outlier.shape = NA, alpha = .5, width = .1, colour = "black")+
    #geom_line(data = adjusted_m, aes(x = Recovery, y = Monetary.Sensitivity,group=id), linetype = 1,size=1,alpha=0.5,colour='gray')+guides(color=FALSE)+
    #geom_point(data = mse, aes(x = Recovered, y = Monetary.Sensitivity), shape = 18,size=2.5) +
    #geom_errorbar(data = mse, aes(x = Recovered, y = Monetary.Sensitivity,ymin = Monetary.Sensitivity-se, ymax = Monetary.Sensitivity+se), width = .05,size=1)+
    scale_colour_brewer(palette = "Dark2")+
    scale_fill_brewer(palette = "Dark2")+theme_cowplot()+
    ggtitle("Figure 11: Repeated Measures - Factorial (Extended)")
  