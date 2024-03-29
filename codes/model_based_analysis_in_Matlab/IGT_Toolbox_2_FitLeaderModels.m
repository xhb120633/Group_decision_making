%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                             IGT TOOLBOX                                 %
%  Once the data has been formatted correctly, this script is the only    %
%  one which need to be launched in order to fit reinforcement-learning   %
%  models to IGT data. All steps and options are extensively commented.   %
%                                                                         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% v1.1
% Romain Ligneul. 
% 11/2018. romain.ligneul@gmail.com
clear all;close all; 

%% General settings
% In theory, this section of the script is the only one that users
% unexperienced with computational modeling will modify.

% 1) Models5 that the toolbox is going to fit to your data
 %A.fit.models ={@run_L_M3,@run_L_M3b,@run_L_M3c,@run_L_M3d,@run_L_M3e,@run_L_M3f,@run_L_M4,@run_L_M4b,@run_L_M4c,@run_L_M4d,@run_L_M4e,@run_L_M4f};%{@run_L_VSE,@run_L_VSE_both,@run_L_VSE_decay,@run_L_VSE_explor,@run_L_M2,@run_L_M2b,@run_L_M2c,@run_L_M2d,@run_L_M3,@run_L_M3b,@run_L_M3c,@run_L_M3d,@run_L_M3e,@run_L_M4,@run_L_M4b,@run_L_M4c,@run_L_M4d,@run_L_M4e,@run_L_M5,@run_L_M5b,@run_L_M5c,@run_L_M6,@run_L_M6b};%{@run_B_M3e,@run_B_M3f,@run_B_M3g,@run_B_M4,@run_B_M4c,@run_B_M4d,@run_B_M4e,@run_B_M4f,@run_B_M5,@run_B_M5b,@run_B_M5c,@run_B_M6,@run_B_M6b};%{@run_B_VSE_decay,@run_B_VSE_explor,@run_B_VSE_ed};%{@run_B_M2b,@run_B_M2,@run_B_M2c,@run_B_M2d};%{@run_M6d,@run_M6e,@run_M3f,@run_M4b,@run_M4c,@run_M4d};%{@run_B_M4c, @run_B_M3e, @run_B_M3c,@run_B_M3d,  @run_B_M3,@run_B_M3b,@run_B_M2,@run_B_M2b @run_B_VSE};%{@run_B_M7b, @run_B_M7,@run_B_M6,@run_B_M6b,  @run_B_M5, @run_B_M5b, @run_B_M4,@run_B_M4b};%{@run_M5ci};%{@run_M2d,@run_M3e,@run_M5cc};%{@run_M5cL};%@run_M5c,@run_M5d,@run_M3c,@run_M3d,@run_VSE,@run_M2,@run_M2b,@run_M2c,@run_M3,@run_M3d,@run_M4,@run_M5,@run_M6};%, @run_PVL, @run_PVLdelta, @run_VPP, @run_VSE_LA, @run_ORL, @run_EV};
 A.fit.models ={@run_L_M6};
% 2) Types of priors to be used: 'informed','flat' or 'shrinkage'
% 'informed' priors are based on the distributions of parameters obtained
% from a open dataset of 504 subjects. 
% 'flat' priors correspond to approximate uniform distributions
% 'shrinkage' priors correspond to priors of mean 0 and variance 1.
% Although is it good practice to compare the results obtained with each
% method, 'informed' priors will diminish the risk of aberrant fit.
A.fit.priors.type = 'flat';

% 3) Should the toolbox perform Bayesian Model Comparison in order to
% determine which model fits best your data at the end of the analysis
% yes = 1 / no = 0;
A.comparison.do = 1;

% 6) Output_name (determines directory in which output of the fit is
% written
A.output_name = 'toydata';

% 6) Should the toolbox save all the information (can be >100mb per model)
% or just a summary? yes = 1 / no = 0
A.complete_save = 1;

% 7) Should the toolbox also simulate the models and perform parameter
% recovery? The recovery strategy consist in simulating all subjects based
% on the estimated parameters and check whether the algorithm  retrieve
% similar values. 
% estimate = 0, estimate & recover = 1
A.simulate_and_recover = 0;

% 8) Should program use grid-computing? (qsub method / linux)
A.cluster_run = 0;

%% Data to be used
% in order to regenerate the dataset of the 504 subjects open dataset,
% indicate: load('IGTdata/opendata_1/opendata504subjects.mat');
% otherwise you can easily use your own dataset, provided that it is 
% formatted like this one. In the study-specific data folder, you can find
% example of conversion scripts: 
% - Ahn 2014 was converted from 1 text file per subject group.
% - Bravers 2014 was converted from 1 xls file per subject group.
% - opendata504 was converted from 1 big xls file configured differently
% If you have different groups define carefully data{}.cond and
% data{}.cond_label, as those will be used automatically at the end of the
% analysis. 
load('IGTdata/IGTdata.mat');

%% Specific settings
% These settings offer more flexibility in the analysis.

% 1) Maximal number of subject on which the fit should be performed. Can be
% useful to use a small number (e.g 5) to check if everything works fine.
A.fit.maxsubjects =295;

% 2) Number of trials in the version of the IGT tested. The standard
% version of the IGT has 100 trials, but some modified versions have 150 or
% 200.
A.fit.ntrials = 100;

% 3) It is common practice to divide feedbacks by 100 before fitting the
% IGT data. All results in Ligneul & Sescousse 2018 have been generated
% using such transform.
A.fit.divide_feeback = 100;

% 4) If you don't want to use the VBA toolbox or if you want to
% double-check your main results using a simpler gradient descent
% algorithm, you can set the option below to 1. In this case, the Matlab
% fminunc will be used (see Tools/OTHERS/VBA_fminunc_wrapper.m for more
% details). You can define the number of random initialization points (i.e
% initial parameters drawn randomly from their prior distributions) with
% the following line (nb. this parameter has a dramatic impact on
% computational time).
A.fit.fminunc =0;
A.fit.options.fminunc_n_init_random = 20; 

%% options for the vba toolbox
% See VBA documentation for more information about the following options.
A.fit.options.updateX0 = 0; 
A.fit.options.binomial = 2;
A.fit.options.DisplayWin = 0; 
A.fit.options.MaxIter = 100;
A.fit.options.GnMaxIter = 500;
A.fit.options.TolFun  = 1e-4;

%% Add dependencies to Matlab path
% Model fitting and model comparison are performed using the VBA package
% described in Rigoux & Daunizeau, Plos Comp. Biol. 2014.
% https://mbb-team.grun_EVithub.io/VBA-toolbox/
% Most technical questions concerning the model fitting and model
% comparison proceTsigMin5to5dures can be solved by consulting the VBA website and the
% manual of the VBA toolbox.
% Models themselves are contained in decidated folders, and consist in a
% parametrized launcher script as well as an evolution and an observation
% function.
addpath(genpath([pwd '/Tools/VBA/']));
addpath(genpath([pwd '/Tools/Formal_Models/']));
addpath(genpath([pwd '/Tools/OTHERS/']));
% manage directories
A.main_path = pwd;
A.output.dir = [A.main_path '/IGTmodelfit/' A.output_name '/'];
mkdir(A.output.dir);

%% load data and build the input structure
% The VBA toolbox uses an input structure to give information about the
% task. One particularity of the VBA toolbox is that the evolution of
% hidden states precedes the observation made by the experimenter (e.g
% choice). Therefore, one needs to define a 'null trial' in the first
% column of the u structure. 

% Additional rows can be defined if necessary.
for s = 1:length(data)
    
    % subject info (numeric or string)
    try
        A.fit.subject(s,1) = data{s}.id;
    catch % if id is string
        A.fit.subject{s,1} = data{s}.id;
    end
    % if available, report condition
    try
        A.fit.condition(s,1) = data{s}.cond;
    end
    %  inputs
    A.fit.u{s}(1,:) = 0:A.fit.ntrials-1;                                    % indices of the trials where evolution function should run.
    A.fit.u{s}(2,:) = [0; data{s}.deck(1:end-1)];                           % previous choice
    A.fit.u{s}(3,:) = [0; data{s}.win(1:end-1)]/A.fit.divide_feeback;       % previous win
    A.fit.u{s}(4,:) = abs([0; data{s}.lose(1:end-1)]/A.fit.divide_feeback); % previous lose
    A.fit.u{s}(5,:) = 1:A.fit.ntrials;                                      % indices of the trials where observation function should run.
    A.fit.u{s}(6,:) = data{s}.s_decision;
    A.fit.u{s}(7,:) = data{s}.deck;
    A.fit.u{s}(8:11,:) =data{s}.social_number';
    %A.fit.u{s}(8:11,:) =[zeros(4,1),data{s}.social_number(1:end-1,:)'];
    A.fit.u{s}(12,:) = [0; data{s}.s_decision(1:end-1)];   
    A.fit.u{s}(13:16,:) =[zeros(4,1),data{s}.social_number(1:end-1,:)'];
    %A.fit.u{s}(13,:) = data{s}.confidence1;
    %A.fit.u{s}(13:16,:) =data{s}.social_number';
    % multinomial observation vector
    %A.fit.y{s} = zeros(4,length(A.fit.u{s}));
    A.fit.y{s} = zeros(8,length(A.fit.u{s}));
     for t = 1:length(A.fit.u{s})
         A.fit.y{s}(A.fit.u{s}(6,t),t) = 1;
         A.fit.y{s}(A.fit.u{s}(7,t)+4,t) = 1;
     end
%     
end

%% run the fitting procedure
% cluster output will only works if you have the qsub functions provided by
%fieldtrip and a torque-compatible grid computing architecture.
cluster_ouput = 'torque_output/';
mkdir(cluster_ouput);
return_dir = pwd;
for m = 1:length(A.fit.models)
   if A.cluster_run ==1
       try cd(cluster_ouput);end
       qsubfeval(A.fit.models{m}, A, data, 'memreq', 4*(1024^3), 'timreq', 600*60);
   else
       A.fit.models{m}(A,data);
   end
end
cd(return_dir);

%shut down the computer after completing the code (do not use it unless you
%have much work to do here and do not want to wait until it is completed)
%system('shutdown -s -t 60');