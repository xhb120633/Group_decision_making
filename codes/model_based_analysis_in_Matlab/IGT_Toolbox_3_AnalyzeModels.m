%%%%%%%%%%%%%%%%%%%%%%% IGTtoolbox Results - Modeling %%%%%%%%%%%%%%%%%%%%%
% This script outputs all the behavioral measures which are dependent of
% computational modeling (e.g best fitting parameters, recovery analyses,
% model comparison, etc.).
% It computes automatically the statistics according to the type of
% analysis performed:
% - takes into account the independent vs dependent nature of your observations
% - takes into account the number of groups/conditions to decide for ex. if
% t-tests or ANOVAs should be run
% - tests whether your data is normally distributed using a lillietest and
% output both parametric and nonparametric statistics in any case.
% It also plots everything (plot2svg can be used to obtain vectorial images).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Romain Ligneul / romain.ligneul@gmail.com / August 2018.
% v1.1

% clean the environment
clear all;close all;clc;

% ask user for directory (what should be analyzed)
model_dir = [uigetdir('IGTmodelfit', 'Select folder containing the models of interest') '/'];

% get all models
dummy_list = dir(model_dir)
for d = 1:length(dummy_list); model_list{d,1} = dummy_list(d).name; end; model_list(1:2)=[];

% select models to use for the comparison
model_selection = listdlg('PromptString', 'Select the models you want to compare:', ...
    'ListString', model_list, 'ListSize', [300 200], 'InitialValue', 1:length(model_list));
model_list = model_list(model_selection);

% load relevant dataset (matching its folder name in IGTdata/)
data_dir = [uigetdir('IGTdata', 'Select folder corresponding to the modeling analysis') '/'];
load([data_dir 'IGTdata.mat']);

% initialize stuff
model_short_name = [];
keep_param = [];
keep_param_names = [];
keep_model_names = [];
keep_conditions = [];

% start looping over models list and logging info
for m = 1:size(model_list,1)
    disp(['model ' num2str(m) ' = ' model_list(m)]);
    
    % log subject id and condition id
    if m == 1
        for s = 1:length(data)
            subj_id(s,1) = s;
            cond_id(s,1) = double(data{s}.cond);
            try
                cond_label{s,1} = data{s}.cond_label;
            catch
                cond_label{s,1} = num2str(cond_id(s,1));
            end
        end
    end
    
    % make short name
    underscore_ind = strfind(model_list{m}, '_');
    model_short_name{m} = model_list{m}(underscore_ind(1)+1:underscore_ind(2)-1);
    
    % load model
    load([model_dir model_list{m} '/fitted_model.mat'],'R', 'A');
    
    % get Goodness of fit metrics
    AIC(m, :) = R.GoF(:,3);
    F(m, :) = R.GoF(:,1);
    BIC(m, :) = R.GoF(:,2);
    LL(m, :) = R.GoF(:,4);
    
    if  exist('R.phi','var')==1

    % keep all parameters together, separate models by nan columns
    keep_param = [keep_param R.theta R.phi nan(size(R.theta,1),1)];
    keep_param_names = [keep_param_names repmat([A.fit.options.inF.param_name A.fit.options.inG.param_name {'NULL'}], size(R.theta,1),1)];
    keep_model_names = [keep_model_names repmat(model_short_name(m), size(R.theta,1),size(R.theta,2)+size(R.phi,2)) repmat({'NULL'},size(R.theta,1),1)];
    else
    keep_param = [keep_param R.theta  nan(size(R.theta,1),1)];
    keep_param_names = [keep_param_names repmat([A.fit.options.inF.param_name A.fit.options.inG.param_name {'NULL'}], size(R.theta,1),1)];
    keep_model_names = [keep_model_names repmat(model_short_name(m), size(R.theta,1),size(R.theta,2)) repmat({'NULL'},size(R.theta,1),1)]; 
    end
    % test effects on parameters
    if m==1 % we don't need to repeat this for each model
        % test effects on parameters
        [condition_list, ia] = unique(cond_label);
        condition_number = cond_id(ia);
        % ask user to select conditions to use for statistical inference
        condition_selection = listdlg('PromptString', 'Select the conditions you want to include in the analysis:', ...
            'ListString', condition_list, 'ListSize', [300 200]);
        condition_ids = cellstr(char(condition_list(condition_selection)));
        condition_number = condition_number(condition_selection);
        % ask user to define test type
        dumstr = questdlg('Are the conditions reflecting repeated measures or independent observation units?', 'Test type', 'Repeated', 'Independent', 'Independent');
        test_type = strmatch(dumstr, {'Repeated', 'Independent'});
        % ask user to chose what to plot
        dumstr = questdlg('Plot all or just selected condtions/groups?', 'Plot type', 'All', 'Selected', 'Selected');
        plot_type = strmatch(dumstr, {'All', 'Selected'});
        
        %         condition_list = cellstr(num2str(unique(A.fit.condition)));
        %         % select conditions to use for the comparison (and plot)
        %         condition_selection = listdlg('PromptString', 'Select the conditions you want to include in the analysis:', ...
        %             'ListString', condition_list, 'ListSize', [300 200]);
        %         condition_ids = str2num(char(condition_list(condition_selection)));
        %         dumstr = questdlg('Are the conditions reflecting repeated measures or independent observation units?', 'Test type', 'Repeated', 'Independent', 'Independent');
        %         test_type = strmatch(dumstr, {'Repeated', 'Independent'});
    end
    if  exist('R.phi','var')==1 
        keep_conditions = [keep_conditions repmat(A.fit.condition, 1,size(R.theta,2)+size(R.phi,2)+1)];
    else
        keep_conditions = [keep_conditions repmat(A.fit.condition, 1,size(R.theta,2)+1)];
    end

    % parameter analysis / evolution
    for t = 1:size(R.theta,2)
        stats_theta{m,t}.param_name = A.fit.options.inF.param_name{t};
        stats_theta{m,t}.model_name = model_list{m};
        try
            stats_theta{m,t}.recovery_corr = corr(R.theta(:,t), R.simulation.theta_recovered(:,t),'type', 'spearman');
        end
        gmat = [];ymat = [];
        for c = 1:length(condition_selection);
            test_mat{c} = R.theta(A.fit.condition==condition_number(c), t);
            stats_theta{m,t}.normality(c) = 1-lillietest(test_mat{c});
            gmat = [gmat; c*ones(sum(A.fit.condition==condition_number(c)),1)];
            ymat = [ymat; test_mat{c}];
        end
        stats_theta{m,t}.means = meanbycond(ymat, gmat,[]);
        stats_theta{m,t}.stds = stdbycond(ymat, gmat,[]);

        if length(test_mat)==2
            if test_type==2;
                [stats_theta{m,t}.nonparam_p, ~, stats_theta{m,t}.nonparam_stats] = ranksum(test_mat{1}, test_mat{2});
                [~, stats_theta{m,t}.param_p, stats_theta{m,t}.param_stats] = ttest2(test_mat{1}, test_mat{2});
                stats_theta{m,t}.nonparam_test = 'Wilcoxon unpaired';
                stats_theta{m,t}.param_test = 't-test unpaired';   
            else
                                [stats_theta{m,t}.nonparam_p, ~, stats_theta{m,t}.nonparam_stats] = signrank(test_mat{1}, test_mat{2});
                [~, stats_theta{m,t}.param_p, stats_theta{m,t}.param_stats] = ttest(test_mat{1}, test_mat{2});
                stats_theta{m,t}.nonparam_test = 'Wilcoxon paired';
                stats_theta{m,t}.param_test = 't-test paired';   
                   
            end
        else
            if test_type==2;
           [stats_theta{m,t}.param_p, stats_theta{m,t}.param_table, stats_theta{m,t}.stats] = anova1(ymat, gmat, 'off');                
                [stats_theta{m,t}.nonparam_p, stats_theta{m,t}.nonparam_table, stats_theta{m,t}.nonparam_stats] = kruskalwallis(ymat, gmat, 'off');   
                stats_theta{m,t}.nonparam_test = 'kruskalwallis';
                stats_theta{m,t}.param_test = 'anova1';  
            else
                [stats_theta{m,t}.param_p, stats_theta{m,t}.param_table] = anova_rm(cell2mat(test_mat), 'off');                
                [stats_theta{m,t}.nonparam_p, stats_theta{m,t}.nonparam_table, stats_theta{m,t}.nonparam_stats] = friedman(cell2mat(test_mat));  
                stats_theta{m,t}.nonparam_test = 'kruskalwallis';
                stats_theta{m,t}.param_test = 'anova1';   
                
            end
        end
        try
            theta_fitsim{m}{t}(:,1) = R.theta(:,t);
            theta_fitsim{m}{t}(:,2) = R.simulation.theta_recovered(:,t);
        catch
            warning('It seems that theta parameters recovered from simulation are lacking')
        end
    end
        
    % parameter analysis / observation
  if  exist('R.phi','var')==1 
    for p = 1:size(R.phi,2)
        % try to compute correlation between recovered and fitted parameters
        stats_phi{m,p}.name = A.fit.options.inG.param_name{p};
        stats_phi{m,p}.model_name = model_list{m};
        try
            stats_phi{m,p}.recovery_corr = corr(R.phi(:,p), R.simulation.phi_recovered(:,p),'type', 'spearman');
        end
        gmat = [];ymat = [];
        for c = 1:length(condition_selection);
            test_mat{c} = R.phi(A.fit.condition==condition_number(c), p);
            stats_phi{m,p}.normality(c) = 1-lillietest(test_mat{c});
            gmat = [gmat; c*ones(sum(A.fit.condition==condition_number(c)),1)];
            ymat = [ymat; test_mat{c}];
        end
        stats_phi{m,p}.means = meanbycond(ymat, gmat,[]);
        stats_phi{m,p}.stds = stdbycond(ymat, gmat,[]);

        if length(test_mat)==2
            if test_type==2;
                [stats_phi{m,p}.nonparam_p, ~, stats_phi{m,p}.nonparam_stats] = ranksum(test_mat{1}, test_mat{2});
                [~, stats_phi{m,p}.param_p, stats_phi{m,p}.param_stats] = ttest2(test_mat{1}, test_mat{2});
                stats_phi{m,p}.nonparam_test = 'Wilcoxon unpaired';
                stats_phi{m,p}.param_test = 't-test unpaired';   
            else
             [stats_phi{m,p}.nonparam_p, ~, stats_phi{m,p}.nonparam_stats] = signrank(test_mat{1}, test_mat{2});
                [~, stats_phi{m,p}.param_p, stats_phi{m,p}.param_stats] = ttest(test_mat{1}, test_mat{2});
                stats_phi{m,p}.nonparam_test = 'Wilcoxon paired';
                stats_phi{m,p}.param_test = 't-test paired';
            end
        else
            if test_type==2;
                     [stats_phi{m,p}.param_p, stats_phi{m,p}.param_table, stats_phi{m,p}.stats] = anova1(ymat, gmat, 'off');                
                [stats_phi{m,p}.nonparam_p, stats_phi{m,p}.nonparam_table, stats_phi{m,p}.nonparam_stats] = kruskalwallis(ymat, gmat, 'off');   
                stats_phi{m,p}.nonparam_test = 'kruskalwallis';
                stats_phi{m,p}.param_test = 'anova1';
                 
            else
                [stats_phi{m,p}.param_p, stats_phi{m,p}.param_table] = anova_rm(cell2mat(test_mat), 'off');                
                [stats_phi{m,p}.nonparam_p, stats_phi{m,p}.nonparam_table, stats_phi{m,p}.nonparam_stats] = friedman(cell2mat(test_mat));  
                stats_phi{m,p}.nonparam_test = 'kruskalwallis';
                stats_phi{m,p}.param_test = 'anova1';   
            end
        end
        try
            phi_fitsim{m}{p}(:,1) = R.phi(:,p);
            phi_fitsim{m}{p}(:,2) = R.simulation.phi_recovered(:,p);
        catch
            warning('It seems that phi parameters recovered from simulation are lacking')
        end        
    end
  end
    
    % compute the correlation matrix accross the entire space of models and
    % parameters
    if  exist('R.phi','var')==1
        cross_corr{m,1} = corr([R.theta R.phi],'type', 'Spearman');
    else
        cross_corr{m,1} = corr([R.theta],'type', 'Spearman');
    end
        
    % log DE events in the real data and in the choice predicted by the
    % model (first-pass)
    % compute directed exploration indexes
    for s = 1:length(data)
        for t = 1:97
            DE3{m}.data(s,t) = double(numel(unique(data{s}.deck(t:t+2)))==3);
            DE3{m}.model(s,t) = double(numel(unique(R.predicted_choices(s,t:t+2)))==3);
            if t<98
                DE4{m}.data(s,t) = double(numel(unique(data{s}.deck(t:t+3)))==4);
                DE4{m}.model(s,t) = double(numel(unique(R.predicted_choices(s,t:t+3)))==4);
            end
        end
    end
    
    % classification based on parameters' values only if two groups are
    % defined
    if length(condition_number)==2
        pop1=A.fit.condition==condition_number(1);
        pop2=(A.fit.condition==condition_number(2));
        pop=pop1+pop2*2;
        classifparams=[R.theta R.phi];
        classifparams(pop==0,:)=[];
        pop(pop==0)=[];
        classification.all{m} = VBA_classification(classifparams,pop-1,size(classifparams,1));
        classification.pa(m,1) = classification.all{m}.stat.pa;
        classification.bpa(m,1) = classification.all{m}.stat.bpa;        
    end
    
    % compute greedy accuracy of fitted and simulated predictions
    accmat_fitted{m} = double(R.predicted_choices==R.actual_choices);
    accmat_fitted{m}(isnan(R.predicted_choices))=NaN;
    try
        accmat_simul{m}  = double(R.simulation.simulated_choices==R.actual_choices);
        accmat_simul{m}(isnan(R.simulation.predicted_choices))=NaN; 
    end
    
end

figure('Name', 'Cross correlation of all parameters');
cross_allparam = corr(keep_param,'type','Spearman');
imagesc(cross_allparam);
set(gcf, 'color', 'w');
set(gca, 'xtick', [2 7 12.5 17.5 24.5], 'xticklabel', model_short_name)
set(gca, 'ytick',1:size(keep_param_names,2), 'yticklabel', keep_param_names(1,:)')

figure('Name', 'Greedy prediction accuracy per model');
% colormap gray
acc_mean=[];
acc_sem = [];
for m=1:length(model_list)
    try
         % all param.
         acc_mean = [acc_mean; nanmean(nanmean(accmat_fitted{m},2)) nanmean(nanmean(accmat_simul{m},2))];
         acc_sem = [acc_sem; nanstd(nanmean(accmat_fitted{m},2))/sqrt(size(accmat_fitted{m},1)) nanstd(nanmean(accmat_simul{m},2))/sqrt(size(accmat_simul{m},1)) ];
    catch
         % all param.
         acc_mean = [acc_mean; nanmean(nanmean(accmat_fitted{m},2))];
         acc_sem = [acc_sem; nanstd(nanmean(accmat_fitted{m},2))/sqrt(size(accmat_fitted{m},1))];
    end
end;
subplot(1,2,1)
bar(acc_mean(:,1));
hold on
errorbar(acc_mean(:,1), acc_sem(:,1), '.k');
hold off
title('fitted choices')
set(gca, 'xticklabel', model_short_name)
xlim([0 length(model_list)+1])
ylim([0.2 0.9])
try
    subplot(1,2,2)
    bar(acc_mean(:,2));
    hold on
    errorbar(acc_mean(:,2), acc_sem(:,2), '.k');
    hold off
    title('simulated choices')
    set(gca, 'xticklabel', model_short_name)
    xlim([0 length(model_list)+1]);
    ylim([0.2 0.7])
end
set(gcf, 'color', 'w')

figure('Name', 'Goodness of fit against EE / All metrics (');
dumcheck = strfind(model_list,'VSE'); for d=1:length(dumcheck);if ~isempty(dumcheck{d});EE_ind=d;break;end;end
diff_metrics = [-2*sum(BIC,2)-(-2*sum(BIC(EE_ind,:),2)), -2*sum(AIC,2)-(-2*sum(AIC(EE_ind,:),2)) -2*sum(F,2)-(-2*sum(F(EE_ind,:),2))];
metrics = [-2*sum(BIC,2), -2*sum(AIC,2) -2*sum(F,2)];
bar(diff_metrics);
legend('BIC','AIC','Free Energy')
set(gca,'xticklabels', model_short_name)
set(gcf,'unit','centimeters','position',[0 0 10 10])
set(gcf, 'color', 'w');

% Metrics used for Bayesian Model Comparison:
% One can choose between 'Free Energy' (which takes into account the
% uncertainty about parameter estimates), 'BIC' (which include a strong
% penalty term for model complexity), 'AIC' (which penalize more loosely
% for model complexity), 'NLL' (which does not penalize for model
% complexity). Note that those options are not mutually exclusive.
options.figName = 'Free energy metric';
options.modelNames = model_short_name;
VBA_groupBMC(F,options);
options.figName = 'AIC metric';
options.modelNames = model_short_name;
VBA_groupBMC(AIC,options);
options.figName = 'BIC metric';
options.modelNames = model_short_name;
VBA_groupBMC(BIC,options);

% ensuring that the EE model captures directed exploration events (based on the 
% DE indexes) better than other models
% streaks of 4
figure('Name','Reproducing DE index events', 'color', 'w')
h1 = subplot(1,3,1)
plot(nansum(DE4{m}.data))
hold on
for m=1:length(DE4)
    % plot as a function of time
    h_keep(m) = plot(nansum(DE4{m}.model))
    hold on
    % compute overall d-prime of the model
    Hrate = mean2(DE4{m}.model(DE4{m}.data==1)==1);
    FArate = mean2(DE4{m}.model(DE4{m}.data==0)==1);
    DE4_dprime_global(m,1) = dprime_simple(Hrate,FArate);
    sum_global(m,1) = nansum(nansum(DE4{m}.model));
end
hold off
% streaks of
[h] = legend(['data', model_short_name])
ylim([0 100])
title('DE4 model-data')
h2 = subplot(1,3,3)
bplot = bar(DE4_dprime_global','facecolor', [0.5 0.5 0.5]);
xlim([0 length(['data', model_short_name])])
title('sensitivity to DE events (d'')')
set(gca,'xticklabels',[model_short_name'])
% streaks of
h2 = subplot(1,3,2)
bplot = bar([nansum(nansum(DE4{m}.data)); sum_global],'facecolor', [0.5 0.5 0.5]);
xlim([0 size(['data', model_short_name],2)+1])
title('number of DE events')
set(gca,'xticklabels',['data'; model_short_name'])

% Overview of all parameters splitted by group or condition, for all
% selected model
for m=1:length(model_list)
    figure('name', ['Complete overview of parameters for ' model_short_name{m}], 'units','normalized', 'position', [-0.10+m*0.15 0.05 0.45 0.45])
    clear g;
    selector = strcmp(keep_model_names, model_short_name{m});
    y = keep_param(selector);
    x = keep_param_names(selector);
    color = repmat(cond_label, 1, size(selector,2));
    color = color(selector);
    subset = ismember(color, condition_ids);
    facet = keep_model_names(selector);
    if plot_type==1
        g = gramm('x',x(:), 'y', y(:), 'color', color(:));
    else
        g = gramm('x',x(:), 'y', y(:), 'color', color(:), 'subset', subset);
    end
    g.stat_summary('type', 'sem', 'geom', 'bar', 'dodge', 0.7, 'width', 0.7);
    g.stat_summary('type', 'sem', 'geom', 'black_errorbar', 'dodge', 0.7, 'width', 0.7);
    g.set_names('x', 'parameters', 'y', 'parameter value');
    g.geom_hline('yintercept',0);
    g.set_title(model_short_name{m});
    g.draw();
end

% Parameter recovery analysis
figure('name', 'parameter recovery')
clear g
for m = 1:size(model_list,1)
    selector = strcmp(keep_model_names, model_short_name{m});
    param_names = keep_param_names(selector);    
    param_names = unique(param_names, 'stable');
    x = []; y = [];
    col =[];
    for t = 1:size(theta_fitsim{m},2)
        x = [x; zscore(theta_fitsim{m}{t}(:,1))];
        y = [y; zscore(theta_fitsim{m}{t}(:,2))];
        col = [col; t*ones(length(theta_fitsim{m}{t}(:,1)),1)];
    end
    
    for p = 1:size(phi_fitsim{m},2)
        x = [x; zscore(phi_fitsim{m}{p}(:,1))];
        y = [y; zscore(phi_fitsim{m}{p}(:,2))];
        col = [col; length(theta_fitsim{m}) + p*ones(length(phi_fitsim{m}{p}(:,1)),1)];
    end
    col=param_names(col)
    g(1,m) = gramm('x',x, 'y',y, 'color',col);
    g(1,m).stat_glm('distribution', 'normal', 'geom', 'line');
    g(1,m).geom_funline('fun', @(x)x,'style', 'k--');
    g(1,m).axe_property('xlim', [-3 3], 'ylim', [-3 3]);
    g(1,m).set_color_options('map', 'lch');
end
g.draw();
