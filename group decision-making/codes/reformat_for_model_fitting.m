%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%this script is a reformatting code that you can use to transform the
%preprocessed data into the cell datatype in accordance to the requirement
%by the fitting code. The code is simplified to avoid duplicate repetition.
%Simply modify the code and you will get the data you want. Just follow the
%instructions beside the codes!

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%reformat the preprocessed data for model fitting
clear all;
clc;
%individual_data
load('individual_data.mat');
data=individual_i_data;
clear individual_i_data;
id_list=unique(data(:,1));
reformat_data=cell(length(id_list),1);
for i=1:length(id_list)
    tmp_data=data(find(data(:,1)==id_list(i)),:);
    reformat_data{i}.id=id_list(i);
    reformat_data{i}.gender=tmp_data(:,17);
    reformat_data{i}.cond = 0    ;            % a single integer corresponding to the id number of the group or condition
    reformat_data{i}.cond_label = 'individual';
    reformat_data{i}.trial=tmp_data(:,2);
    reformat_data{i}.deck=tmp_data(:,11)-10;
    reformat_data{i}.win=tmp_data(:,7);
    reformat_data{i}.lose=tmp_data(:,8);
    reformat_data{i}.rt=tmp_data(:,4);
    reformat_data{i}.total_score=sum(data{i}.win+data{i}.lose);
end
individual_data=reformat_data;

%no leader data reformat (modify the code to get various conditions of
%data)
load('no_leader_data.mat');
data=no_leader_data;
clear no_leader_data;
id_list=unique(data(:,1));
%%pick data according to their sub_id
id_list=id_list(find(id_list<2000));%by setting the standard, you can get the split of seperate conditions of data, just modify the code and get the data you want
reformat_data=cell(length(id_list),1);
for i=1:length(id_list)
    tmp_data=data(find(data(:,1)==id_list(i)),:);
    reformat_data{i}.id=id_list(i);
    reformat_data{i}.gender=tmp_data(:,23);
    reformat_data{i}.cond = 0    ;            % a single integer corresponding to the id number of the group or condition
    reformat_data{i}.cond_label = 'control';%edit the label as well for you better to distinguish the condition
    reformat_data{i}.trial=tmp_data(:,2);
    reformat_data{i}.s_decision=tmp_data(:,11)-10;%individual_decision
    reformat_data{i}.deck=tmp_data(:,13)-10;%group decision
    reformat_data{i}.win=tmp_data(:,7);
    reformat_data{i}.lose=tmp_data(:,8);
    reformat_data{i}.rt=tmp_data(:,4);
    reformat_data{i}.total_score=sum(data{i}.win+data{i}.lose);
    
    if mod(id_list(i),4)==0
        reformat_data{i}.social_number=tmp_data(:,14:17);
    elseif mod(id_list(i),4)==1
        reformat_data{i}.social_number=tmp_data(:,[17 14 15 16]);
    elseif mod(id_list(i),4)==2
        reformat_data{i}.social_number=tmp_data(:,[16 17 14 15]);
    else
        reformat_data{i}.social_number=tmp_data(:,[15 16 17 14]);
    end
end
group_control_data=reformat_data;
%group_discuss_data=reformat_data;

%%leader data
load('leader_data.mat');
data=leader_data;
clear leader_data;
id_list=unique(data(:,1));
%%pick data according to their sub_id
id_list=id_list(find(id_list<4000));%by setting the standard, you can get the split of seperate conditions of data, just modify the code and get the data you want
reformat_data=cell(length(id_list),1);
for i=1:length(id_list)
    tmp_data=data(find(data(:,1)==id_list(i)),:);
    reformat_data{i}.id=id_list(i);
    reformat_data{i}.gender=tmp_data(:,23);
    reformat_data{i}.cond = 0    ;            % a single integer corresponding to the id number of the group or condition
    reformat_data{i}.cond_label = 'leaderL';
    reformat_data{i}.trial=tmp_data(:,2);
    reformat_data{i}.s_decision=tmp_data(:,11)-10;%leader's first decision
    reformat_data{i}.deck=tmp_data(:,16)-10;%leader's second decision
    reformat_data{i}.win=tmp_data(:,7);
    reformat_data{i}.lose=tmp_data(:,8);
    reformat_data{i}.rt=tmp_data(:,4);
    reformat_data{i}.total_score=sum(data{i}.win+data{i}.lose);
    
    if mod(id_list(i),4)==0
        reformat_data{i}.social_number=tmp_data(:,17:20);
    elseif mod(id_list(i),4)==1
        reformat_data{i}.social_number=tmp_data(:,[20 17 18 19]);
    elseif mod(id_list(i),4)==2
        reformat_data{i}.social_number=tmp_data(:,[19 20 17 18]);
    else
        reformat_data{i}.social_number=tmp_data(:,[18 19 20 17]);
    end
end
group_leaderL_data=reformat_data;
%group_mixL_data=reformat_data;

%%note: to finally run the code IGTtoolbox2_Fitmodels.m, you should save
%%the data nanming 'data', and save in the ~IGTdata/ fold. Or you can
%%modift the fitting code with your own preferred routine to read the file.