%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
This is a data documentation that contains information of how each of the data column mean in all the datasets for you to better use the data to replicate our research or investigate on your own interests. Our data has been preprocessed, formatting them in a long table. We did not provide complete raw data because it contains lots of personal information of participants, which we collected to check the match between individual participants, groups and their post-task surveys. The reformatted data masks most sensitive personal information, making each participant annoymous.

In our experiments we record many behavioral information. They are:

choice(integer):1,2,3,4; ---represents which option participants chose in their own versions(!!!). This means, 'choice' variable can be different meanings in different versions of participants. We did this because we apply 'Latin-counter-balance' to permute the order of options. To specify which option in the standard IGT, you should look at chose_performance.

chose_performance(integer): 11, 12, 13, 14---represents which option in the standard IGT participants chose.

reward information: contains gain, loss, and net-income. In IGT, the reward is always displayed with a gain and a loss. Participants can see both information. The net-income is gain-loss.

we also record participants' reaction time(RT) making each of the decision. We also record their confidence making each of the condition from 1 to 7 (least confident to most confident).

In group conditions, the social influence is also recorded in form of the distribution of the group votes(e.g., 3,2,0,0). However, note that, the recorded information is in order of their choices in the task--not in the standard IGT. You may need to transform this into the distribution in the standard IGT. To realize this, we contain condes both for modeling (in reformat.m) and model-free analysis (in Figure2.R code as a custom function). No worry about that!

Finally we recorded their gender and age information, for demographic analysis(in gender, 1-Female, 2-Male).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%The specific meaning in each of the dataset.
Individual_data.mat (data collected in individual condition)

From left to right column: Sub_id, Trial_Number, Original Choice, Choice_RT, Confidence, Confidence_RT, Gain, Loss, Net_income, Total_score(staring 2000), Chose_performance, Times participants have selected for each deck(in original version, 12-15, used to check the process once encountered bugs), version, gender, age.

No_leader_data.mat(data collected in group conditons, and contains all the non-leader data)

From left to right column: Sub_id, Trial_Number, Individual decision, Choice_RT, Confidence, Confidence_RT, Gain, Loss, Net_income, Total_score(staring 2000), Chose_performance (individual_decision), Group_decision, Chose_performance(group_decision) , distribution of the group votes(from 14 to 17), Times participants have selected for each deck(18-21), version, gender, age.

Leader_data.mat(data collected in group conditons, and contains all the leader data)

From left to right column: Sub_id, Trial_Number, Individual decision, Choice_RT, Confidence, Confidence_RT, Gain, Loss, Net_income, Total_score(staring 2000), Chose_performance (individual_decision), Group_decision, Chose_performance(group_decision) , leader decision(group_decision), leader_decision_RT, leader confidence, leader_confidence_RT, chose_performance(group_decision/ leader_decision), distribution of the group votes(from 17 to 20), Times participants have selected for each deck(18-21), version, gender, age.



%%%How to know which condition participants are in?
Participants' conditions can be indicated by their subject id. 

If 1000<sub_id<2000, they are in no_leader_no_discussion condition;
If 2000<sub_id<3000, they are in no_leader_discussion condition;
If 3000<sub_id<4000, they are in leader_no_discussion condition;
If sub_id>4000,they are in leader_discussion condition;

From their sub_id, we can also know the which of the five participants belong to the same group.
Take one subject with id 1101 as an example, his/her teammate id should be: 1201, 1301, 1401 and 1501.
To arrange this for group-level analysis, we prepared group_tag.csv for you to easily indicate their groups. Note that in leader groups, sub_id in the last column are always the leaders.

%%End
feel free to email me when you have any trouble in codes, data and understanding the documentations: hanboxie1997@email.arizona.edu