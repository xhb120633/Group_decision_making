# Group_decision_making
The codes contains analysis from model-free to model-based, including codes generating main figures in the paper and supplementary materials. All the model fitting processes are implmented in Matlab, while the rest of the analysis (model-free analysis, parameter comparsion, parameter-survey correlation and SEM) are all implemented in R. Go to corresponding the folders to check the relevant analysis.


The Behavioral data and a complete set of model fitting results are stored in OSF:https://osf.io/8p9r3/

You can get access to the data as well as download the codes to replicate our results or analyze with some new explorations.


In the model fitting codes, we use Variational Bayesian Approach (VBA toolbox) to fit the models. Refer to: Daunizeau, J., Adam, V., & Rigoux, L. (2014). VBA: A Probabilistic Treatment of Nonlinear Models for Neurobiological and Behavioural Data. PLoS Comput Biol, 10(1), e1003441. For specific use of the toolbox, visit: https://mbb-team.github.io/VBA-toolbox/wiki/

It is notable that our fitting codes also adopt IGT toolbox developed by Romain Ligneul (2019) to run our model fitting and post-fitting analysis more efficiently. We write our own custom model codes within the framework of IGT toolbox. If you are interested, you could visit:https://github.com/romainligneul/igt-toolbox.


Any technical questions are welcomed to contact: hanboxie1997@arizona.edu.
