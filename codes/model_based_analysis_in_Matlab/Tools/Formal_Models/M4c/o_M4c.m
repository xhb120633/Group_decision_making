function  gx = o_M4c(x,P,u,in)
%%%% EXPLORE model / Observation function
% see run_EXPLORE for a description of the model
% see documentation of VBA_toolbox for a description of inputs / outputs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% R.Ligneul 06/17

%% parameter transformation / should always be performed.
for pp =1:length(P)  
    P(pp) = in.param_transform{pp}(P(pp));   
end

%% compute probability of each choice given hidden states
consistency = 3^P(1)-1;
social_influence=u(8:11);
for de=1:4
    if de==u(12)%%no leader u(12), leader u(6)
        social_influence(de)=social_influence(de)^P(3);
    else
        social_influence(de)=social_influence(de)^P(2);
    end
end
gx = sub_softmax(x(1:4)+x(5:8)+social_influence', consistency);

%% softmax subfunction
function p = sub_softmax(collapsed, consistency)
    for i = 1:length(collapsed)
      p(i,1) = exp(collapsed(i)*consistency)/(exp(collapsed(1)*consistency)+exp(collapsed(2)*consistency)+exp(collapsed(3)*consistency)+exp(collapsed(4)*consistency));
    end
end

end