function  gx = o_M2b(x,P,u,in)
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
social_influence=[u(8)^P(2),u(9)^P(2),u(10)^P(2),u(11)^P(2)];
gx = sub_softmax(x(1:4)+x(5:8)+social_influence, consistency);

%% softmax subfunction
function p = sub_softmax(collapsed, consistency)
    for i = 1:length(collapsed)
      p(i,1) = exp(collapsed(i)*consistency)/(exp(collapsed(1)*consistency)+exp(collapsed(2)*consistency)+exp(collapsed(3)*consistency)+exp(collapsed(4)*consistency));
    end
end

end