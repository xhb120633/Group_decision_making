function  [fx] = e_VSE(x,P,u,in)
%%%% EXPLORE model / Evolution function
% see run_EXPLORE for a description of the model
% see documentation of VBA_toolbox for a description of inputs / outputs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% R.Ligneul 06/17

%% parameter transformation
for pp = 1:length(P)  
    P(pp) = in.param_transform{pp}(P(pp));   
end

%% write current on next
fx = x;

%% update relevant current
% deck selected
d = u(2);

% update exploitation [1:4] and exploration values [5:8]
if u(1)==0 % initialization
     fx=[0 0 0 0 P(4)*ones(1,4)]; % sigma - exploration bonus
else       % update
    for de = 1:4
        if d~=de
            fx(de)= x(de)*P(2);% p(2) - value decay
            fx(de+4) = x(de+4) + P(3)*(P(4)-x(de+4)); % p(3) - exploration learning rate
        else
            fx(de)= x(de)*P(2) + abs(u(3))^P(1) - abs(u(4))^P(1); %p(1) theta - value sensitivity
            fx(de+4) = 0;
        end
    end
end

end