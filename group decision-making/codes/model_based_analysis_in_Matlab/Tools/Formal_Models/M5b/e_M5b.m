function  [fx] = e_M5b(x,P,u,in)
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
     fx=[0 0 0 0 P(4)*ones(1,4)];
else       % update
    for de = 1:4
        if d~=de
            if de==u(12)
                con=0;
            else
                con=1;
            end
            fx(de)= x(de)*P(2);
            fx(de+4) = x(de+4) +((P(6)^con))*P(3)+((P(4)+(u(de+7)^P(6))-x(de+4)));
        else
            if u(2)==u(12)
                con=0;
            else
                con=1;
            end
            fx(de)= x(de)*P(2) +(P(5)^con)* (abs(u(3))^P(1) - abs(u(4))^P(1)+u(de+7)^P(5));
            fx(de+4) = 0;
        end
    end
end

end