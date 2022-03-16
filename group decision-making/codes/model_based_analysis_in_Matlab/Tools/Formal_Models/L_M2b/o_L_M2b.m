function  gx = o_L_M2b(x,P,u,in)
%%%% EXPLORE model / Observation function
% see run_EXPLORE for a description of the model
% see documentation of VBA_toolbox for a description of inputs / outputs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% R.Ligneul 06/17
gx = cat(1, ...
        g_source1 (x, P, u, in), ...
    	g_source2 (x, P, u, in));
%source 1
 function g1=g_source1(x,P,u,in)
        for pp =1:length(P)  
             P(pp) = in.param_transform{pp}(P(pp));   
        end
            consistency = 3^P(1)-1;
            g1 = exp((x(1:4)+x(5:8))*consistency)/sum(exp((x(1:4)+x(5:8))*consistency));
        
 end

%source2
function g2=g_source2(x,P,u,in)
        for pp =1:length(P)  
             P(pp) = in.param_transform{pp}(P(pp));   
        end
            consistency = 3^P(1)-1;
            g2 =exp((x(9:12)+x(13:16))*consistency)/sum(exp((x(9:12)+x(13:16))*consistency));

    end

end
