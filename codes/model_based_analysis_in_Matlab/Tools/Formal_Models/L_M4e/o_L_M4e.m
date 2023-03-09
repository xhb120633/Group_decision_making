function  gx = o_L_M4e(x,P,u,in)
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
            
           cons=zeros(4,1);
            for de=1:4
                if de==u(6)
                  cons(de)=0;
                else
                   cons(de)=1;
                end
            end
            g2 =sub_softmax((P(2).^cons).*(x(9:12)+x(13:16)), consistency);
  
    function p = sub_softmax(collapsed, consistency)
        for i = 1:length(collapsed)
            p(i,1) = exp(collapsed(i)*consistency)/(exp(collapsed(1)*consistency)+exp(collapsed(2)*consistency)+exp(collapsed(3)*consistency)+exp(collapsed(4)*consistency));
        end
    end
    end

end
