%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function y = splf(x)
%This function should really be inline with fPablo
%But that doesnt seem to work as expected...yet.
        
        
        global PPX
            y = ppval(PPX,x);
        end%function splf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%