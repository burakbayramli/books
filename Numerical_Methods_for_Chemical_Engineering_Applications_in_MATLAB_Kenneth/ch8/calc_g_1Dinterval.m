% calc_g_1Dinterval.m
% g = calc_g_1Dinterval(theta,sigma,Param);
%
% This routine returns the indicator function
% that parameter # Param.j in theta is found
% between the values Param.val_lo and
% Param.val_hi.
%
% K.J. Beers. MIT ChE. 12/9/2004

function g = calc_g_1Dinterval(theta,sigma,Param);

if((theta(Param.j) >= Param.val_lo) & ...
        (theta(Param.j) <= Param.val_hi))
    g = 1;
else
    g = 0;
end

return;
