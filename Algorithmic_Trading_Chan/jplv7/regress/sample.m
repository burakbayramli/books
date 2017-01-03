function [vinew,vonew] = sample(vin,vout)
% PURPOSE: function used by sar BMA models to sample variables for changing model size
% ----------------------------------------------------------
% USAGE: [vinew vonew] = sample(vin,vout)
% where:   vin  = a 1 x nvar1 vector of variable #'s for
%                 variables included in the model
%          vout = a 1 x nvar2 vector of variable #'s for
%                 variables excluded from the model
% ----------------------------------------------------------
% RETURNS: vinew = a 1 x nvar1+1 or 1 x nvar1-1 vector of
%                  variable #'s in the new model
%          vonew = a 1 x nvar2+1 or 1 x nvar2-1 vector of
%                  variable #'s excluded from the new model
% ----------------------------------------------------------

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jlesage@spatial-econometrics.com
% last modified June, 2004

% decide on increase, move, decrease model size
coin = rand;
if coin < 0.33
    if isempty(vout)
        increase = 1;
    else
    increase = 0;
    end
elseif coin > 0.66
    if isempty(vin)
        increase = 0;
    else
    increase = 1;
    end
else
    increase = 2;
end;

switch increase
case {0} % decrease the # variables in the model
    choose = floor(1+length(vout)*rand);
    vinew = [vin vout(choose)];
    vonew = vout;
    vonew(choose) = []; %removes element
case {1} % increase the # variables in the model
    choose = floor(1+length(vin)*rand);
    vonew = [vout vin(choose)];
    vinew = vin;
    vinew(choose) = []; %removes element
case {2} % change a variable that is in with one that is out
    if ~isempty(vin) && ~isempty(vout)
        choose1 = floor(1+length(vout)*rand);
        choose2 = floor(1+length(vin)*rand);
        vinew = vin;
        vonew = vout;
        vonew(choose1) = vin(choose2);
        vinew(choose2) = vout(choose1);
    end
otherwise
    disp('error in sample function');    
end; % end of switch   
    