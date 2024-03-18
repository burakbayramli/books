%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
%                         DEMOFLOW                                  %
%                                                                   %
%       CALCULATION OF QUASI-1D INVISCID FLOW IN A TUBE/NOZZLE      %
%====================================================== =======     %
%             Alexander von Essen, Created Apr. 11 2005             %
%                   Last modified: Apr. 11 2005                     %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                   %
% Name: entropy_correction.m                                        %
% Purpose: Entropy correction due to Harten.                        %
% Called by: flux_roe.m                                             %
% Calls:                                                            %
%                                                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [g] = entropy_correction(Lambda,delta)

% if Lambda > delta
%     g = Lambda;
% else
%     g = (Lambda.^2+delta.^2)./(2.*delta);
% end


del = delta;
lambdafixed = Lambda;                                       % copy all,
i = find(Lambda < del);                                     % then the small ones,
if length(i) > 0                                            % if any,
    lambdafixed(i) = (Lambda(i).^2+del(i).^2)./(2*del(i));  % get replaced.
end

g = lambdafixed;