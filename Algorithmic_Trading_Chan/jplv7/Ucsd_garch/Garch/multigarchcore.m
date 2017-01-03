function h=multigarchcore(datamb,dataneg,parameters,nu,lambda,b,p,o,q,m,T,stdEstimate);
% PURPOSE:
%     Helper function for multigarch
% 
% USAGE:
%     h=multigarchcore(datamb,dataneg,parameters,nu,lambda,b,p,o,q,m,T,stdEstimate);
% 
% INPUTS:
%     See multigarch_likelihood    
% 
% OUTPUTS:
%     h: The conditional standard deviation
% 
% COMMENTS:
%     You shoudl use the MEX file
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001
constp=parameters(1);
archp=parameters(2:p+1);
tarchp=parameters(p+2:p+o+1);
garchp=parameters(p+o+2:p+o+q+1);
h=stdEstimate*ones(size(datamb));
for t = (m + 1):T
    h(t) = (constp   +  archp'*(datamb(t-(1:p)).^nu)+ tarchp'*(dataneg(t-(1:o)).^nu) +  garchp'*(h(t-(1:q)).^lambda))^(1/lambda);
end