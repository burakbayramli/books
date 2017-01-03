function [lambda, nu, b, garchtype, indicator]=multi_garch_paramsetup(type);
% PURPOSE:
%     Set up the parameters for multigarch
% 
% USAGE:
%     [lambda, nu, b, garchtype]=multi_garch_paramsetup(type);
% 
% INPUTS:
%     type     - (string) The type fo the GARCH form
% 
% OUTPUTS:
%     lambda      - Starting value for lambda
%     nu          - Starting value for nu
%     b           - Starting value for b
%     c           - Starting value for c
%     garchtype   - Numberical Value for Garchtyp
%     indicator   - Vector indicating which parameters are variables [lam; nu; b; c]
% 
% COMMENTS:
%     Helper function for multigarch
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

if strcmp(type,'GARCH');
   lambda=2;
   nu=2;
   b=0;
   indicator=[0 0 0]';
   garchtype = 1;
elseif strcmp(type,'TGARCH');
   lambda=1;
   nu=1;
   b=0;
   indicator=[0 0 0]';
   garchtype = 2;
elseif strcmp(type,'AVGARCH');
   lambda=1;
   nu=1;
   b=0;
   indicator=[0 0 1]';      
   garchtype = 3;
elseif strcmp(type,'NGARCH');
   lambda=2.3;
   nu=lambda;
   b=0;
   indicator=[1 0 0]';         
   garchtype = 4;
elseif strcmp(type,'NAGARCH');
   lambda=2;
   nu=2;
   b=0;
   indicator=[0 0 1]';            
   garchtype = 5;
elseif strcmp(type,'APGARCH');
   lambda=2;
   nu=lambda;
   b=0;
   indicator=[1 0 0]';   
   garchtype = 6;
elseif strcmp(type,'ALLGARCH');
   lambda=2;
   nu=lambda;
   b=0;
   indicator=[1 0 1]';
   garchtype = 7;      
elseif strcmp(type,'GJRGARCH');
   lambda=2;
   nu=2;
   b=0;
   indicator=[0 0 0]';
   garchtype = 8;
elseif strcmp(type,'EGARCH');
   lambda=2;
   nu=lambda;
   b=0;
   indicator=[1 0 1]';
   garchtype = 0; 
else
   error('Garch type must be one of the specified types, Please check spelling, ALL CAPS');
end


