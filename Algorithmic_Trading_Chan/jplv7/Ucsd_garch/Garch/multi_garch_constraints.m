function [sumA, sumB, startingvalues, LB, UB, garchtype]=multi_garch_constraints( startingvalues, p,o, q, data, type)
% PURPOSE:
%    Set up the constriant for FMINCON in multigarch
% 
% USAGE:
%    [sumA, sumB, startingvalues, LB, UB, garchtype]=multi_garch_constraints(garchtype, startingvalues, sumA, sumB, p, q, data, type)% 
% INPUTS:
%     See multigarch for details    
%
% OUTPUTS:
%     See multigarch for details     
%
% COMMENTS:
%     Helper function for multigarch
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


LB         =  [];     
UB         =  [];     
sumA =  [-eye(1+p+o+q); ...
        0  ones(1,p) 0.5*ones(1,o)  ones(1,q)];
sumB =  [zeros(1+p+o+q,1);...
        1-2* 1e-6];                          
TolCon=2*1e-6;    
CTolCon = 0.02;

[lambda, nu, b, garchtype]=multi_garch_paramsetup(type);

% Now to build the parameter restrictions and starting values;
if garchtype==1
   % Do Nothing   
elseif garchtype==2
   % Do Nothing   
elseif garchtype==3
   % Need to add a B starting val and restriction
   sumA=[sumA zeros(size(sumA,1),1)] ;
   
   Bconst=zeros(2,size(sumA,2));
   Bconst(1,size(sumA,2))=1;
   Bconst(2,size(sumA,2))=-1;   
   Bconst1=quantile(data,.9);
   Bconst2=-quantile(data,.1);
   %(quantile(data,.3)>0)*quantile(data,.2)+(quantile(data,.3)<0)*(-1)*quantile(data,.3);
   
   sumA=[sumA;Bconst];
   sumB=[sumB;Bconst1;Bconst2];
   startingvalues=[startingvalues;b];
elseif garchtype==4
   % Need to add lambda starting values and restrictions
   sumA=[sumA';zeros(1,2+p+q+o)]';
   
   lambdaconst=zeros(1,size(sumA,2));
   lambdaconst(size(sumA,2))=-1;
   
   sumA=[sumA;lambdaconst];
   sumB=[sumB;.1];
   startingvalues=[startingvalues;lambda];   
elseif garchtype==5
   % Need to add a B starting val and restriction
   sumA=[sumA zeros(size(sumA,1),1)] ;
   
   Bconst=zeros(2,size(sumA,2));
   Bconst(1,size(sumA,2))=1;
   Bconst(2,size(sumA,2))=-1;   
   Bconst1=quantile(data,.9);
   Bconst2=-quantile(data,.1);
   %(quantile(data,.3)>0)*quantile(data,.2)+(quantile(data,.3)<0)*(-1)*quantile(data,.3);
   
   sumA=[sumA;Bconst];
   sumB=[sumB;Bconst1;Bconst2];
   startingvalues=[startingvalues;b];
elseif garchtype==6
   % Need to add lambda starting values and restrictions
   sumA=[sumA';zeros(1,2+p+q+o)]';
   
   lambdaconst=zeros(1,size(sumA,2));
   lambdaconst(size(sumA,2))=-1;
   
   sumA=[sumA;lambdaconst];
   sumB=[sumB;-.1];
   
   startingvalues=[startingvalues;lambda];
elseif garchtype==7
   % Need to add lambda and b starting values and restrictions
   sumA=[sumA';zeros(1,2+p+q+o)]';
   
   lambdaconst=zeros(1,size(sumA,2));
   lambdaconst(size(sumA,2))=-1;
   
   sumA=[sumA;lambdaconst];
   sumB=[sumB;-.1];

   % Need to add a B starting val and restriction
   sumA=[sumA zeros(size(sumA,1),1)] ;
   
   Bconst=zeros(2,size(sumA,2));
   Bconst(1,size(sumA,2))=1;
   Bconst(2,size(sumA,2))=-1;   
   Bconst1=quantile(data,.9);
   Bconst2=-quantile(data,.1);
   %(quantile(data,.3)>0)*quantile(data,.2)+(quantile(data,.3)<0)*(-1)*quantile(data,.3);
   
   sumA=[sumA;Bconst];
   sumB=[sumB;Bconst1;Bconst2];
  
   startingvalues=[startingvalues;lambda;b];
elseif garchtype==8
   % Do nothing
end
