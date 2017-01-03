function [finaldata,Ht,state]=cc_mvgarch_simulate(k,t,CorrMat,garchparameters,archP,garchQ,state);
% PURPOSE:
%        Simulate data from a CC MV GARCH process
% 
% USAGE:
%      [finaldata,Ht,state]=cc_mvgarch_simulate(k,t,CorrMat,garchparameters,archP,garchQ,state);
% 
% INPUTS:
%         k                - the number of series to be returned
%         t                - The length of the data to be returned
%         CorrMat          - A k by k matrix of unconditional correlation
%         garchparameters  - A vecotor of garch parameters for the univatiate garch processes, 
%                             k+sum(archP)+sum(garchQ) by 1 of the form [omega(1) a(11) (a12) ... a(1archP(1)) b(11) ... b(1(garchQ(1))
%                              omega(2) ... b(2garchQ(2)) ... omega(k) ... b(kgarchQ(k))
%         archP            - A vector of lag lengths of the individual garch innovations [k by 1]
%         garchQ           - A vector of lag lengths of the individual garch AR terms [k by 1]
%         state            - (optional) The state to which to set randn.  Should be a 2 vector.  
%                      If not included, the state is reinitialized by randn('state',sum(100*clock));
% 
% OUTPUTS:
%         finaldata - The simulated data from the entered parameters, t x k
%         Ht        - The estimated variance-covariance k x k x t
%         state     - The state of randn, so you canrecreate it if needed
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

t=t+500; 
   
if isempty(archP)
    archP=ones(1,k);
elseif length(archP)==1
    archP=ones(1,k)*archP;
end

if isempty(garchQ)
    garchQ=ones(1,k);
elseif length(garchQ)==1
    garchQ=ones(1,k)*garchQ;
end
   
m=max(max(garchQ,archP));
if  nargin <=6 
    randn('state',sum(100*clock));
    state=randn('state');
else
    randn('state',state);
end

rawdata=randn(t+m,k);
stdresid=randn(t+m,k);
R=CorrMat;

stdresid=rawdata*R^(1/2);

% We now have correlated residuals.  Now we need to simulate the univariate GARCHs
index=1;
finaldata=zeros(t+m,k);
H=zeros(t+m,k);
for i=1:k
    parameters=garchparameters(index:index+archP(i)+garchQ(i));
    index=index+1+archP(i)+garchQ(i);
    constp=parameters(1);
    archp=parameters(2:archP(i)+1);
    garchp=parameters(archP(i)+2:archP(i)+garchQ(i)+1);
       
    UncondStd =  sqrt(constp/(1-sum(archp)-sum(garchp)));
    h=UncondStd.^2*ones(t+m,1);
    data=UncondStd*ones(t+m,1);
    RandomNums=stdresid(:,i);
    T=size(data,1);
    for j = (m + 1):t+m
        h(j) = parameters * [1 ; data(j-(1:archP(i))).^2;  h(j-(1:garchQ(i))) ];
        data(j)=RandomNums(j)*sqrt(h(j));
    end
    finaldata(:,i)=data;
    H(:,i)=h;
end


finaldata=finaldata(m+501:t+m,:);
Ht=zeros(k,k,t+m);
for i=m+501:t+m
    Ht(:,:,i)=diag(H(i,:).^(0.5))*R*diag(H(i,:).^(0.5));
end
Ht=Ht(:,:,m+501:t+m);






