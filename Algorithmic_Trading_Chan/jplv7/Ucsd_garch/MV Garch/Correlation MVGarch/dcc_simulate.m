function [finaldata,Ht,state,Rt, Qt]=dcc_simulate(k,t,CorrMat,garchparameters,archP,garchQ,dccparameters,dccP,dccQ,state);
% PURPOSE:
%     Simulate a DCC MVGARCH time series
% 
% USAGE:
%     [finaldata,Ht,state]=dcc_simulate(k,t,CorrMat,garchparameters,archP,garchQ,dccparameters,dccP,dccQ);
% 
% INPUTS:
%     k                - the number of series to be returned
%     t                - The length of the data to be returned
%     CorrMat          - A k by k matrix of unconditional correlation
%     garchparameters  - A vecotor of garch parameters for the univatiate garch processes, 
%                          k+sum(archP)+sum(garchQ) by 1 of the form [omega(1) a(11) (a12) ... a(1archP(1)) b(11) ... b(1(garchQ(1))
%                          omega(2) ... b(2garchQ(2)) ... omega(k) ... b(kgarchQ(k))
%     archP            - A vector of lag lengths of the individual garch innovations [k by 1]
%     garchQ           - A vector of lag lengths of the individual garch AR terms [k by 1]
%     dccparameters    - The DCC parameters (DccP+DccQ x 1)
%     dccP             - The order of the DCC innovation term
%     dccQ             - The order of the DCC AR term
%     state            - (optional) The state to which to set randn.  Should be a 2 vector.  
%                      If not included, the state is reinitialized by randn('state',sum(100*clock));
% 
% OUTPUTS:
%  finaldata - The simulated data from the entered parameters, t x k
%  Ht        - The estimated variance-covariance k x k x t
%  state     - The state of randn, so you canrecreate it if needed
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

   
   
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
   
   
m2=max(dccP,dccQ);
m1=max(max(garchQ,archP));
m=max(m2,m1);
state=randn('state');
rawdata=randn(t+m,k);





Qbar=CorrMat;
stdresid=randn(t+m,k);


dccA=dccparameters(1:dccP);
dccB=dccparameters(dccP+1:dccQ+dccP);
sumA=sum(dccA);
sumB=sum(dccB);
Qt=zeros(k,k,m+t);
Qt(:,:,1:m)=repmat(Qbar,[1 1 m]);
Rt=zeros(k,k,m+t);
Qt(:,:,1:m)=repmat(Qbar,[1 1 m]);
P=dccP;
Q=dccQ;

for j=(m+1):t+m
    Qt(:,:,j)=Qbar*(1-sumA-sumB);   
    for i=1:P
        Qt(:,:,j)=Qt(:,:,j)+dccA(i)*(stdresid(j-i,:)'*stdresid(j-i,:));
    end
    for i=1:Q
        Qt(:,:,j)=Qt(:,:,j)+dccB(i)*Qt(:,:,j-i);
    end
    Rt(:,:,j)=Qt(:,:,j)./(sqrt(diag(Qt(:,:,j)))*sqrt(diag(Qt(:,:,j)))');
    stdresid(j,:)=rawdata(j,:)*(Rt(:,:,j))^(0.5);

end;


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
    
    h=garchcore(RandomNums,parameters,UncondStd,archP(i),garchQ(i),m,t+m);
    data=RandomNums.*sqrt(h);
    finaldata(:,i)=data;
    H(:,i)=h;
end



finaldata=finaldata(m+1:t+m,:);
Ht=zeros(k,k,t+m);
for i=m+1:t+m
    Ht(:,:,i)=diag(H(i,:).^(0.5))*Rt(:,:,i)*diag(H(i,:).^(0.5));
end
Ht=Ht(:,:,m+1:t+m);
Rt=Rt(:,:,m+1:t+m);
Qt=Qt(:,:,m+1:t+m);






