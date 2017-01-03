% PURPOSE: An example of using svar                                               
%---------------------------------------------------
% USAGE: svar_d
%---------------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5
y = test(:,1:5);
     
nlag = 2;  % number of lags in var-model

% estimate the model
results = vare(y,nlag);
n = results(1).neqs;
nsq=n^2;
% choleski restrictions for a five variables VAR
afree=10;bfree=5;
sa=zeros(nsq,afree);
sa(2,1)=1;sa(3,2)=1;sa(4,3)=1;sa(5,4)=1;sa(8,5)=1;
sa(9,6)=1;sa(10,7)=1;sa(14,8)=1;sa(15,9)=1;sa(20,10)=1;
da=zeros(nsq,1);da(1,1)=1;da(7,1)=1;da(13,1)=1;da(19,1)=1;da(25,1)=1;
sb=zeros(nsq,bfree);sb(1,1)=1;sb(7,2)=1;sb(13,3)=1;sb(19,4)=1; sb(25,5)=1;db=zeros(nsq,1);

[a, b,a_se, b_se]=svar(results,sa,sb,da,db,afree,bfree)


        
