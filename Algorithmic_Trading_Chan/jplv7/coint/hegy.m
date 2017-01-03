function results = hegy(x,sig,det,alag,fid);
% PURPOSE: performs the Hylleberg, Engle, Granger and Yoo(1990)
%          seasonal unit root test for quaterly time series, with 
%          and without intercept, seasonal dummies, and/or trend.
%          Also reported are the extensions of the HEGY method by
%          Ghysels, Lee and Noh (1994)
%-------------------------------------------------------------
% USAGE: results = hegy(x,sig,det,fid,lag);
% where:       x = a time-series vector
%            sig = level of significance or
%                  size of the test for all test 
%                  [1% 5% 10% 90% 95% 99%] 
%                  the default is 5%.
%            det = 1, no deterministic components
%                = 2, constant (default)
%                = 3, constant & 3 seasonal dummies
%                = 4, constant & trend
%                = 5, constant & 3 seasonal dummies & trend
%            lag = lag (integer) length for Augmented component.
%-------------------------------------------------------------
% RETURNS: Prints Out Results of the test
%-------------------------------------------------------------
% SEE ALSO: requires crthegy.m not a self standing program
%-------------------------------------------------------------
%Based on the procedure written for RATS by Suliman Al-Turki
%www.estima.com

%Written by:
% Carlos Alberto Castro
% National Planning Department
% Bogota, Colombia
% Email: ccastro@dnp.gov.co & ccastroir@cable.net.co



nobs = rows(x);
if nargin > 5 
  error('Wrong # of arguments to hegy');
elseif (nargin ==4)
    fid = 1;
elseif (nargin ==3)
    fid =1;
    alag =0;  % default has no augmented component
elseif (nargin ==2)
    fid =1;
    alag =0;  
    det =2; % the default model only includes a constant
elseif (nargin ==1)
    fid =1;
    alag =0;  
    det =2; 
    sig = 0.05; %the default level of significance is 95% 
else
    error('Wrong # of arguments to Hegy');
end;

%create auxiliary series 

y1= x + lag(x,1) + lag(x,2) + lag(x,3);
y1=trimr(y1,4,0);
y2= -x + lag(x,1) - lag(x,2) + lag(x,3);
y2=trimr(y2,4,0);
y3= -x + lag(x,2);
y3=trimr(y3,4,0);
y4= x - lag(x,4);
y4=trimr(y4,4,0);
 

%Deterministic component options
    switch det
    
    case 1 % no deterministic components

%Augmented lags of the dependent variable option 
if alag==0
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1)];
    xmat=trimr(xmat,2,0);
    yvec=trimr(y4,2,0);
elseif alag == 1
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) lag(y4,1)];    
    xmat=trimr(xmat,2,0);
    yvec=trimr(y4,2,0);
else
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) mlag(y4,alag)];    
    xmat=trimr(xmat,alag,0);
    yvec=trimr(y4,alag,0);
end

    case 2 % constant (default)

%Augmented lags of the dependent variable option 
if alag==0
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1)];
    xmat=trimr(xmat,2,0);
    yvec=trimr(y4,2,0);
elseif alag == 1
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1) lag(y4,1)];    
    xmat=trimr(xmat,2,0);
    yvec=trimr(y4,2,0);
else
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1) mlag(y4,alag)];    
    xmat=trimr(xmat,alag,0);
    yvec=trimr(y4,alag,0);
end
  
    case 3 % constant & 3 seasonal dummies

seasond=sdummy(rows(y4),4);
seasond=trimc(seasond,0,1);
%Augmented lags of the dependent variable option 
if alag==0
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1) seasond];
    xmat=trimr(xmat,2,0);
    yvec=trimr(y4,2,0);
elseif alag == 1
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1) seasond lag(y4,1)];    
    xmat=trimr(xmat,2,0);
    yvec=trimr(y4,2,0);
else
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1) seasond mlag(y4,alag)];    
    xmat=trimr(xmat,alag,0);
    yvec=trimr(y4,alag,0);
end    

    case 4 % constant & trend

%Augmented lags of the dependent variable option 
if alag==0
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1) seqa(1,1,rows(y4))];
    xmat=trimr(xmat,2,0);
    yvec=trimr(y4,2,0);
elseif alag == 1
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1) seqa(1,1,rows(y4)) lag(y4,1)];    
    xmat=trimr(xmat,2,0);
    yvec=trimr(y4,2,0);
else
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1) seqa(1,1,rows(y4)) mlag(y4,alag)];    
    xmat=trimr(xmat,alag,0);
    yvec=trimr(y4,alag,0);
end

    case 5 % constant & 3 seasonal dummies & trend

seasond=sdummy(rows(y4),4);
seasond=trimc(seasond,0,1);
%Augmented lags of the dependent variable option 
if alag==0
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1) seasond seqa(1,1,rows(y4))];
    xmat=trimr(xmat,2,0);
    yvec=trimr(y4,2,0);
elseif alag == 1
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1) seasond seqa(1,1,rows(y4)) lag(y4,1)];    
    xmat=trimr(xmat,2,0);
    yvec=trimr(y4,2,0);
else
    xmat=[lag(y1,1) lag(y2,1) lag(y3,2) lag(y3,1) ones(rows(y4),1) seasond seqa(1,1,rows(y4)) mlag(y4,alag)];    
    xmat=trimr(xmat,alag,0);
    yvec=trimr(y4,alag,0);
end    

    otherwise;
    error('Wrong kind of deterministic components for Hegy');
end;

% Estimation of the model and general test
     res=ols(yvec,xmat);
     pi1=res.tstat(1,1);  %t-stat for pi1
     pi2=res.tstat(2,1);  %t-stat for pi2
     pi3=res.tstat(3,1);  %t-stat for pi3
     pi4=res.tstat(4,1);  %t-stat for pi4
     residu=res.resid;      %residuals from the unrestricted model
     rrsu= residu'*residu; 
     nobsu= res.nobs;
     nvaru= res.nvar;
     
  %t-stat probabilities for deterministic components and last lag 
  %(these do not have to be cross check against the critical values tables from Hegy or Ghysels)
     switch det
  case 2
      detstat=res.tstat(5,1);
      pvdet = tdis_prb(detstat,nobsu-nvaru); % find t-stat probabilities
      info3.rnames = strvcat('Data','Const');    
  case 3
     for j=1:det+1
     detstat(j,1)=res.tstat(4+j,1); 
     pvdet(j,1) = tdis_prb(detstat(j,1),nobsu-nvaru); % find t-stat probabilities
     info3.rnames = strvcat('Data','Const','Sdum1','Sdum2','Sdum3');    
     end
   case 4
     for j=1:det-2
     detstat(j,1)=res.tstat(4+j,1); 
     pvdet(j,1) = tdis_prb(detstat(j,1),nobsu-nvaru); % find t-stat probabilities
     info3.rnames = strvcat('Data','Const','Trend');    
     end
  case 5
     for j=1:det
     detstat(j,1)=res.tstat(4+j,1); 
     pvdet(j,1) = tdis_prb(detstat(j,1),nobsu-nvaru); % find t-stat probabilities
     info3.rnames = strvcat('Data','Const','Sdum1','Sdum2','Sdum3','Trend');    
     end

  end
 
     if (alag~=0)
     kth =cols(xmat);    
     last=res.tstat(kth,1); 
     pvlast = tdis_prb(last,nobsu-nvaru); % find t-stat probabilities
     end


% F-statistics 
if det~=1 
%test of the null of unit roots at frequency: 0, pi/2 and pi simultaneously
     resr1=ols(yvec,xmat(:,5:cols(xmat)));
     residr1=resr1.resid;
     rrsr1= residr1'*residr1; 
     nobs1= res.nobs;
     nvar1= res.nvar;
%F-test pi1=pi2=pi3=pi4=0
theta14 = ((rrsr1-rrsu)/4)/(rrsu/(nobsu-nvaru));
%test of the null of unit roots at frequency: pi/2 and pi simultaneously
     resr2=ols(yvec,[xmat(:,1) xmat(:,5:cols(xmat))]);
     residr2=resr2.resid;
     rrsr2= residr2'*residr2; 
     nobs2= res.nobs;
     nvar2= res.nvar;
%F-test pi2=pi3=pi4=0
theta24 = ((rrsr2-rrsu)/3)/(rrsu/(nobsu-nvaru));
%test of the null of unit roots at frequency: pi/2 simultaneously
     resr3=ols(yvec,[xmat(:,1:2) xmat(:,5:cols(xmat))]);
     residr3=resr3.resid;
     rrsr3= residr3'*residr3; 
     nobs3= res.nobs;
     nvar3= res.nvar;
%F-test pi3=pi4=0
theta34 = ((rrsr3-rrsu)/2)/(rrsu/(nobsu-nvaru));
end
 
%Print Out Results (check consistency)

%Table 1 Coefficient Test
fprintf(fid,'\n Model= %6d (According to deterministic components) \n', det );
fprintf(fid,'**********************************************************\n');
fprintf(fid,'\n Coefficient Test \n');
fprintf(fid,'********************************************\n');
info1.cnames = strvcat('t-Statistic','Critical Value');
info1.rnames = strvcat('Data','pi1', 'pi2', 'pi3','pi4');
info1.fmt = '%16.6f';
x1=[pi1 crthegy('pi1',det,nobsu,sig);
    pi2 crthegy('pi2',det,nobsu,sig);
    pi3 crthegy('pi3',det,nobsu,sig);
    pi4 crthegy('pi4',det,nobsu,sig);];
mprint(x1,info1);
fprintf(fid,'********************************************\n');

if det~=1 
fprintf(fid,'\n Joint Test \n');
fprintf(fid,'********************************************\n');
info2.cnames = strvcat('F-Statistic','Critical Value');
info2.rnames = strvcat('Data','F14', 'F24', 'F34');
info2.fmt = '%16.6f';
x2=[theta14 crthegy('theta14',det,nobsu,sig);
    theta24 crthegy('theta24',det,nobsu,sig);
    theta34 crthegy('theta34',det,nobsu,sig);];
mprint(x2,info2);
fprintf(fid,'********************************************\n');

fprintf(fid,'\n Statistical Significance of Deterministic Components \n');
fprintf(fid,'********************************************\n');
info3.cnames = strvcat('P-Value');
info3.fmt = '%16.6f';
mprint(pvdet,info3);
fprintf(fid,'********************************************\n');
end

if (alag~=0)
fprintf(fid,'\n P-Value for the last lag = %9.4f \n', pvlast );
fprintf(fid,'**********************************************************\n');
end

%Diagnostics (residual autocorrelation)
diag=acf(residu);
d=[seqa(1,1,diag.k) diag.lowb diag.ac diag.topb diag.qstat diag.prob];
fprintf(fid,'\n Diagnostics (residual autocorrelation) \n');
fprintf(fid,'***********************************************\n');
infod.cnames = strvcat('Lag','Lowb','AC','Topb','Q-Stat','Prob');
infod.fmt = '%6.3f';
mprint(d,infod);
fprintf(fid,'***********************************************\n');






   