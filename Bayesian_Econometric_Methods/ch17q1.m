%program which does empirical illustration for chapter 17 Exercise 1
%Univariate AR(p) model based on Geweke (1988) who uses an AR(3)
%The definitions of the features of interest relating to secular/cyclical behavior
%are defined for the AR(3) so always set p=3
load realgdp.txt;
rawdat=realgdp;
traw=size(rawdat,1);
yraw=rawdat(:,2);
%Specify order of AR
p=3;

%Now log and lag raw data to set things up for AR(p) model in logs
%Chop off p initial conditions and make matrices of lagged variables
y=log(yraw(p+1:traw,1));
ylag=zeros(traw-p,p);
for ii = 1:p
    ylag(:,ii)= log(yraw(p+1-ii:traw-ii,1));
end
t=size(y,1);
x = [ones(t,1) ylag];
k=size(x,2);

%Posterior under noninformative prior is t(bmean,vscale,v)
v=t-k;
xtxinv = inv(x'*x);
bmean=xtxinv*x'*y;
s2 = (y-x*bmean)'*(y-x*bmean)/v;
vscale=s2*xtxinv;
vchol=chol(vscale);
vchol=vchol';

%Now start Monte Carlo loop
%Specify the number of replications
r=10000;
%initialize variables
expcount=0;
ccount=0;
minroot=0;
minroo2=0;

%tdis_rnd takes random draws from the multivariate t
%with mean zero and scale, V=I
%Hence we have to transform draws to get t(bmean,vscale,v)
for i = 1:r
    %draw a t(0,1,v) then transform to yield draw of beta
    bdraw=bmean + vchol*tdis_rnd(k,v);
    %Matlabs root-finding subroutine orders the polynomial coefficients in the reverse order we have, hence make bstar
    bstar = [-bdraw(4,1) -bdraw(3,1) -bdraw(2,1) 1]; 
rr=roots(bstar);
%calculate absolute value of roots
rabs=abs(rr);
minrabs=min(rabs);
minroot=minroot + minrabs;
minroo2=minroo2 + minrabs^2;
if minrabs<1
    expcount=expcount+1;
end
%count the number of roots which are complex
icount=0;
for j=1:3
    if abs(imag(rr(j,1)))>0
        icount=icount+1;
    end
end
if icount==2
    ccount=ccount+1;
end
end


%Now create some results to print out
bcov = vscale*v/(v-2);
bsd=zeros(k,1);
for i = 1:k
bsd(i,1)=sqrt(bcov(i,i));
end
%now get posterior mean and standard deviation for the smalllest root
minroot = minroot/r;
mrootsd=sqrt(minroo2/r - minroot^2);

'Posterior mean and standard deviation for coefficients (intercept followed by AR coeffs)'
[bmean bsd]
'Posterior mean and standard deviation for minimum root (in absolute value'
[minroot mrootsd]
'Probability that the series is explosive'
expcount/r
'Probability that the series is oscillatory'
ccount/r
