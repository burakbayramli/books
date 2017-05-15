function y=D_1sf(lo,hi,sf,d)

% 1D synthesis filtering

lpf=sf(:,1); %low-pass filter
hpf=sf(:,2); %high-pass filter

if d==2, lo=lo'; hi=hi'; end;

N=2*size(lo,1);
L=length(sf);
y=upfirdn(lo,lpf,2,1)+upfirdn(hi,hpf,2,1);
y(1:L-2,:)=y(1:L-2,:)+y(N+[1:L-2],:);
y=y(1:N,:);

%2D circular shift
[nn,mm]=size(y); m=1-L/2; n=0:nn-1;
n=mod(n-m,nn);
y=y(n+1,:);

if d==2, y=y'; end;

