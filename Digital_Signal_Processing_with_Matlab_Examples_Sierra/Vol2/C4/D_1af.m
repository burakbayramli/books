function [lo,hi]=D_1af(x,af,d)

% 1D analysis filtering

lpf=af(:,1); %low-pass filter
hpf=af(:,2); %high-pass filter

if d==2, x=x'; end;

N=size(x,1);
L=size(af,1)/2;
%2D circular shift
[nn,mm]=size(x); m=-L; n=0:nn-1;
n=mod(n-m,nn);
x=x(n+1,:);

lo=upfirdn(x,lpf,1,2);
lo(1:L,:)=lo(1:L,:)+lo([1:L]+N/2,:);
lo=lo(1:N/2,:);

hi=upfirdn(x,hpf,1,2);
hi(1:L,:)=hi(1:L,:)+hi([1:L]+N/2,:);
hi=hi(1:N/2,:);

if d==2, lo=lo'; hi=hi'; end;

