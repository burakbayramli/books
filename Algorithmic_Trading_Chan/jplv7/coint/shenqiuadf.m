function [b,t]=shenqiuadf(y,constant,trend,lags)
%Shenqiu Zhang
%Department of Economics
%Lancaster University
%Email: s.zhang3@lancs.ac.uk
%Jul 2007
[n,k]=size(y);
% Check Data
if k>1                     
   error('Vector Only')
end
% Check Data
ylag1=mlag(y,1);
y=diff(y);
dy=mlag(y,lags);
dy=dy(lags+1:end,:);
ylag1=ylag1(lags+2:end,1);
x=[ylag1,dy];
x=shenqiucandt(x,constant,trend); %create constand and trend
y=y(lags+1:end,1);
a=ols(y,x);
std=a.bstd(1,1);
b=a.beta(1,1);
t=(b-0)/std;
%%%%%%%%%%%%%%%%%%%%
function data=shenqiucandt(data,constant,trend)
% constant, 1 if true, 0 if false
% trend, 1 if true, 0 if false
[n,k]=size(data);
if constant==1                         %create constant
   data=[data,ones(n,1)];
else
   if constant~=0
      error('Wrong argument');
   end
end

if trend==1                           %creat trend
   trend=[1:n]';
   data=[data,trend/n];
else
    if trend~=0
       error('Wrong argument');
    end
end
clear n k;