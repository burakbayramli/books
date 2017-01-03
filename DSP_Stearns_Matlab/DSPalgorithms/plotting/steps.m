function [ty,y]=steps(tx,x)
%[ty,y]=steps(tx,x)
%
%Created vectors for plotting steps from one sample to the next.
%
%inputs:  tx=original time vector
%          x=original sample vector
%
%Ouputs:  [ty,y].  When these vectors are plotted, the result is a plot of
%         x versus tx with steps instead of lines from one point to the 
%         next.  For example, if tx=[t0,t1,t2,...] and x=[x0,x1,x2,...],
%         then ty=[t0,t1,t1,t2,t2,...] and y=[x0,x0,x1,x1,x2,x2,...]
%

%check for errors
K=length(tx);
if length(x)~=K
    error('tx and x must be vectors of the same length')
end

%create ty and y
L=2*K-1;
ty=zeros(L,1);
ty(1:2:L)=tx;
ty(2:2:L-1)=tx(2:K);
y=zeros(L,1);
y(1:2:L)=x;
y(2:2:L-1)=x(1:K-1);
return

sp_fig(1); plot(tx,x,'o',ty,y,'-x'); grid


