function [y,rcv]=downcode(x)
% [y,rcv]=downcode(x)
%
% x=any vector of positive integers.
% y=downcoded version of x
%  =row vector with sequential symbols 0,1,2,...
% rcv= recovery vector such that x=rcv(y+1)
%    = ordered list of symbols in x.
%
% Example:  x=[2 1 2 4 4 2 1 4] ... symbols=1,2,4
%           y=[1 0 1 2 2 1 0 2] ... symbols=0,1,2
%         rcv=[1 2 4] ... x=rcv(y+1)
[M,N]=size(x);
if sum(abs(x-fix(x)))~=0 | min(x)<0 | M*N>max(M,N)
    error('downcode requires x to be a vector of positive integers.');
end
[f,x0,x1]=freq(x);                  % f=frequencies of x; x0=xmin
rcv=(x0-1) + find(f);               % ordered list of symbols in x
i=zeros(1,x1+1);
i(rcv+1)=0:length(rcv)-1;           % i=codes (values of y) for x=0,...,x1
y=i(x+1);
