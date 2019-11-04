function [fa,ifaila,icounta]=pidobj(xa,pid_info)
% PIDOBJ calls PARALLEL_PIDLSQ to build an objective 
% function that does not use the least squares structure.
%
%
[nr,nc]=size(xa);
fa=zeros(1,nc);
[fl,ifaila,icounta]=parallel_pidlsq(xa,pid_info);
for i=1:nc
    fa(i)=fl(:,i)'*fl(:,i)/2;
end
