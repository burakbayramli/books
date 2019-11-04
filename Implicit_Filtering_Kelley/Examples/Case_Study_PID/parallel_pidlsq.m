function [fa,ifaila,icounta]=parallel_pidlsq(xa,pid_info)
% PARALLEL_PIDLSQ uses parfor to parallelize the serial code.
%
% The code will accept multiple input vectors and return a matrix of outputs.
%
[nr,nc]=size(xa);
fa=[];
ifaila=zeros(nc,1);
fcounta=zeros(nc,1);
parfor i=1:nc
    [fap,ifaila(i),icounta(i)]=serial_pidlsq(xa(:,i),pid_info);
    fa=[fa, fap];
end
