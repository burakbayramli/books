function [fa,ifaila,icounta]=scale_aware_pidlsq(xa,h,pid_info)
% SA_PARALLEL_PIDLSQ uses parfor to parallelize the serial code and
%                    is scale-aware.
%
% The code will accept multiple input vectors and return a matrix of outputs.
%
[nr,nc]=size(xa);
fa=[];
ifaila=zeros(nc,1);
fcounta=zeros(nc,1);
parfor i=1:nc
    [fap,ifaila(i),icounta(i)]=sa_serial_pidlsq(xa(:,i),h,pid_info);
    fa=[fa, fap];
end
