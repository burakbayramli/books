function[fval,ifail,icost]=hcevalp(v,working_directory)
% HCEVALP Parallel version of the HC objective.
%
[nc,nv]=size(v);
if nc ~= 12
   error('Size mismatch in v');
end
fval=zeros(1,nv);
ifail=zeros(1,nv);
icost=zeros(1,nv);
parfor i=1:nv
   [fval(i),ifail(i),icost(i)]=hceval(v(:,i),working_directory,i);
end


