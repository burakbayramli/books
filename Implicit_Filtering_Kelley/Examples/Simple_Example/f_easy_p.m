function [fv, ifail, icount]=f_easy_p(x)
% F_EASY_P
%
% A parallel version of f_easy.
% You must call matlabpool if you want this to run in parallel.
%
%function [fv, ifail, icount]=f_easy_p(x)
%
% fv must be a ROW vector. This makes scalar optimization consistent
% with what imfil does for nonlinear least squares.
%
% If you make fv a column vector, you will get some very interesting 
% error messages.
%
[nr,nc]=size(x);
fv=zeros(1,nc);
%
% Like f_easy, this function never fails and all calls to f have the
% same cost. We just have to say that nc times.
%
ifail=zeros(nc,1);
icount=ones(nc,1);
%
parfor i=1:nc
   fv(i) = feval('f_easy',x(:,i));
end
