% PARALLEL EASY
%
% A simple matlab parfor example.
% You must call matlabpool if you want this to run in parallel.
%
x=rand(2,16);
f=zeros(16,1);
parfor i=1:16
   f(i) = feval('f_easy',x(:,i));
end
f
