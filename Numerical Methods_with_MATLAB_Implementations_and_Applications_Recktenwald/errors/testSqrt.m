function testSqrt
% testSqrt  Test the newtsqrt function for a range of inputs

xtest = [4  0.04  4e-4  4e-6  4e-8  4e-10  4e-12];   % arguments to test

fprintf('\n Absolute Convergence Criterion\n');
fprintf('    x         sqrt(x)   newtsqrt(x)    error       relerr\n');

for x=xtest         %  repeat for each column in xtest
   r = sqrt(x);
   rn = newtsqrt(x);
   err = abs(rn - r);
   relerr = err/r;
   fprintf('%10.3e  %10.3e  %10.3e  %10.3e  %10.3e\n',x,r,rn,err,relerr)
end
