function k = halfDiff(x1,x2)
% halfDiff  Reduce the distance between two numbers until it is set to zero
%
% Synopsis:  k = halfDiff(x1,x2)
%
% Input:   x1,x2 = starting interval, only x2 is changed
%
% Output:  k = number of divisions by 2 to reduce (x2-x1)/2 to less than realmin

fprintf('   k       x1                        x2                        delta\n');
for k=1:1075
   delta = (x2 - x1)/2;
   fprintf('%4d  %24.16e  %24.16e  %12.3e\n',k,x1,x2,delta);
   if delta==0,  break,  end
   x2 = x1 + delta;
end
