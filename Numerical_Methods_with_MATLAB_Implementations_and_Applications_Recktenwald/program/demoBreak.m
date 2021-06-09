function k = demoBreak(n)
% demoBreak  Show how "break" causes exit from a while loop
%            Search a random vector to find index of first element
%            greater than 0.8.
%
% Synopsis:  k = demoBreak(n)
%
% Input:     n = size of random vector to be generated
%
% Output:    k = first (smallest) index in x such that x(k)>0.8
x = rand(1,n);
k = 1;
while k<=n
   if x(k)>0.8
     break
   end
   k = k + 1;
end
fprintf('x(k)=%f   for k = %d   n = %d\n',x(k),k,n);

%  What happens if loop terminates without finding x(k)>0.8 ?
