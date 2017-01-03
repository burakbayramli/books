function k = demoReturn(n)
% demoReturn  Show how "return" causes exit from a function
%             Search a random vector to find index of first element
%             greater than 0.8.
%
% Synopsis:  k = demoReturn(n)
%
% Input:     n = size of random vector to be generated
%
% Output:    k = first (smallest) index in x such that x(k)>0.8
x = rand(1,n);
k = 1;

while k<=n
   if x(k)>0.8
     return
   end
   k = k + 1;
end           %  What happens if loop terminates without finding x(k)>0.8 ?
