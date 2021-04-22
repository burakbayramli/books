%Collatz script
a(1) = input('Enter a postive integer:   ');
n=1;
while a(n) ~= 1
   if ceil(a(n)/2)==a(n)/2  %tests if an is even
      a(n+1)=a(n)/2;
   else
      a(n+1)=3*a(n)+1;
   end
   n=n+1;
end
   fprintf('\r Collatz iteration with initial value a(1)= %d took \r', a(1))
   fprintf(' %d iterations before reaching the value 1 and begining\r',n-1)
   fprintf(' to cycle. The resulting pre-cycling sequence is as follows:')
   a 
   clear a %lets us start with a fresh vector a on each run