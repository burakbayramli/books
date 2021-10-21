function qtrue = qtrue(x,t)
% Not working!

xi = [1.5 2.5];
u = [1 .5 1];

i = max(find(xi<x))
while t>0,
   x0 = x-u(i+1)*t;
   if x0 < xi(i)
       t = t - (x-xi(i))/u(i+1)
       x = xi(i);
       i = i-1
     else 
       x = x0;
       t = 0;
     end
   end %while

qtrue = exp(-beta*(x0-0.5).^2) .* cos(freq*(x0-0.5));
if x0>1.0 & x0<1.5
   qtrue = qtrue + 1;
   end


