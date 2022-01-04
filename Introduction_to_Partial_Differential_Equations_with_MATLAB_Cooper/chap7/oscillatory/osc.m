

%  This program computes an oscillatory integral on the interval
%  x in [-2 2] with the integrand
%
%         f(x,x0,t) = exp(-3x**2)*cos(t*x) 
%
%  to illustrate the method of stationary phase.
%  This program requires the supporting mfiles intgnd1.m.
%  The integrals are evaluated using the matlab package quad8.
%  The values of RF(t) are placed in the vector RF and the t values
%  are place in the vector tt.



global t
tt = zeros(size(31));
RF = zeros(size(31));

    for k = 1:20;
      tt(k) = k;
       t = k
       RF(k) = quad8('intgnd1', -2,2);
     end


   plot(tt,RF)

   xlabel( ' increasing t ')
   ylabel('   RF(t) for linear phase  ')










