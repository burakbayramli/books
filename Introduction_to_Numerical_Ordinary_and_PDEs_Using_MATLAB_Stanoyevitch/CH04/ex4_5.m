function y = ex4_5(x)
if x<-1     
   y = -x.^2-4*x-2;
elseif x>1     
      y= 2-exp(sqrt(x-1));
   else    
      y = abs(x);
      
   end
   