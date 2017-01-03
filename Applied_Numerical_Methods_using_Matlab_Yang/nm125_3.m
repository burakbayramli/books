%nm125_3: reduce the roundoff error using Taylor series
clear
f3=inline('(exp(x)-1)/x','x'); 
f4=inline('((x/4+1)*x/3)+x/2+1','x'); 
x=0;  tmp= 1;
fprintf('\n')
for k1=1:12
   tmp=tmp*0.1; x1= x+tmp;
   fprintf('At x=%14.12f, ', x1) 
   fprintf('f3(x)=%18.12e; f4(x)=%18.12e\n', f3(x1),f4(x1));
end
