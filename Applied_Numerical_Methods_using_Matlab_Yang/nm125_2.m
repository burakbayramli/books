%nm125_2: roundoff error test
clear
f1=inline('(1-cos(x))/x/x','x'); 
f2=inline('sin(x)*sin(x)/x/x/(1+cos(x))','x'); 
for k=0:1
   x=k*pi;  tmp= 1;
   for k1=1:8
     tmp=tmp*0.1; x1= x+tmp;
     fprintf('At x=%10.8f, ', x1) 
     fprintf('f1(x)=%18.12e; f2(x)=%18.12e\n', f1(x1),f2(x1));
   end
end