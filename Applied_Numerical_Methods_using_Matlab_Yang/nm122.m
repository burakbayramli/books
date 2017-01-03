%nm122
f1=inline('sqrt(x)*(sqrt(x+1)-sqrt(x))','x');
f2=inline('sqrt(x)./(sqrt(x+1)+sqrt(x))','x');
x=1;
format long e
for k=1:15
   fprintf('At x=%16.0f, f1(x)=%20.18f,  f2(x)=%20.18f\n',x,f1(x),f2(x)); 
   x= 10*x; %16*x;
   if k>13
     sx1= sqrt(x+1);  sx= sqrt(x); d=sx1-sx;  s=sx1+sx;
     fprintf('sqrt(x+1)=%25.13f, sqrt(x)=%25.13f \n',sx1,sx); 
     fprintf('  diff=%25.23f, sum=%25.23f \n',d,s); 
   end
end
sx1= sqrt(x+1);  sx= sqrt(x); d=sx1-sx;  s=sx1+sx;
fprintf('sqrt(x+1)=%25.13f, sqrt(x)=%25.13f \n',sx1,sx); 
fprintf('  diff=%25.23f, sum=%25.23f \n',d,s); 