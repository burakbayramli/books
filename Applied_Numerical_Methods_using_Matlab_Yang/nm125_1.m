%nm125_1: overflow/underflow prevention
clear
x=36; y=10^16; 
for n=[-20 -19 19 20]
   fprintf('y^n/exp(n*x)=%25.15e\n', y^n/exp(n*x));
   fprintf('(y/exp(x))^n=%25.15e\n', (y/exp(x))^n);
end
