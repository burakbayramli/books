x=x0; y = x1;

for i = k:n
   z = (x*f(y) - y*f(x))/(f(y) - f(x));
   fprintf(1,'%3d     ',k)
   fprintf(1,'%5.4f     ',z')
   fprintf(1,'%5.4f     ',f(z)')
   fprintf(1,'\n')
   x = y; y = z;
end
