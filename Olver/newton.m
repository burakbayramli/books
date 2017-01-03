x=x0;

for k = 1:n
   x = x - f(x)/fp(x);
   fprintf(1,'%3d     ',k)
   fprintf(1,'%5.4f     ',x')
   fprintf(1,'\n')
end
