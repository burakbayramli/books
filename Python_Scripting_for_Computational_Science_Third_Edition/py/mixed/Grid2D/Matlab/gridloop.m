n = 2000;
a = zeros([n,n]);
xcoor=0:(1/n):1;
ycoor=0:(1/n):1;
for i = 1:n,
   for j = 1:n,
      a(i,j) = f(xcoor(i), xcoor(j));
   end
end
cputime

