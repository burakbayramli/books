function out=S(x)
x=-log(normcdf(x));
out=min([x(1)+x(4),x(1)+x(3)+x(5),x(2)+x(3)+x(4),x(2)+x(5)]);
 
