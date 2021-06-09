function y = EX13_7_bdydata(x)
for i = 1:length(x)
if (0<=x(i))&(x(i)<=2)
     y(i)=2*x(i)^2;
elseif (2<x(i))&(x(i)<=3)
     y(i)=8;
else
     y(i)=0;
end
end
