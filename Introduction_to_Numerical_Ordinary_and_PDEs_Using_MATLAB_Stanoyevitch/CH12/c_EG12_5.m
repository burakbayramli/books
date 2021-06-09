function y = c_EG12_4(t,x,u,ux)
for i = 1:length(x)
if (0<=x(i))&(x(i)<=3)
     y(i)=2;
elseif (3<x(i))&(x(i)<=5)
     y(i)=1;
else
     y(i)=0;
end
end
