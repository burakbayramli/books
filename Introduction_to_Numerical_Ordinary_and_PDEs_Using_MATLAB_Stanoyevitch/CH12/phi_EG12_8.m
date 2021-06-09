function y = phi_EG12_8(x)
for i = 1:length(x);
if (0<=x(i))&(x(i)<=pi/2)
     y(i)=x(i);
 else
     y(i)=pi-x(i);
end
end


