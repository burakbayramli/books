function y = Apulse_EG12_4(t)
for i = 1:length(t)
if (0<=t(i))&(t(i)<=pi/5)
     y(i)=sin(5*t(i));
else
     y(i)=0;
end
end
