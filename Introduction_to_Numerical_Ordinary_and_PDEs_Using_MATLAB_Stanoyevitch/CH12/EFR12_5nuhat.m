function y = EFR12_5nuhat(x)
for i=1:length(x)
if (x(i)>=0)&(x(i)<=10), y(i)=-2*BSprime(x(i)-3);
elseif (x(i)<0)&(x(i)>=-10), y(i) = -EFR12_5nuhat(-x(i));
else q=floor((x(i)+10)/20);, y(i)=EFR12_5nuhat(x(i)-20*q);
end
end
