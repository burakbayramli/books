function y = EFR12_3nu(x)
for i = 1:length(x)
if abs(x(i))<1, y(i)=1;
else y(i)=0;
end
end

