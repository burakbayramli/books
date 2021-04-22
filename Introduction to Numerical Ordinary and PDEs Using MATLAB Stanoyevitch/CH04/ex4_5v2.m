function y = ex4_5v2(x)
for i = 1:length(x)
if x(i)<-1
     y(i) = -x(i).^2-4*x(i)-2;
elseif x(i)>1
     y(i) = 2-exp(sqrt(x(i)-1));
else
     y(i)=abs(x(i));
end
end


