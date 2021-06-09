function y = phihat(x)
for i = 1:length(x)
    if (x(i)<=2)&(x(i)>=0), y(i)=1-abs(1-x(i));
    elseif (x(i)<2)&(x(i)>=-2), y(i)=-phihat(-x(i));
    else n = floor((x(i)+2)/4);, r = x(i)-4*n;, y(i) = phihat(r);
    end
end
