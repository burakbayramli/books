function y = phiEFR12_13(x)
for i = 1:length(x)
    if x(i)<=3 & x(i)>=1, y(i) = 100;
    else, y(i)=0;
    end
end
