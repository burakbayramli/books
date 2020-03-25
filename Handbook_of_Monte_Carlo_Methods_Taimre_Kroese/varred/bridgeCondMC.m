%bridgeCondMC.m
N = 10^4;
S = zeros(N,1);
for i = 1:N
    u = rand(1,5);
    Z = Zcond(u);
    if Z(2)> Z(1) + 1 
        S(i) = 1/2 + Z(1);
    elseif (Z(2) > Z(1)) 
        S(i) = 5/12 + (3*Z(1))/4 - Z(1)^2/4 - Z(1)^3/12 + Z(2)/4 ...
	     + (Z(1)*Z(2))/2 + (Z(1)^2*Z(2))/4 ...
             - Z(2)^2/4 - (Z(1)*Z(2)^2)/4 + Z(2)^3/12; 
    else
        S(i) = (5 - 3*Z(1)^2 + 3*Z(2) - 3*Z(2)^2  ...
	     + Z(1)*(9 + 6*Z(2)))/12; 
    end
end
est = mean(S)
RE = std(S)/sqrt(N)/est
