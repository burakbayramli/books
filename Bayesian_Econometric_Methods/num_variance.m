%For given parameter draws, this m-file calculates the 
%numerical variance of the sample mean, based on simulated output

function out  = num_var(x);
n = length(x);

tempp3 = 1;
i=1;
while tempp3 > .05; 
    tempp = x(1:n-i);
    tempp2 = x(1+i:n);
    tempp3  = corrcoef(tempp,tempp2);
    critical = tempp3(2,1);
    rho(i,1) = critical;
    i=i+1;
end;
                    %this could be modified a little. As it 
                    %is written, even under independent sampling,
                    %the contribution of the first autocorrelation
                    %is always included when calculating the NSE
for j = 1:length(rho);
    lagterm(j,1) = (1-(j/n))*rho(j);
end;
out = (var(x)/n)*(1 + 2*sum(lagterm));
