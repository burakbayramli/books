%gamma_best.m
N = 10^5; alpha = 0.3;
d= 0.07 + 0.75*sqrt(1-alpha); b = 1 + exp(-d)*alpha/d;
x = zeros(N,1);
for i = 1:N
    cont = true;
    while cont
        U1 = rand;
        U2 = rand;
        V = b*U1;
        if V <= 1
            X = d*V^(1/alpha);
            if U2 <= (2-X)/(2+X)
                cont = false; break;
            else
                if U2 <= exp(-X)
                    cont = false; break;
                end
            end
        else
            X = -log(d*(b-V)/alpha);
            y = X/d;
            if U2*(alpha + y*(1-alpha)) < 1
                cont= false; break;
            else
                if U2 <= y^(alpha - 1)
                    cont= false;break;
                end
            end
        end
    end
    x(i) = X;
end

% Uncomment for plotting purposes
%clf
%hold on
%x = sort(x);
%ecdf(x); % empirical cdf
%y = gamcdf(x,alpha);  % cdf of gamma distribution
%plot(x,y,'r')
%hold off
