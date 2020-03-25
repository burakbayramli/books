%wsbridge.m
clear all
N = 10^4;w  = zeros(N,1);
for k=1:N 
    cont = true;
    while cont
        R = rand(1,5);v = rand*2;y = h(R);
        if v < y
            w(k) = 1/y;cont=false;
        end
    end
end
est = 1/mean(w)
percRE = std(w)*est/sqrt(N)*100

