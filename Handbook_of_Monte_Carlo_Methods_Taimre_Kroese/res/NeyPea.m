%NeyPea.m
N=10^5; n=16;W=nan(N,1);
for i=1:N
    S=0;
    for j=1:n
        U=rand;
        if U<1/4
            x=-1;
        elseif U<1/2
            x=1;
        else
            x=2*rand-1;
        end
        S=S+x;
    end
    W(i)=exp(-S+n*(log(2)-1))*(S>=0);
end
ell=mean(W), std(W)/sqrt(N)/mean(W)

