function [c,x]=hdr(gam,m)
% gam is a vector with increasing levels
T=length(gam); c=zeros(T,1);
sum=0; B(1)=-1; % negative binomial
while sum<m+1
    x=randn(1,5); G=1;     % generate from f_0
    while S(x)<gam(1)
        G=G+1;x=randn(1,5); % generate from f_0
    end
    sum=sum+G; B(1)=B(1)+1; % binomial r.v.
end

for t=2:T
    sum=0; B(t)=-1; % negative binomial
    while sum<m+1
        x=hit_run(x,gam(t-1)); G=1;
        while (S(x)<gam(t))&&(G<m+1)
            G=G+1; x=hit_run(x,gam(t-1));
        end
        sum=sum+G; B(t)=B(t)+1; % binomial r.v.
    end
    [gam(t),B(t)/m]
    % stopping condition
    if (B(t)==0)|(t==T),  break, end
end
c=B/m; % estimate conditional probabilities


