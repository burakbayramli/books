function X=brownian_bridge(t,x_r,x_s,Z)
% generates a brownian bridge between the points
% x_r and x_s in the time interval [r,s];
% the discrete skeleton of the bridge
% is given at  times r=t(1),...,t(n+2)=s, where
% t is a vector of 'n+2' increasing time values. 
% In this way there are 'n' newly sampled
% values between x_r and x_s.
% Z is the vector with 'n' gaussian random variables

n=length(t)-2;X=nan(1,n+2);
X(1)=x_r; X(n+2)=x_s;s=t(n+2);
for k=2:n+1
    mu=X(k-1)+(x_s-X(k-1))*(t(k)-t(k-1))/(s-t(k-1));
    sig2=(s-t(k))*(t(k)-t(k-1))/(s-t(k-1));
    X(k)=mu+sqrt(sig2)*Z(k-1);
end
