function ell=convolution(t,nu)
% computes P(A_1+...+A_b>t) exactly,
%where A_i ~ Exp(nu(i)) independently;
%nu has to be decreasing (sorted) sequence
b=length(nu); % parameters of the waiting times
w=zeros(b,b); % b is critical number
w(1,1)=1;
for k=1:b-1
    for j=1:k
    w(k+1,j)=w(k,j)*nu(b-k)/(nu(b-k)-nu(b-j+1));
    w(k+1,k+1)=1-sum(w(k+1,1:k));
    end
end
ell=w(b,:)*exp(-nu(end:-1:1)'*t); % probability