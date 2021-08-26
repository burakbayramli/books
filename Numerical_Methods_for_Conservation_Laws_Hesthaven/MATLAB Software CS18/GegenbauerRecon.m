function [uRecon] = GegenbauerRecon(x,u,xp,xeval,N,Nc);
% function [uRecon] = GegenbauerRecon(x,u,xp,xeval,N,Nc);
% Purpose: Gegenbauer reconstruction of u with points of discontinuity in 
% vector xp. Recobstructed at xeval xp(1)/xp(end) = left/right point of domain
% Only Nc first terms are used.
Nseg = length(xp)-1; ii=sqrt(-1); Llen = xp(end)-xp(1);
xleval = length(xeval); uRecon = zeros(xleval,1);
uhat = fft(u)/(2*N+1);
if (Nc<N)
    uhat(Nc+2:2*N+1-Nc)=0;
end

% Set parameters for the Gegenbauer construction
lambda = floor(sqrt(Nc)); M = floor(sqrt(Nc));

% Postprocess each segment
for ns=1:Nseg
    a = xp(ns); b = xp(ns+1); 
    epsh=(b-a)/(2*pi); deltah = (a+b)/(2*pi);
    id = find((xeval>=a)&(xeval<=b));
    xl = xeval(id); xlen = length(id); r = -1 + 2*(xl-a)/(b-a);
    
    Cmat = zeros(xlen,M+1); b = zeros(M+1,1); 
    for m=0:M
        Cmat(:,m+1) = GegenbauerP(r,lambda,m);
        gammaG = pi*2^(1-2*lambda)*gamma(m+2*lambda)/...
               (gamma(m+1)*(m+lambda)*gamma(lambda)^2);
        for n=1:N
          b(m+1) = b(m+1)+uhat(n+1)*(2/(n*epsh*pi))^lambda*...
              besselj(m+lambda,n*epsh*pi)*exp(ii*pi*n*deltah);
          b(m+1) = b(m+1)+uhat(2*N+2-n)*(2/(-n*epsh*pi))^lambda*...
              besselj(m+lambda,-n*epsh*pi)*exp(-ii*pi*n*deltah);
        end
        b(m+1) = sqrt(gammaG)*gamma(lambda)*ii^m*(m+lambda)*b(m+1);
    end
    gammaG0 = pi*2^(1-2*lambda)*gamma(2*lambda)/(lambda*gamma(lambda)^2);
    b(1) = b(1)+uhat(1)*sqrt(gammaG0);
    uRecon(id) = Cmat*b;
end
return