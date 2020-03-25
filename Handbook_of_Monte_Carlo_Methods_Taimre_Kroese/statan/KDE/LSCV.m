function [bandwidth,f,y_k]=LSCV(X,n,MIN,MAX)

X=X(:); %make data a column vector
% set up the grid over which the density estimate is computed;
R=MAX-MIN; dy=R/n; y_k=MIN+[0:dy:R]; N=length(X);
%bin the data uniformly using the grid define above;
[f_k,bins]=histc(X,y_k);f_k=f_k/N; 
f_k(end)=[]; y_k(1)=[]; bins(bins==n+1)=[];
a=dct1d(f_k); % discrete cosine transform of initial data
% now compute the optimal bandwidth^2 using the LSCV
t_LS= fminbnd(@g,0,.01);
% defines the LSCV function g(t) that is to be minimized
    function out=g(t) 
        a_t=a.*exp(-[0:n-1]'.^2*pi^2*t/2);
        f=idct1d(a_t)/dy;
        int_f2_dy=(f'*f)*dy;
        out=int_f2_dy-2/(N-1)*sum(f(bins))+2/(N-1)/sqrt(2*pi*t*R^2);
    end

a_t=a.*exp(-[0:n-1]'.^2*pi^2*t_LS/2); % smoothed coefficients
f=idct1d(a_t)/dy;    % take the IFCT of the data
bandwidth=t_LS*R^2; % adjust the bandwidth after the rescaling

end

