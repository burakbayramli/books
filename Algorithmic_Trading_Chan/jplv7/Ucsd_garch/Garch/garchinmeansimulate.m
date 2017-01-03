function [y,e,h]=garchinmeansimulate(T,ar,ma,const,arparams,maparams,gimparam,p,q,garchconst,archparams,garchparams);
% PURPOSE:
%     Function to simulate a Garch in mean ARMA time series
% 
% USAGE:
%     [y,e,h]=garchinmeansimulate(T,ar,ma,const,arparams,maparams,gimparam,p,q,garchconst,archparams,garchparams);
% 
% INPUTS:
%     T           - Length of time series desired
%     ar          - Number of AR lags
%     ma          - Number of MA terms
%     const       - Intercept coefficient
%     ARparams    - A 1 by ar vector of parameters for the lags(t-1, t-2, ..., t-ar)
%     MAparams    - A 1 by ma vector of parameters for the ma terms(e-1,...,e-ma)
%     GIMparam   - Scalar coefficient for the garch in mean 
%     p           - Number of lags of returns in the garch model
%     q           - Number of lags of H in the garch model
%     garchconst  - A scalar for teh constant fo a garch model(>0)
%     archprams   - A 1 by p vector of arch parameters(>=0)
%     garchparams - A 1 by q vector of garch parameters(>=0)
% 
% OUTPUTS:
%     y           - A Garch in mean ARMA time series
%     e           - Timeseries of heteroskedastic errors
%     h           - Time series of conditional volatilities
% 
% COMMENTS:
%    y(t) = const + ARparmas(1)*y(t-1) + ...+ Arparams(ar)*y(t-ar) + MAparams(1)*e(t-1) + ...
%           + MAparams(ma)*e(t-ma) + sqrt(h(t))*GIMparams + e(t) 
%
%     where e(t)~N(0,H(t))
%     H(t) = Omega + Alpha(1)*e_{t-1}^2 + Alpha(2)*e_{t-2}^2 +...+ Alpha(P)*e_{t-p}^2+...
%                    Beta(1)*H(t-1)+ Beta(2)*H(t-2)+...+ Beta(Q)*H(t-q)
%
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001




T=T+500;

m=max([ar ma p q]);
if ar==0
    arparams=0;
    ar=1;    
end
if ma==0
    maparams=0;
    ma=1;
end

y=zeros(T+1,1);
e=randn(T+1,1);
h=zeros(T+1,1);
h(1:m+1)=garchconst/(1-sum(archparams)-sum(garchparams));
e(1:m)=e(1:m).*sqrt(h(1:m));
maparams=[1 maparams];
for t=m+1:T
    % Make the erros have the correct variance
    e(t)=sqrt(h(t))*e(t);
    % Build up the Y's    
    y(t)=const+arparams*y(t-1:-1:t-ar)+maparams*e(t:-1:t-ma)+gimparam*sqrt(h(t));
    % Update the GARCH vol
    h(t+1)=garchconst+archparams'*e(t:-1:t-p+1).^2+garchparams'*h(t:-1:t-q+1);    
end

t=501:T;
y=y(t);
h=h(t);
e=e(t);


