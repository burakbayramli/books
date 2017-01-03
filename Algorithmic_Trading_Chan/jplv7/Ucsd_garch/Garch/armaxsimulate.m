function y=armaxsimulate(T,ar,ma,const,ARparams,MAparams,Xparams,X);
% PURPOSE:
%     Function designed to provide ARMAX simulation
% 
% USAGE:
%     y=armaxsimulate(T,ar,ma,const,ARparams,MAparams,Xparams,X)
% 
% INPUTS:
%     T           - Length of time series desired
%     ar          - Number of AR lags
%     ma          - Number of MA terms
%     const       - Intercept coefficient
%     ARparams    - A 1 by ar vector of parameters for the lags(t-1, t-2, ..., t-ar)
%     MAparams    - A 1 by ma vector of parameters for the ma terms(e-1,...,e-ma)
%     Xparams     - A cols(X) by 1 vectos of parameters for the X variables(optional)
%     X           - A T by k matrix fo exogenous variables(optional)
% 
% OUTPUTS:
%     y           - An ARMAX time series
% 
% COMMENTS:
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


tau=T;
%Make sure ARparams and MA Params are rows
if size(MAparams,1)>size(MAparams,2)
    MAparams=MAparams';
end
if size(ARparams,1)>size(ARparams,2)
    ARparams=ARparams';
end


%How many cases?  MA=0 | AR=0 | CONST=0 |With and Without X
if nargin<7 % No Expgenous
    T=T+2000;
    m=max(ar,ma);
    y=zeros(T+m,1);    
    if ar==0 %MA
        e=randn(T+m,1); 
        [E,E1]=newlagmatrix(e,ma,0);
        y=const+E1*MAparams'+E;
    elseif ma==0 %AR
        e=randn(T+m,1); 
        ybar=const/(1-sum(ARparams));
        y(:)=ybar;
        for i=m+1:T+m
            y(i)=const+ARparams*[y(i-1:-1:i-ar)]+e(i);
        end
    else  %ARMA
        e=randn(T+m,1); 
        ybar=const/(1-sum(ARparams));
        y(:)=ybar;
        for i=m+1:T+m
            y(i)=const+ARparams*[y(i-1:-1:i-ar)]+MAparams*[e(i-1:-1:i-ma)]+e(i);
        end
    end
end
y=y(length(y)-tau+1:length(y));










