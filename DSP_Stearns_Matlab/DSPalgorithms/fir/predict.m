function [e,b]=predict(x,N,c)
% [e,b]=predict(x,N,c)
%
% Least-squares one-step prediction of a single waveform.
% Note: This version has rounding and is used mainly in signsl comprssion.
%       Exact recovery is accomplished using function unpredict.
% Inputs:
%    N  = filter size.
%    x  = input vector to FIR predictor. 
%         NOTE: x is rounded to integers.
%    c  = omitted or 0 for covariance; 1 for correlation.
%         NOTE: correlation is much faster than covariance.
% Outputs:  
%    e  = prediction error, e(1:length(x)), rounded to integers.
%    b  = optimal weight vector with N elements.
%
% See also: unpredict, predict_nr, predict_q, lms_predict, lms_filter

x=round(col_vec(x));                    %x is rounded
if max(abs(x))==0
    error ('No prediction possible because x=0 after rounding.')
end
if nargin<3,
    c=0;
elseif c~=0 & c~=1,
    error ('Third argument (c) is not 0 or 1.')
end
K=length(x);
if c==0,
    Rff=autocovar_mat(x,N);           %covariance if c=0
    rfd=crosscovar([0; x(1:K-1)],x,N);
else
    Rff=autocorr_mat(x,0,N);            %correlation if not
    rfd=crosscorr([0; x(1:K-1)],x,0,N);
end
b=Rff\rfd;                              %optimal weights
g=filter([0; b],1,x);                   %one-step prediction
e=x-round(g);                           %e is rounded by rounding g
