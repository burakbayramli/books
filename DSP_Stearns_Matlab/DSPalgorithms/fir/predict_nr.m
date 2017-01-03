function [e,b]=predict_nr(x,N,c)
% [e,b]=predict_nr(x,N,c)
%
% Least-squares one-step prediction of a single waveform.
% Note: unlike predict.m, this is ordinary prediction w/o rounding.
% Inputs:
%    N  = filter size.
%    x  = input vector to FIR predictor. 
%    c  = omitted or 0 for covariance; 1 for correlation.
%         NOTE: correlation is much faster than covariance.
% Outputs:  
%    e  = prediction error, e(1:length(x)).
%    b  = optimal weight vector with N elements.
%
% See also: predict, predict_q, lms_predict

x=x(:);                                 %assure x is a col. vector
if length(x)<N+1,
    error ('No prediction possible because x is too short.')
end
if nargin<3,
    c=0;
elseif c~=0 & c~=1,
    error ('Third argument (c) is not 0 or 1.')
end
K=length(x);
if c==0,
    Rff=autocovar_mat(x,N);             %covariance if c=0
    rfd=crosscovar([0; x(1:K-1)],x,N);
else
    Rff=autocorr_mat(x,0,N);            %correlation if not
    rfd=crosscorr([0; x(1:K-1)],x,0,N);
end
b=Rff\rfd;                              %optimal weights
g=filter([0; b],1,x);                   %one-step prediction
e=x-g;                                  %e is the prediction error
