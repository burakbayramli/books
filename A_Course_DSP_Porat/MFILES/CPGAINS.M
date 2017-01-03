function [cg,pg,jw] = cpgains(w,dtheta);
% Synopsis: [cg,pg,jw] = cpgains(w,dtheta).
% Computes the coherent gain and the processing gain of
% a given window as a function of the frequency deviation.
% Also computes the parameter Jw (see text).
% Input parameters:
% w: the window sequence (of length N)
% dtheta: the frequency deviation:
%   0 gives the best-case gains
%   2*pi/N gives the worst case for N-point DFT
%   2*pi/M gives the worst case for M-point zero-padded DFT
% Output parameters:
% cg: the coherent gain, in dB
% pg: the processing gain, in dB
% jw: the parameter Jw.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

N = length(w); w = reshape(w,1,N);
cg = (1/N)*abs(sum(w.*exp(j*0.5*dtheta*(0:N-1))));
pg = N*cg^2/sum(w.*w);
cg = 20*log10(cg); pg = 10*log10(pg);
n = (0:N-1)-0.5*(N-1);
jw = (N^3/12)*(sum((w.*n).^2)/(sum(w.*(n.^2)))^2);

