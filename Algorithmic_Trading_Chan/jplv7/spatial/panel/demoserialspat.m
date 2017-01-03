clear all;

A=wk1read('cigardemo.wk1',1,0);
W1=wk1read('Spat-Sym-US.wk1');
% Dataset downloaded from www.wiley.co.uk/baltagi/
% Spatial weights matrix constructed by Elhorst
% -------------------------------------------------------------------------
% written by: J.Paul Elhorst 5/2008
% University of Groningen
% Faculty of Economics and Business
% 9700AV Groningen
% the Netherlands
% j.p.elhorst@rug.nl
%
% REFERENCES: 
% Elhorst J.P. (2008) Serial and spatial autocorrelation. Economics Letters
% http://dx.doi.org/10.1016/j.econlet.2008.03.009
% -------------------------------------------------------------------------
% dimensions of the problem
T=6; % number of time periods
N=46; % number of regions
% row-normalize W
W=normw(W1); % function of LeSage
y=A(:,[3]); % column number in the data matrix that corresponds to the dependent variable
x=A(:,[4,5,6]); % column numbers in the data matrix that correspond to the independent variables
xconstant=ones(N*T,1);

% serial and spatial error correlation, including logpn variable
% one constraint on the paramaters active
vnames=strvcat('logcit','constant','logp','logpn','logy');
serialspatial(y,[xconstant x],W,N,T,vnames);

% serial and spatial error correlation, without logpn variable
% no constraint active, numerical problems with one of the t-values
x=A(:,[4,6]); % column numbers in the data matrix that correspond to the independent variables
vnames=strvcat('logcit','constant','logp','logy');
serialspatial(y,[xconstant x],W,N,T,vnames);