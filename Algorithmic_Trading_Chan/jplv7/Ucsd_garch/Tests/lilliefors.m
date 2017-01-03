function [stat, critval, H]=lilliefors(x,testsize);
% PURPOSE:
%  Performs a lilliefors test for normality then the mean and variance are unknown 
%  If the mean and variance are known, the KS test is appropriate
% 
% USAGE:
%  [stat, critval, H]=lilliefors(x,testsize);
% 
% INPUTS:
%  x        - A set of deviates from a supposed normal that the user wants to test
%  testsize - Default is .05.  The size fo the test desired(Optional) Must be less than or equal to .20
% 
% OUTPUTS:
%  stat      - The calculated test statistic
%  critical  - The critical value for the test for the given size and length
%  H         - The hypothesis.  ! mean reject normality, 0 otherwise
% 
% 
% COMMENTS:
% 
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


if size(x,2)>size(x,1);
    x=x';
end


if nargin==1
    testsize=.05;
end
if testsize >.20 | testsize<0
    error('Wrong test size must be between 0 and .2')
end
if length(x)<4 | size(x,2)>1
    error('X must have more than 4 observations and must be a column vector')    
end

x=sort(x);
n=length(x);
mu=mean(x);
sigma=(x-mu)'*(x-mu)/(n-1);
cdfvals=norm_cdf(x,ones(size(x))*mu,ones(size(x))*sigma);
S=(1:n)'/n;

d=max(abs(cdfvals-S));

stat=d;

% Here is are the critical values for the Lilliefors Test for alpha = 
%    .8   .9    .95  .975  .99 
alpha = [ .2 .1 .05 .025 .01];
N=[4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 25 30];

if n<=30
CV= [.300 .319 .352 .381 .417
    .285 .299 .315 .337 .405
    .265 .277 .294 .319 .364
    .247 .258 .276 .300 .348
    .233 .244 .261 .285 .331
    .223 .233 .249 .271 .311
    .215 .224 .239 .258 .294
    .206 .217 .230 .249 .284
    .199 .212 .223 .242 .275
    .190 .202 .214 .234 .268
    .183 .194 .207 .227 .261
    .177 .187 .201 .220 .257
    .173 .182 .195 .213 .250
    .169 .177 .189 .206 .245
    .166 .173 .184 .200 .239
    .163 .169 .179 .195 .235
    .160 .166 .174 .190 .231
    .142 .147 .158 .173 .200
    .131 .136 .144 .161 .187 ];
critval=interp2(alpha',N',CV,testsize,n);
end

if n> 30
CV=[.736/sqrt(n) .768/sqrt(n) .805/sqrt(n) .886/sqrt(n) 1.031/sqrt(n)];
critval=interp1(alpha,CV,testsize);
end

H=stat>critval;
pval=1;

