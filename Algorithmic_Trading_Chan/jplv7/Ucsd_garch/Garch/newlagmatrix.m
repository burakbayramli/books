function [y,x]=newlagmatrix(x,nlags,c)
% PURPOSE:
%     Construct an X matrix and a Y vector for use in an AR regression
% 
% USAGE:
%     [y,x]=newlagmatrix(x,nlags,c)
% 
% INPUTS:
%     y is the dependant variable(nx1)
%     nlags is the number of lags(scalar)
%     c=1 if you want to include a constant
% 
% OUTPUTS:
%     Y will be (n-p)x1
%     X will be (n-p)xp(or p+1 if c=1)(lags)
% 
% COMMENTS:
%     Name cnflicts with a Matlab file
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

nlags=nlags+1;
t=size(x,1);
newX=[x;zeros(nlags,1)];
lagmatrix=repmat(newX,nlags,1);
lagmatrix=reshape(lagmatrix(1:size(lagmatrix,1)-nlags),t+nlags-1,nlags);
lagmatrix=lagmatrix(nlags:t,:);
y=lagmatrix(:,1);
x=lagmatrix(:,2:nlags);
if c==1
    x=[ones(size(x,1),1) x];
end
