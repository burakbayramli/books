function [bsdata, indexes]=cont_bootstrap(data,w,B);
% PURPOSE:
%   Implements Politis' continuous bootstrap for bootstrapping unit root series
% 
% USAGE:
%     [bsdata, indexes]=cont_bootstrap(data,w,B);
% 
% INPUTS:
%     data: T by 1 matrix of data to be bootstrapped(shoudl be unit root)
%     w:    Desired average windows length
%     B:    Number fo bootstraps
% 
% OUTPUTS:
%     bsdata: T x B matrix of bootstrapped data
%     indexes: T by B matrix of location sof the original data(data(indexes)=bsdata;
% 
% COMMENTS:
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001


p=1/w;
[t,k]=size(data);
s=sign(data);
tempdata=data;
data=data.^2;
bsdata=zeros(t,B);
indexes=zeros(t,B);
indexes(1,:)=ceil(t*rand(1,B));
bsdata(1,:)=data(indexes(1,:))';

for i=2:t
    select=rand(1,B)<p;
    indexes(i,:)=ceil((t-1)*rand(1,B)).*select+(mod(~select.*(indexes(i-1,:)),t)+1);
    bsdata(i,:)=data(indexes(i,:))';
end

for i=1:B
    scaling=1;
    d=diff(indexes);
    for j=2:length(B)-1;
        if d(j,i)~=1;
            scaling=bsdata(j,i)/bsdata(j-1,i);
        end
        bsdata(j,i)=bsdata(j,i)*scaling;
    end
end

bsdata=sqrt(bsdata).*s(indexes);