function h=kern_dens_plot(returns,bw,nodes,range,kern)
% PURPOSE:
%    Plots a bivariate kernel smooth
% 
% USAGE:
%     h=kern_dens_plot(returns,bw,nodes,range,kern)
% 
% INPUTS:
%     returns : T by 2 length series to generae the data
%     bw      : The bandwidth to use. If empty, silverman's BW is used
%     nodes   : Number of nodes to use
%     range   : Range to calculate the nodes over(2 by 1 vector)(can be empty, in which case the range is used)
%     kern    : String arguement of the type of kernel to be used
%             'cosinus'
%             'epanechnikov'
%             'normal'
%             'quartic'
%             'triangular'
%             'triweight'
%             'uniform'
%
% OUTPUTS:
%     h       : Graphics handle to plos
% 
% COMMENTS:
%     Uses product kernels
% 
% Author: Kevin Sheppard
% kevin.sheppard@economics.ox.ac.uk
% Revision: 2    Date: 12/31/2001

if isempty(bw)
   bw=1.06*std(returns)*length(returns)^(-1/5); 
end

if isempty(range)
    rmin=min(returns);
    rmax=max(returns);
else
    rmin=range(1);
    rmax=range(2);
end

if isempty(nodes)
    nodes=length(returns);
end

height=zeros(nodes,1);
steps=linspace(rmin,rmax,nodes);

index=1;
for i=steps
  height(index)=(1/bw)*mean(feval(kern,returns,steps(index),bw)); 
  index=index+1;
end

h=plot(steps,height);





