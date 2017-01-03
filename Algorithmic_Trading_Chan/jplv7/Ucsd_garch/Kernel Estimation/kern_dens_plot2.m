function [h]=kern_dens_plot2(returns,bw,nodes,range,kern)
% PURPOSE:
%    Plots a bivariate kernel smooth
% 
% USAGE:
%     h=kern_dens_plot2(returns,bw,nodes,range,kern)
% 
% INPUTS:
%     returns : T by 2 length series to generae the data
%     bw      : The bandwidth to use(can be scalar or a 2 element vector) If empty, silverman's BW is used
%     nodes   : Numbr of nodes to use, can be scalar ot a 2 element vetor.
%     range   : Range to calculate the nodes over(can be empty, in which case the range is used)[xmin ymin;xmax ymax];
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


[t,k]=size(returns);

if isempty(bw)
    bw1=1.06*std(returns(:,1))*length(returns)^(-1/5); 
    bw2=1.06*std(returns(:,2))*length(returns)^(-1/5); 
elseif length(bw)==1
    bw1=bw;
    bw2=bw;
else
    bw1=bw(1);
    bw2=bw(2);
end

if isempty(range)
    xmin=min(returns(:,1));
    xmax=max(returns(:,1));
    ymin=min(returns(:,2));
    ymax=max(returns(:,2));
else
    xmin=range(1);
    xmax=range(2);
    ymin=range(3);
    ymax=range(4);
end

if isempty(nodes)
    nodes=length(returns);
end
    
    
    
xsteps=linspace(xmin,xmax,nodes)';
ysteps=linspace(xmin,xmax,nodes);

tempx=repmat(xsteps,1,nodes);
tempy=repmat(ysteps,nodes,1);
tempx=reshape(tempx,nodes^2,1);
tempy=reshape(tempy,nodes^2,1);
points=[tempx tempy];

heights=zeros(nodes,nodes);

xholder=zeros(t,nodes);
yholder=zeros(t,nodes);

for i=1:nodes;
    xholder(:,i)=(feval(kern,returns(:,1),xsteps(i),bw1));%normpdf(returns(:,1),xsteps(i),bw);
    yholder(:,i)=(feval(kern,returns(:,2),ysteps(i),bw2));
end

for i=1:nodes;
    heights(i,:)=sqrt((1/bw1)*(1/bw2))*mean(repmat(xholder(:,i),1,nodes).*yholder);        
end
    

[h]=surf(xsteps,ysteps,heights);

shading interp;
