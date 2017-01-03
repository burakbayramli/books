function gr=k_pdf(X,type,nb,range,bw)
% PURPOSE: Plots a univariate kernel density
% 
% USAGE:
%     gr=kpdf(X,nb,range,bw)
% 
% INPUTS:
%     X       : series to generate the data
%     type    : The type of kernel used, by numbers, default is epanechnikov
%             cosinus       :   0
%             uniform       :   1
%             normal        :   2
%             quartic       :   3   
%             triangular    :   4   
%             triweight     :   5
%             epanechnikov  :   6
%     nb      : Number of nodes to use ; default is min(lenght X,300) 
%     range   : Range to calculate the nodes over(2 by 1 vector)(can be omitted, 
%               in which case an expanded range is used)
%     bw      : The bandwidth to use. by default Silverman's BW is used
%
% OUTPUTS:
%     gr       : Graphics handle to plot
% 
% COMMENTS:
%
% Author: François Desmoulins-Lebeault
% fd-l@caramail.com
% Revision: 2    Date: 01/05/2003

[N,p]=size(X);
if p~=1
    X=X';
    N=p;
end

if nargin < 2
     type = 6;
end

if nargin < 3
    nb=min(N,300);
end

if nargin < 4
    rmin=min(X)-.5*std(X);
    rmax=max(X)+.5*std(X);
else
    rmin=range(1);
    rmax=range(2);
end

if nargin < 5
  switch type
    case 0
        k=2.2810;
    case 1
        k=1.7291;
    case 2
        k=0.9933;
    case 3
        k=2.6121;
    case 4
        k=2.4190;
    case 5
        k=2.9616;
    otherwise
        k=2.2074;
  end
  bw=0.99*k*N^(-1/5)*min(std(X),iqr(X)/1.34);
end
    
vals=zeros(nb,1);
x=linspace(rmin,rmax,nb);

for i=1:nb
    u=(repmat(x(i),N,1)-X)./bw;
    switch type
        case 0
            vals(i)=mean((pi/4)*cos((pi/2)*u).*(abs(u)<1))/bw;
        case 1
            vals(i)=mean(.5*(abs(u)<1))/bw;
        case 2
            vals(i)=mean((1/sqrt(2*pi))*exp(-.5*u.^2))/bw;
        case 3
            vals(i)=mean((15/16)*(ones(N,1)-u.^2).^2.*(abs(u)<1))/bw;
        case 4
            vals(i)=mean((ones(N,1)-abs(u)).*(abs(u)<1))/bw;
        case 5
            vals(i)=mean((35/32)*(ones(N,1)-u.^2).^3.*(abs(u)<1))/bw;
        otherwise
            vals(i)=mean((3/4)*(ones(N,1)-u.^2).*(abs(u)<1))/bw;
    end
end

gr=plot(x,vals);