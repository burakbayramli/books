function hinton(M,varargin)
%HINTON Plot a Hinton diagram
% hinton(M,opts)
% Plot a Hinton diagram for matrix M. Positive is Green, and negative Red
% use hinton(M,1) to turn on the grid
% opts.grid = 1/0
% opts.coloursRGB: 2 x 4 matrix of RGB colours for the patches. First row is the RGB for
% the positive and the second row for the negative entries. Default is
% green and red.
opts=[];if nargin==2; opts=varargin{1}; end
opts=setfields(opts,'grid',1,'coloursRGB',[0 1 0; 1 0 0]);% default options
[x, y, col]=makepatch(flipud(M));
cla
for count=1:size(x,1);
    if col(count)==1
        c=opts.coloursRGB(1,:);
    else
        c=opts.coloursRGB(2,:);
    end
    patch(y(count,:),x(count,:),c);
end
[I J]=size(M);
set(gca,'xtick',[1:J])
set(gca,'xticklabel',[1:J])
set(gca,'ytick',[1:I])
set(gca,'yticklabel',[I:-1:1])
set(gca,'box','on')
axis([0 J+1 0 I+1]);
if opts.grid
    grid on
else
    grid off
end
function [x, y, col]=makepatch(M)
MM=max(max(abs(M)));
M2=M./MM;
M2=abs(M2);
count=0;
for i=1:size(M,1)
    for j=1:size(M,2)
        count=count+1;
        xx = i + M2(i,j)*0.45*[-1 -1 1 1];
        yy = j + M2(i,j)*0.45*[-1 1 1 -1];
        x(count,:)=xx;
        y(count,:)=yy;
        col(count)=sign(M(i,j));
    end
end
