function sp_fig(N,width,height)
% sp_fig(N,width,height)
% Creates figure N in lower right corner of screen.
% Width and height are in inches. If they are omitted,
% they are set to about 2/3 screen size.
%
%IMPORTANT NOTE: If the figure width and height on your screen are off, it
%may be because the Matlab 'screensize' property is incorrect. To check
%this, enter the following in the command window:
%  set(0,'units','inches')
%  get(0,'screensize')
%If the resulting screen size is not the actual size of your screen, you
%can adjust it by changing the "ScreenPixelsPerInch" property as follows:
%Let w=actual width of your screen in inches. Then execute the following:
%  set(0,'units','inches')
%  ss=get(0,'screensize')
%  ppi=get(0,'screenpixelsperinch')
%  set(0,'screenpixelsperinch',ppi*ss(3)/w)
%The "screensize" property should now be correct. Alternatively, you can
%make the 4 expressions above part of your "startup.m" file.

% Set the figure size and position.
set(0,'units','inches');
ss=get(0,'screensize');              	%screensize vector (inches)
if nargin < 3,
    width=.7*ss(3);
    height=.7*ss(4);
end
x0=.8*ss(3)/4;                          %lower left corner
y0=.75;
h=figure(N);
clf(N);
set(h,'units','inches')
set(h,'position',[x0 y0 width height])

%set the defaults
set(gcf,'color',[1 1 1]);
set(gcf,'defaultaxesfontname','times');
set(gcf,'defaultaxesfontsize',12);
