function splash
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% usage: [-] =  SPLASH(-)
%
% Displays the splash screen on startup of Tornado. This
% Should usually contain the latest development info and 
% Important code changes. The image is displayed in figure
% 100.
%
% Example:
%
%  [void] = splash (void);
%
% Calls:
%       none
%
% Author: Tomas Melin <melin@kth.se>
% Keywords: Splash screen.
%
% Revision History:
%   Bristol, 2007-06-27:  Addition of new header.   TM
%                         Made into a function.     TM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


clear
A=imread('splash.jpg');
h=figure(100);
set(gca,'Position',[0 0 1 1])
set(h,'ToolBar','none');
set(h,'MenuBar','none');
set(h,'Color','White');
set(h,'Name','Tornado startup splash screen.');
axis off
h2=image(A);
axis off
end%function