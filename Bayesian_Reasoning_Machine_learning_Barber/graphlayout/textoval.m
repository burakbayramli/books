function [t, wd] = textoval(x, y, str)
% TEXTOVAL		Draws an oval around text objects
%
%  [T, WIDTH] = TEXTOVAL(X, Y, STR)
%  [..] = TEXTOVAL(STR)  % Interactive
%
% Inputs :
%    X, Y : Coordinates
%    TXT  : Strings
%
% Outputs :
%    T : Object Handles
%    WIDTH : x and y Width of ovals
%
% Usage Example : [t] = textoval('Visit to Asia?');
%
%
% Note     :
% See also TEXTBOX

% Uses :

% Change History :
% Date		Time		Prog	Note
% 15-Jun-1998	10:36 AM	ATC	Created under MATLAB 5.1.0.421

% ATC = Ali Taylan Cemgil,
% SNN - University of Nijmegen, Department of Medical Physics and Biophysics
% e-mail : cemgil@mbfys.kun.nl

temp = [];

switch nargin,
    case 1,
        str = x;
        if ~isa(str,'cell') str=cellstr(str); end;
        N = length(str);
        wd = zeros(N,2);
        for i=1:N,
            [x, y] = ginput(1);
            tx = text(x,y,str{i},'HorizontalAlignment','center','VerticalAlignment','middle');
            [ptc wx wy] = draw_oval(tx, x, y);
            wd(i,:) = [wx wy];
            delete(tx);
            tx = text(x,y,str{i},'HorizontalAlignment','center','VerticalAlignment','middle');
            temp = [temp ; tx ptc];
        end;
    case 3,
        if ~isa(str,'cell') str=cellstr(str); end;
        N = length(str);
        wd = zeros(N,2);
        for i=1:N,
            tx = text(x(i),y(i),str{i},'HorizontalAlignment','center','VerticalAlignment','middle');
            [ptc wx wy] = draw_oval(tx, x(i), y(i));
            wd(i,:) = [wx wy];
            delete(tx);
            tx = text(x(i),y(i),str{i},'HorizontalAlignment','center','VerticalAlignment','middle');
            temp = [temp;  tx ptc];
        end;
    otherwise,
end;

if nargout>0, t = temp; end;


function [ptc, wx, wy] = draw_oval(tx, x, y)
% Draws an oval box around a tex object
if isoctave
    pos=get(gcf,'Position');
    xl=pos(3);
    yl=pos(4);
    l = length(get(tx,'string')); % octave workaround: db
    sz = [1 1 0.011*(4+l)*560/xl 0.012*(4)*420/yl];
else
    sz = get(tx,'Extent'); % octave doesn't understand this
end
wy = sz(4);
wx = max(2/3*sz(3), wy);
ptc = ellipse(x, y, wx, wy);
set(ptc, 'FaceColor','w');