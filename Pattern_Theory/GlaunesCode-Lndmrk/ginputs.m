function [x,y] = ginputs

% selection graphique des points

clf
axis([0,1,0,1])
hold on
[x1,x2] = gginputs('dr');
[y1,y2] = gginputs('*r');
x = [x1;x2;zeros(1,length(x1))];
y = [y1;y2;zeros(1,length(y1))];

function [x1,x2] = gginputs(color);
xn1 = 1;
n = 0;
while ~isempty(xn1)
    n = n + 1;
    [xn1,xn2] = ginput(1);

    if ~isempty(xn1)
        plot(xn1,xn2,color)
        x1(n) = xn1;
        x2(n) = xn2;
    end
end
