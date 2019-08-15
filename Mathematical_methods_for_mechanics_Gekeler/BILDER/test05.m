% Testen Ersetzen
cla;

   x = [1; 2; 3; 4];
   y = x;
   plothandle=plot(x,y,'Color','red');
   drawnow
   axis([0 4 0 4]);
   set(gca,'NextPlot','add');
 for i = 1:10
   % cla
   x = [1; 2; 3; 4];
   y = x;
   plot(x,y);
%   set (plothandle,'XData',x,'YData',y);
   %axis off;
   drawnow;
   pause(1)
   X = [1; 2; 3; 4];
   Y = [1; 3; 3; 4];
   clf reset
   plothandle = newplot;
    plot(X,Y);
%   set (plothandle,'XData',X,'YData',Y);
%   drawnow
   pause(1)
end
