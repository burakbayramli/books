function out=pltpts(xnew,xcurr)

plot([xcurr(1),xnew(1)],[xcurr(2),xnew(2)],'r-',xnew(1),xnew(2),'o','Erasemode','none');
drawnow; % Draws current graph now
%pause(1)

out = [];
