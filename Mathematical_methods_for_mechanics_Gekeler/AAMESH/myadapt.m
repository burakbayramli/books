function [p1,e1,t1] = myadapt(fn,p,e,t,Z,TOL)
% adaptive Netzverfeinerung ------
% Kriterium. Max. Differenz der Werte
% von Z in den Ecken
e1 = e;
Z1 = Z(t(1,:))'; Z2 = Z(t(2,:))'; Z3 = Z(t(3,:))';
U  = max([Z1;Z2;Z3]); V = min([Z1;Z2;Z3]);
DIFF  = abs(U-V);
disp('Dreiecke mit Maxdiff > TOL:')
it  = find(DIFF > TOL*max(DIFF));
it  = find(DIFF > TOL)
% BILD -------------------------
bildflag = 1;
if bildflag == 1
   clf
   trimesh(t',p(1,:),p(2,:),'color','k'), hold on;
   TRI1 = t(1:3,it)';
   trimesh(TRI1,p(1,:),p(2,:),'color','r')
   axis equal
end
pause
% -----------------------------------------
%[p1,t1] = mesh17(p,t,it);            % "longest"
[p1,e1,t1] = mesh01_t(fn,p,e,t,it); % "regular"

