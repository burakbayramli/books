function [p1,e1,t1] = myadapt(fn,p,e,t,SOLUTION,LOADS,GRAFIK)
% adaptive mesh refinement ------
% Observe different options below
if nargin == 6, GRAFIK = 0; end
e1 = e;
errf = pdejmps(p,t,1,0,LOADS,SOLUTION,0.15,0.15,1);
it = pdeadworst(p,t,1,0,LOADS,SOLUTION,errf,0.7)
% BILD -------------------------
if GRAFIK == 1
   clf
   trimesh(t',p(1,:),p(2,:),'color','k'), hold on;
   TRI1 = t(1:3,it).';
   trimesh(TRI1,p(1,:),p(2,:),'color','r')
   axis equal %,hold on
   pause
end
% -----------------------------------------
OPTION = 2;
switch OPTION
case 1
  [p1,e1,t1] = refinemesh(fn,p,e,t,it.','longest');
  if GRAFIK == 1, pdemesh(p1,e1,t1); end
case 2  
   e = e(1:5,:); t = t(1:3,:);
   [p1,e1,t1] = mesh17(p,e,t,it,GRAFIK);            % "longest"
case 3
   e = e(1:5,:); t = t(1:3,:);
   [p1,e1,t1] = mesh01_t(fn,p,e,t,it); % "regular"
end
%-- Order boundary ------------
LL = max(e1(5,:)); AUX = [];
for I = 1:LL
   J = find(e1(5,:) == I); EE = e1(:,J);
   [U,K] = sort(EE(3,:)) ; EE = EE(:,K);
   AUX = [AUX,EE];
end
e1 = AUX;
