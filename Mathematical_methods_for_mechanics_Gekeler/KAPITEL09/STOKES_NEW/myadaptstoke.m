function [pn,en,tn] = myadaptstoke(p,e,t,U,V,P)
%function myadaptstoke

N1 = size(p,2);
U1 = U(1:N1); V1 = V(1:N1);

%xlin    = linspace(min(X),max(X),30);
%ylin    = linspace(min(Y),max(Y),30);
%[X1,Y1] = meshgrid(xlin,ylin);
%W1      = griddata(X,Y,Z1,X1,Y1,'cubic');
GRAFIK = 1;
if GRAFIK == 1
   clf, hold on
   trimesh(t(1:3,:).',p(1,:),p(2,:),zeros(1,length(p(2,:))),'edgecolor','g'), hold on
   for I = 1:size(e,2)
      A = [p(1,e(1,I));p(1,e(2,I))];
      B = [p(2,e(1,I));p(2,e(2,I))];
      plot(A,B,'r','linewidth',2), hold on
   end
   axis equal, axis manual
end
flag = 2;
if flag == 1
   errfu = pdejmps(p,t,1,0,0,U1,0.15,0.15,1);
   errfv = pdejmps(p,t,1,0,0,V1,0.15,0.15,1);
   errf = max(errfu,errfv);
   it1 = pdeadworst(p,t,1,0,0,U1,errf,0.2) % 0.7
   it2 = pdeadworst(p,t,1,0,0,V1,errf,0.2) % 0.7
   it = it1;
    %errfp = pdejmps(p,t,1,0,0,P,0.15,0.15,1);
   %it = pdeadworst(p,t,1,0,0,U1,errfp,0.5) % 0.7
end
if flag == 2
   out_bound = find(e(7,:) == 0);
   out_bound = sort(out_bound);
   out_bound = e(:,out_bound);
   RAND = out_bound(1,:);
   N = size(p,2); M = size(t,2); KK = zeros(1,M);
   for I = 1:length(RAND)
      for J = 1:M
         if ismember(RAND(I),t(1:3,J)) & KK(J) == 0
            KK(J) = J;
         end
      end
   end   
   L1 = find(KK ~= 0); it  = KK(L1)
end
if GRAFIK == 1
% BILD -------------------------
   clf
   trimesh(t.',p(1,:),p(2,:),'color','k'), hold on;
   TRI1 = t(1:3,it).';
   trimesh(TRI1,p(1,:),p(2,:),'color','r')
   axis equal %,hold on
   pause
% -----------------------------------------
  %  [p1,e1,t1] = mesh17(p,e,t,it,1);            % "longest"
  e2 = e(1:7,:);
 [p1,e1,t1] = refinemesh('bsp01g',p,e2,t,it.','regular');
  clf
  pdemesh(p1,e1,t1);
  pause
end
pn = p1; en = e1; tn = t1;
