function gridplot(xy,ev,bound,ebound)
%GRIDPLOT   quadrilateral grid verification
%   gridplot(xy,ev,bound,ebound);
% 
%   IFISS function: DJS; 7 May 2006, 5 January 2011.
% Copyright (c) 2006 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('\nGrid logistics ..\n')
nvtx=length(xy(:,1)); 
fprintf('  %g nodes \n',nvtx)
nelement=length(ev(:,1));
fprintf('  %g elements \n',nelement)
nboundvtx=length(bound);
fprintf('  %g nodes on Dirichlet boundary \n',nboundvtx)
nboundedge=length(ebound(:,1));
fprintf('  %g element edges on Dirichlet boundary \n\n',nboundedge)
%
%
adj=sparse(nvtx,nvtx); adx=sparse(nvtx,nvtx);
for i=1:nelement
   adj(ev(i,1),ev(i,2)) =1;
   adj(ev(i,2),ev(i,3)) =1;  
   adj(ev(i,3),ev(i,4)) =1;
   adj(ev(i,4),ev(i,1)) =1;
end
%
%% define element edges
adjb=sparse(nvtx,nvtx);
% bottom boundary edges
k1=find(ebound(:,2)==1)';
for k=ebound(k1)
   adjb(ev(k,1),ev(k,2))=1;
end
% right boundary edges
k2=find(ebound(:,2)==2)';
for k=ebound(k2)
   adjb(ev(k,2),ev(k,3))=1;
end
% top boundary edges
k3=find(ebound(:,2)==3)';
for k=ebound(k3)
   adjb(ev(k,3),ev(k,4))=1;
end
% left boundary edges
k4=find(ebound(:,2)==4)';
for k=ebound(k4)
   adjb(ev(k,4),ev(k,1))=1;
end
%
figure(1)
gplot(adj,xy,'b');
hold on
stnode=int2str([1:nvtx]');
text(xy(:,1),xy(:,2),stnode)
axis('equal'),axis('off')
title('Indices of nodes of the element grid')
hold off
figure(2)
gplot(adj,xy,'b')
hold on
gplot(adjb,xy,'r')
xybd=xy(bound,:);
stbd=int2str([1:nboundvtx]');
text(xybd(:,1),xybd(:,2),stbd,'color','black')
title('Indices of nodes on the Dirichlet boundary')
axis('equal'),axis('off')
hold off
return
