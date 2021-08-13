function macrogridplot(xy,mv,bound,mbound)
%MACROGRIDPLOT quadrilateral macroelement grid verification
%   macrogridplot(xy,mv,bound,mbound);
% 
%   IFISS function: DJS; 7 May 2006, 5 January 2011.
% Copyright (c) 2006 D.J. Silvester, H.C. Elman, A. Ramage 
fprintf('\nSubdivision logistics ..\n')
nvtx=length(xy(:,1)); 
fprintf('  %g nodes \n',nvtx)
nelement=length(mv(:,1));
fprintf('  %g (2x2 macro)elements \n',nelement)
nboundvtx=length(bound);
fprintf('  %g nodes on Dirichlet boundary \n',nboundvtx)
nboundedge=length(mbound(:,1));
fprintf('  %g macroelement edges on Dirichlet boundary\n\n',nboundedge)
%
%
adj=sparse(nvtx,nvtx); adx=sparse(nvtx,nvtx);
for i=1:nelement
   adj(mv(i,1),mv(i,5))=1;
   adj(mv(i,5),mv(i,2))=1;
   adj(mv(i,2),mv(i,6))=1;
   adj(mv(i,6),mv(i,3))=1;
   adj(mv(i,3),mv(i,7))=1;
   adj(mv(i,7),mv(i,4))=1;
   adj(mv(i,4),mv(i,8))=1;
   adj(mv(i,8),mv(i,1))=1;
   adj(mv(i,9),mv(i,5))=1; adx(mv(i,9),mv(i,5))=1;
   adj(mv(i,9),mv(i,6))=1; adx(mv(i,9),mv(i,6))=1;
   adj(mv(i,9),mv(i,7))=1; adx(mv(i,9),mv(i,7))=1;
   adj(mv(i,9),mv(i,8))=1; adx(mv(i,9),mv(i,8))=1;
end
%
%% define element edges
adjb=sparse(nvtx,nvtx);
% bottom boundary edges
k1=find(mbound(:,2)==1)';
for k=mbound(k1)
   adjb(mv(k,1),mv(k,5))=1;
   adjb(mv(k,5),mv(k,2))=1;
end
% right boundary edges
k2=find(mbound(:,2)==2)';
for k=mbound(k2)
   adjb(mv(k,2),mv(k,6))=1;
   adjb(mv(k,6),mv(k,3))=1;
end
% top boundary edges
k3=find(mbound(:,2)==3)';
for k=mbound(k3)
   adjb(mv(k,3),mv(k,7))=1;
   adjb(mv(k,7),mv(k,4))=1;
end
% left boundary edges
k4=find(mbound(:,2)==4)';
for k=mbound(k4)
   adjb(mv(k,4),mv(k,8))=1;
   adjb(mv(k,8),mv(k,1))=1;
end
%
figure(1)
gplot(adj,xy,'b');
hold on
gplot(adx,xy,'c');
stnode=int2str([1:nvtx]');
text(xy(:,1),xy(:,2),stnode)
axis('equal'),axis('off')
title('Indices of nodes of the macroelement grid')
hold off
figure(2)
			   gplot(adj,xy,'b');
hold on
			   gplot(adx,xy,'c');
			   gplot(adjb,xy,'r');
xybd=xy(bound,:);
stbd=int2str([1:nboundvtx]');
text(xybd(:,1),xybd(:,2),stbd,'color','black')
title('Indices of nodes on the Dirichlet boundary')
axis('equal'),axis('off')
hold off
return
