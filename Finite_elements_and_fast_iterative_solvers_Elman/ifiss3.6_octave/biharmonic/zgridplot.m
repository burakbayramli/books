%ZGRIDPLOT   BFS square grid illustration
%   IFISS function: DJS; 8 September 2018.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi

fprintf('Grid logistics ..\n')
nvtx=length(xy(:,1)); 
fprintf('  %g nodes |',nvtx)
nelement=length(ev(:,1));
fprintf('  %g elements \n',nelement)
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
k1=find(ebound(:,2)==1)';
for k=ebound(k1)
   adjb(ev(k,2),ev(k,3))=1;
end
k2=find(ebound(:,2)==2)';
for k=ebound(k2)
   adjb(ev(k,3),ev(k,1))=1;
end
k3=find(ebound(:,2)==3)';
for k=ebound(k3)
   adjb(ev(k,1),ev(k,2))=1;
end

%
figure(1)
gplot(adj,xy,'k')
hold on
% element
v=ev(6,:);
colormap bone
fill(xy(v,1),xy(v,2),[0.85,0.85,0.85])
colormap default
axis('equal'),axis('off')
%text(0.05,-0.95,'1')
%text(0.95,-0.95,'2')
%text(0.95,-0.05,'3')
%text(0.05,-0.05,'4')
%
%
[bound,bc]=plate_bc(xy);
int1=setdiff([1:nvtx]',bound);
plot(xy(int1,1),xy(int1,2),'ob','MarkerSize',12)
int2=setdiff([nvtx+1:2*nvtx]',bound)-nvtx; nint2=ones(length(int2),1);
quiver(xy(int2,1),xy(int2,2),nint2,0*nint2,0.2,'Linewidth',1.3,'Color','black','MaxHeadSize',0.5)
%plot(xy(int2,1),xy(int2,2),'>k','MarkerSize',10)
int3=setdiff([2*nvtx+1:3*nvtx]',bound)-2*nvtx; nint3=ones(length(int3),1);
quiver(xy(int3,1),xy(int3,2),0*nint3,nint3,0.2,'Linewidth',1.3,'Color','black','MaxHeadSize',0.5)
%plot(xy(int3,1),xy(int3,2),'^k','MarkerSize',10)
int4=[1:nvtx]';
plot(xy(int4,1),xy(int4,2),'xk','MarkerSize',10)
%
stnode=int2str([1:nvtx]');
text(xy(:,1)+0.035,xy(:,2)+0.06,stnode,'FontSize',12);
%text(xy(1,1)-0.1,xy(1,2)-0.05,'1','FontSize',12);
%text(xy(2,1)-0.1,xy(2,2)+0.05,'2','FontSize',12);
%
%t=text('string','$\partial\Omega_N$','interpreter','latex','pos',[0.35,0.6]);
%               t(1).Color='black'; t(1).FontSize=14;
axis('equal'),axis('off')
%axis('square')
               
hold off
return
