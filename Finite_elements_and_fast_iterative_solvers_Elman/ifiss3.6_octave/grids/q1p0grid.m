function [ev,ee,ebound,xyp] = q1p0grid(x,y,xy,mv,bound,mbound);
%Q1P0GRID Q1-P0 element grid generator
%   [ev,ee,ebound,xyp] = q1p0grid(x,y,xy,mv,bound,mbound);
%   input
%          x          x coordinate vector
%          y          y coordinate vector 
%          xy         nodal coordinate vector  
%          mv         Q2 macroelement mapping matrix
%          bound      boundary vertex vector
%          mbound     macroelement boundary vertex vector
%   output       
%          ev         element vertex matrix
%          ee         element edge connection matrix
%          ebound     element boundary edge matrix   
%          xyp        vertex coordinate vector
%
%   IFISS function: DJS; 28 February 2005, 1 May 2012.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
xx=xy(:,1); yy=xy(:,2); nvtx=length(xx);
adj=sparse(nvtx,nvtx);
mel=length(mv(:,1)); nel=4*mel;
ev=zeros(nel,4);
%
%% loop over macroelements
k=1:mel;
% first element
ke=4*k-3;
ev(ke,1)=mv(k,1);
ev(ke,2)=mv(k,5);
ev(ke,3)=mv(k,9);
ev(ke,4)=mv(k,8);
% second element
ke=4*k-2;
ev(ke,1)=mv(k,5);
ev(ke,2)=mv(k,2);
ev(ke,3)=mv(k,6);
ev(ke,4)=mv(k,9);
% third element
ke=4*k-1;
ev(ke,1)=mv(k,9);
ev(ke,2)=mv(k,6);
ev(ke,3)=mv(k,3);
ev(ke,4)=mv(k,7);
% fourth element
ke=4*k;
ev(ke,1)=mv(k,8);
ev(ke,2)=mv(k,9);
ev(ke,3)=mv(k,7);
ev(ke,4)=mv(k,4);
%
%% define element edges
ect=1;
% bottom boundary edges
k1=find(mbound(:,2)==1)';
for k=mbound(k1)
   ebound(ect,1)=4*k-3; ebound(ect+1,1)=4*k-2; 
   ebound(ect,2)=1    ; ebound(ect+1,2)=1;
   ect=ect+2;
end
% right boundary edges
k2=find(mbound(:,2)==2)';
for k=mbound(k2)
   ebound(ect,1)=4*k-2; ebound(ect+1,1)=4*k-1; 
   ebound(ect,2)=2    ; ebound(ect+1,2)=2;
   ect=ect+2;
end
% top boundary edges
k3=find(mbound(:,2)==3)';
for k=mbound(k3)
   ebound(ect,1)=4*k-1; ebound(ect+1,1)=4*k; 
   ebound(ect,2)=3    ; ebound(ect+1,2)=3;
   ect=ect+2;
end
% left boundary edges
k4=find(mbound(:,2)==4)';
for k=mbound(k4)
   ebound(ect,1)=4*k; ebound(ect+1,1)=4*k-3; 
   ebound(ect,2)=4    ; ebound(ect+1,2)=4;
   ect=ect+2;
end
%%
% centroid coordinates
for ielem=1:nel
xc(ielem)=mean(xx(ev(ielem,:))); yc(ielem)=mean(yy(ev(ielem,:)));
end
xyp=[xc',yc'];
%
%% compute edge to edge connection array ee 
      np=nel;
% initialise global matrices
      adj = sparse(nvtx,nvtx); 
      ee = zeros(nel,4);
%
% evaluate element number on each edge in turn
% and assemble into adjacency matrix 
%% nx= 0, ny=-1  
		 adj=adj + sparse(ev(:,1),ev(:,2),1:np,nvtx,nvtx);  
%% nx= 1, ny= 0
		 adj=adj + sparse(ev(:,2),ev(:,3),1:np,nvtx,nvtx); 
%% nx= 0, ny= 1       
		 adj=adj + sparse(ev(:,3),ev(:,4),1:np,nvtx,nvtx); 
%% nx=-1, ny= 0
		 adj=adj + sparse(ev(:,4),ev(:,1),1:np,nvtx,nvtx); 
%
       for el=1:nel
		   [ii,jj]=find(adj==el);
           ee(el,:)=diag(adj(jj,ii))';
		   end
           ee=ee(:,[2,4,3,1]);
%
% plotting of the grid 
%
%if mel <=64,
	adj=sparse(nvtx,nvtx);
    for i=1:nel
	adj(ev(i,1),ev(i,2)) =1;
	adj(ev(i,2),ev(i,3)) =1;
	adj(ev(i,3),ev(i,4)) =1;
	adj(ev(i,4),ev(i,1)) =1;
    end
    figure(30)
    gplot(adj,xy,'b');
    axis('square')
    hold on
    adj=sparse(nvtx,nvtx);
    k1=find(ebound(:,2)==1);
    for k=1:length(k1)
    kk=ebound(k1(k));
    adj(ev(kk,1),ev(kk,2))=1;
    end
    k2=find(ebound(:,2)==2);
    for k=1:length(k2)
    kk=ebound(k2(k));
    adj(ev(kk,2),ev(kk,3))=1;
    end
    k3=find(ebound(:,2)==3);
    for k=1:length(k3)
    kk=ebound(k3(k));
    adj(ev(kk,3),ev(kk,4))=1;
    end
    k4=find(ebound(:,2)==4);
    for k=1:length(k4)
    kk=ebound(k4(k));
    adj(ev(kk,4),ev(kk,1))=1;
    end
%   gplot(adj,xy,'r')
%   axis('off')
plot(xy(:,1),xy(:,2),'ro')
xybd=xy(bound,:);
plot(xybd(:,1),xybd(:,2),'ko')
plot(xyp(:,1),xyp(:,2),'k*')
hold off
title('Q1-P0 finite element subdivision')
drawnow, pause(5), set(gcf,'Visible','off'), drawnow
%end
return
