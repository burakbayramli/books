function [ev,ebound]=q1grid(xy,mv,bound,mbound);
%Q1GRID bilinear element grid generator
%   [ev,ebound]=q1grid(xy,mv,bound,mbound);
%   input
%          xy         vertex coordinate vector  
%          mv         Q2 macroelement mapping matrix
%          bound      boundary vertex vector
%          mbound     macroelement boundary edge vector 
%   output
%          ev         element mapping matrix
%          ebound     element boundary mapping matrix
%
%   IFISS function: DJS; 28 February 2005, HCE; 9 January 2019.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
xx=xy(:,1); yy=xy(:,2); nvtx=length(xx);
adj=sparse(nvtx,nvtx);
mel=length(mv(:,1)); nel=4*mel;
ev=zeros(nel,4);
%
%% loop over macroelements
for k=1:mel
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
end
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
%
% plotting of the grid 
%
%if mel <=256,
	%adj=sparse(nvtx,nvtx);
    %{
    for i=1:nel
	adj(ev(i,1),ev(i,2)) =1;
	adj(ev(i,2),ev(i,3)) =1;
	adj(ev(i,3),ev(i,4)) =1;
	adj(ev(i,4),ev(i,1)) =1;
    end
    %}
    I = [ev(:,1); ev(:,2); ev(:,3); ev(:,4)];
    J = [ev(:,2); ev(:,3); ev(:,4); ev(:,1)];
    adj = sparse(I, J, 1, nvtx, nvtx);
    
    
    figure(10)
    gplot(adj,xy,'b');
    axis('square')
    hold on
    plot(xy(:,1),xy(:,2),'ro')
%   axis('off')
    xybd=xy(bound,:);
    plot(xybd(:,1),xybd(:,2),'ko')
    hold off
    title('Q1 finite element subdivision');
    drawnow
    
%end
return
