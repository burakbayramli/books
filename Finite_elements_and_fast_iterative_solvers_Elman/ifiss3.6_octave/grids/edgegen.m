function [hx,hy,eex] = edgegen(xy,ev)
%EDGEGEN    edge information for flux jump computation
%   [hx,hy,eex] = edgegen(xy,ev);
%   input
%          xy         nodal coordinate vector 
%          ev         element mapping matrix
%   output
%          hx,hy      elementwise edge lengths
%          eex        element edge index vector
%
%   IFISS function: corrected DJS; 3 January 2011.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao
nvtx=length(xy(:,1));
nel=length(ev(:,1));
x=xy(:,1); y=xy(:,2);
%% compute edge to edge connection array ee 
% initialise global matrices
      adj = sparse(nvtx,nvtx); 
      ee = zeros(nel,4);
%
% evaluate element number on each edge in turn
% and assemble into adjacency matrix 
%% nx= 0, ny=-1  
		 adj=adj + sparse(ev(:,1),ev(:,2),1:nel,nvtx,nvtx);  
%% nx= 1, ny= 0
		 adj=adj + sparse(ev(:,2),ev(:,3),1:nel,nvtx,nvtx); 
%% nx= 0, ny= 1       
		 adj=adj + sparse(ev(:,3),ev(:,4),1:nel,nvtx,nvtx); 
%% nx=-1, ny= 0
		 adj=adj + sparse(ev(:,4),ev(:,1),1:nel,nvtx,nvtx); 
%
       for el=1:nel
		   [ii,jj]=find(adj==el);
           ee(el,:)=diag(adj(jj,ii))';
       end
       ee=ee(:,[2,4,3,1]);
       
%       
% inner loop over elements    
for ivtx = 1:4
   xl_v(:,ivtx) = x(ev(:,ivtx));
   yl_v(:,ivtx) = y(ev(:,ivtx)); 
end
%
% compute local mesh sizes
hx=xl_v(:,2)-xl_v(:,1); 
hy=yl_v(:,3)-yl_v(:,2); 

fprintf('checking edge numbering and computing edge lengths ... ')
% centroid coordinates
for ielem=1:nel
xc(ielem)=mean(x(ev(ielem,1:4))); yc(ielem)=mean(y(ev(ielem,1:4)));
end
xyp=[xc',yc'];
%
% remove zero indices corresponding to boundary edges
ppk=[1:nel]'; onecol=ones(nel,1); 
% find boundary edges
iie=ee==0;
eex=ee + ( ee==0).* [ppk,ppk,ppk,ppk];
%
tic,
% check that interior edges have correct orientation
for elem=1:nel;
   xlocal=xyp(eex(elem,:),1); ylocal=xyp(eex(elem,:),2);
   xg=xyp(elem,1); yg=xyp(elem,2);
   ilist=find(xlocal==xg); jlist=find(ylocal==yg);
   [xm,i4]=min(xlocal(jlist)); e4=jlist(i4);
   [xm,i2]=max(xlocal(jlist)); e2=jlist(i2);
   [ym,i1]=min(ylocal(ilist)); e1=ilist(i1);
   [ym,i3]=max(ylocal(ilist)); e3=ilist(i3);
   if sum(iie(elem,:))==2, %% two boundary edge elements
   %fprintf('element %i has two boundary edges \n',elem)
      bedge=iie(elem,:);
      active=jlist(~bedge(jlist));
      if active==e4, e2=mod4(e4+2); else, e4=mod4(e2+2); end,
      active=ilist(~bedge(ilist));  
      if active==e1, e3=mod4(e1+2); else, e1=mod4(e3+2); end,
   end
%
%fprintf('element %i reordered via mapping [%i,%i,%i,%i] \n',elem, e1,e2,e3,e4)
   eex(elem,:)=eex(elem,[e1,e2,e3,e4]);
end
etime=toc;
%fprintf('%g seconds needed to reorientate edges \n', etime)
fprintf('done \n')
% 
return
%--------------------------------------------------------------
function nn=mod4(n)
if n<5, nn=n; else nn=mod(n,4); end
