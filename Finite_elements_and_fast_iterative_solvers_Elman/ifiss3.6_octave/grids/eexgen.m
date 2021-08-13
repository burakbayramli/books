function [hx,hy,eex] = eexgen(xy,xyp,mv,ee)
%EEXGEN Q2-P1 reorientation for flux jump computation
%   [hx,hy,eex] = eexgen(xy,xyp,mv,ee);
%   input
%          xy         Q2 nodal coordinate vector 
%          xyp        Q0 element coordinate vector
%          mv         Q2 element mapping matrix
%          ee         element edge index vector
%   output
%          hx,hy      elementwise edge lengths
%          eex        renumbered edge set
%
%   IFISS function: corrected DJS; 28 September 2013.
% Copyright (c) 2001 D.J. Silvester, Qifeng Liao
x=xy(:,1); y=xy(:,2);
xp=xyp(:,1); yp=xyp(:,2);
nvtx=length(x); nu=2*nvtx; np=length(xp);
nel=length(mv(:,1));
fprintf('checking edge numbering and computing edge lengths ... ')
hx=zeros(nel,1); hy=zeros(nel,1);
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
%
% inner loop over elements    
for ivtx = 1:9
   xl_v(:,ivtx) = x(mv(:,ivtx));
   yl_v(:,ivtx) = y(mv(:,ivtx)); 
end
%
% compute local mesh sizes
hx(:)=xl_v(:,2)-xl_v(:,1); 
hy(:)=yl_v(:,3)-yl_v(:,2);
fprintf('done\n')
return
%--------------------------------------------------------------
function nn=mod4(n)
if n<5, nn=n; else nn=mod(n,4); end
