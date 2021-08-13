function [elerr_p] = diffpost_bc(elerror,fez,xy,ev,ebound);
%DIFFPOST_BC postprocesses local Poisson error estimator 
%   [errorsq_cbc] = diffpost_bc(errorsq_ele,fe,xy,ev,ebound);
%   input
%          errorsq_ele   element error estimate (without BC imposition) 
%          fe            elementwise rhs vectors
%          xy            vertex coordinate vector  
%          ev            element mapping matrix
%          ebound        element edge boundary matrix 
%   output
%          errorsq_cbc    element error estimate with BC correction
%
%   calls functions gausspoints_oned, gausspoints_twod
% Rebuilt in IFISS3.1 to retain compatibility with earlier versions
%   IFISS function: DJS; 4 January 2011.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      x=xy(:,1); y=xy(:,2);
      nel=length(ev(:,1));
      lev=[ev,ev(:,1)]; elerr_p=elerror;
%
% recompute the Q2 matrices
% reconstruct the element matrices
% construct the integration rule
ngpt=3;
[oneg,onew] = gausspoints_oned(ngpt);
[s,t,wt] = gausspoints_twod(oneg,onew);
nngpt=ngpt^2; ng=ngpt; 
tic
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(ev(:,ivtx));
        yl_v(:,ivtx) = y(ev(:,ivtx)); 
		end
        aez = zeros(nel,5,5);
% loop over Gauss points
         for igpt = 1:nngpt
         sigpt=s(igpt);
         tigpt=t(igpt);
         wght=wt(igpt);
% evaluate derivatives etc
         [jac_v,invjac_v,phi_v,dphidx_v,dphidy_v] = deriv(sigpt,tigpt,xl_v,yl_v);
         [psi_v,dpsidx_v,dpsidy_v] = qderiv(sigpt,tigpt,xl_v,yl_v);        
            for j = 1:5
               for i = 1:5
               aez(:,i,j) = aez(:,i,j)+wght*dpsidx_v(:,i+4).*dpsidx_v(:,j+4).*invjac_v(:);
               aez(:,i,j) = aez(:,i,j)+wght*dpsidy_v(:,i+4).*dpsidy_v(:,j+4).*invjac_v(:);
               end 
            end
% end of Gauss point loop
         end
      
%
% recompute contributions from elements with Dirichlet boundaries
      nbde=length(ebound(:,1));
      ebdy = zeros(nel,1);
      edge = zeros(nel,1);
% isolate boundary elements
      for el = 1:nbde
      ee = ebound(el,1);
      ebdy(ee) = ebdy(ee)+1; edge(ee)=ebound(el,2);
      end  
%
% two edge elements
      k2=find(ebdy==2);
      nel2b=length(k2);
% loop over two edge elements
      for el = 1:nel2b
      el2e=k2(el);
      kk=find(ebound(:,1) == el2e);
      edges=ebound(kk,2);
% set up original matrix and RHS vector
	  ae=squeeze(aez(el2e,1:5,1:5)); 
      fe=fez(el2e,:)';
% set up local coordinates and impose interpolated error as Dirichlet bc
      xl=x(lev(el2e,:)); yl=y(lev(el2e,:)); 
      [bae,fe] = localbc_p(ae,fe,edges,xl,yl);
% solve local problem
      err=bae\fe;
      elerr_p(el2e,1) = err'*fe;
      end
% end of element loop
%
% one edge elements
      k1=find(ebdy==1);
      nel1b=length(k1);
% loop over one edge elements
      for el = 1:nel1b
      el1e=k1(el);
      kk=find(ebound(:,1) == el1e);
      edges=ebound(kk,2);
% set up original matrix and RHS vector 
      fe=fez(el1e,:)';
	  ae=squeeze(aez(el1e,1:5,1:5)); 
% set up local coordinates and impose interpolated error as Dirichlet bc
      xl=x(lev(el1e,:)); yl=y(lev(el1e,:));
      [bae,fe] = localbc_p(ae,fe,edges,xl,yl);
% solve local problem
      err=bae\fe;
      elerr_p(el1e,1) = err'*fe;
      end
% end of element loop
%
      etime=toc;
      fprintf('error boundary correction took %6.3e seconds\n',etime)  
 return
















