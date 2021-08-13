function [err_p,elerr_p] = cdpost_bc(viscosity,aez,fez,elerror,xy,ev,ebound)
%CDPOST_BC postprocesses local Poisson error estimator 
%   [err_p,elerr_p] = cdpost_bc(viscosity,aez,fez,elerror,xy,ev,ebound);
%   input
%          viscosity   viscosity parameter 
%          aez         elementwise Poisson problem matrices
%          fez         elementwise rhs vectors
%          elerror     elementwise error estimate (without BC imposition) 
%          xy          vertex coordinate vector  
%          ev          element mapping matrix
%          ebound      element edge boundary matrix 
%   output
%          err_p       global error estimate 
%          elerr_p     elementwise error estimate
%   IFISS function: DJS; 5 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      x=xy(:,1); y=xy(:,2);
      nel=length(ev(:,1));
      lev=[ev,ev(:,1)]; elerr_p=elerror;
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
      [bae,fe] = localbc_pcd(ae,fe,edges,xl,yl);
% solve local problem
        err=(viscosity*bae)\fe;
        elerr_p(el2e,1) = err'*fe/viscosity;
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
      [bae,fe] = localbc_pcd(ae,fe,edges,xl,yl);
% solve for local estimate 
        err=(viscosity*bae)\fe;
        elerr_p(el1e,1) = err'*fe/viscosity;
      end
% end of element loop
%
      err_p = sqrt(sum(elerr_p));
      elerr_p = sqrt(elerr_p);
      fprintf('done\n')
      fprintf('estimated global error (in energy):  %10.6e\n',err_p)   
 return
