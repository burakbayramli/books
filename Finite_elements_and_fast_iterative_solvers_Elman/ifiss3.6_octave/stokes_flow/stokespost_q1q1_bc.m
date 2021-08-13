function [error_x,error_y] = stokespost_q1q1_bc(aez,fezx,fezy,elerrorx,elerrory,xy,ev,ebound)
%STOKESPOST_Q1Q1_BC postprocesses Poisson error estimator 
%   [error_x,error_y] = stokespost_q1q1_bc(aez,fezx,fezy,elerrorx,elerrory,xy,ev,ebound);
%   input
%          aez                  elementwise Poisson problem matrices
%          fezx,fezy            elementwise rhs vectors
%          elerrorx, elerrory   elementwise error estimate (without BC imposition) 
%          xy                   vertex coordinate vector  
%          ev                   element mapping matrix
%          ebound               element edge boundary matrix 
%   output
%          error_x, error_y     component elementwise error estimate
%
%   calls function localbc_xy
%   IFISS function: DJS; 8 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      x=xy(:,1); y=xy(:,2);
      nel=length(ev(:,1));
      lev=[ev,ev(:,1)]; 
	  error_x=elerrorx;  error_y=elerrory;
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
      fex=fezx(el2e,:)'; fey=fezy(el2e,:)';
% set up local coordinates and impose interpolated error as Dirichlet bc
      xl=x(lev(el2e,:)); yl=y(lev(el2e,:)); 
	  [bae,fex,fey] = localbc_xy(ae,fex,fey,edges,xl,yl);
% solve local problems
      errx=bae\fex;  erry=bae\fey;
	  error_x(el2e,1) = errx'*ae*errx; error_y(el2e,1) = erry'*ae*erry;
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
      fex=fezx(el1e,:)'; fey=fezy(el1e,:)';
	  ae=squeeze(aez(el1e,1:5,1:5)); 
% set up local coordinates and impose interpolated error as Dirichlet bc
      xl=x(lev(el1e,:)); yl=y(lev(el1e,:));
	  [bae,fex,fey] = localbc_xy(ae,fex,fey,edges,xl,yl);
% solve local problems
      errx=bae\fex;  erry=bae\fey;
	  error_x(el1e,1) = errx'*ae*errx; error_y(el1e,1) = erry'*ae*erry;

      end
% end of element loop
%
      err_x = sqrt(sum(error_x)); error_x = sqrt(error_x);
	  err_y = sqrt(sum(error_y)); error_y = sqrt(error_y);
      fprintf('estimated velocity error (in energy):  (%10.6e,%10.6e) \n',err_x,err_y)   
 return
