function [a,b,m,r,bbx,bby,f,g] = stokes_q2q1(xy,xyp,mv,mp)
%STOKES_Q2Q1 Q2-Q1 matrix generator
%   [A,B,Q,G,Bx,By,f,g] = stokes_q2q1(xy,xyp,mv,mp);
%   input
%          xy         Q2 nodal coordinate vector 
%          xyp        Q1 nodal coordinate vector  
%          mv         Q2 element mapping matrix
%          mp         Q1 element mapping matrix
%   output
%          A          Q2 vector diffusion matrix
%          B          Q2-Q1 divergence matrix 
%          Q          Q1 mass matrix 
%          G          Q2 vector mass matrix 
%          Bx         Q2 x-derivative matrix    
%          By         Q2 y-derivative matrix    
%          f          velocity rhs vector
%          g          pressure rhs vector
%
%   Natural boundary conditions apply. Dirichlet conditions
%   must be explicitly enforced by calling function flowbc.
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nngpt=9;     
x=xy(:,1); y=xy(:,2);
xp=xyp(:,1); yp=xyp(:,2);
nvtx=length(x); nu=2*nvtx; np=length(xp); 
nel=length(mv(:,1));
lx=max(x)-min(x); ly=max(y)-min(y);
hx=max(diff(x)); hy=max(diff(y));
fprintf('setting up Q2-Q1 matrices...  ')
%
% initialise global matrices
      a = sparse(nu,nu);
      r = sparse(nu,nu);
    bbx = sparse(nvtx,nvtx);
    bby = sparse(nvtx,nvtx);
     bx = sparse(np,nvtx);
     by = sparse(np,nvtx);
	  b = sparse(np,nu);
      m = sparse(np,np);
      f = zeros(nu,1);
      g = zeros(np,1);
%
%
% Gauss point integration rules
      if (nngpt==4)        % 2x2 Gauss points
      gpt=1.0e0/sqrt(3.0e0);
      s(1) = -gpt; t(1) = -gpt; wt(1)=1;
      s(2) =  gpt; t(2) = -gpt; wt(2)=1;
      s(3) =  gpt; t(3) =  gpt; wt(3)=1; 
      s(4) = -gpt; t(4) =  gpt; wt(4)=1;
      elseif (nngpt==9)   % 3x3 Gauss points
      gpt=sqrt(0.6); 
      s(1) = -gpt; t(1) = -gpt; wt(1)=25/81;
      s(2) =  gpt; t(2) = -gpt; wt(2)=25/81;
      s(3) =  gpt; t(3) =  gpt; wt(3)=25/81; 
      s(4) = -gpt; t(4) =  gpt; wt(4)=25/81;
      s(5) =  0.0; t(5) = -gpt; wt(5)=40/81;
      s(6) =  gpt; t(6) =  0.0; wt(6)=40/81;
      s(7) =  0.0; t(7) =  gpt; wt(7)=40/81; 
      s(8) = -gpt; t(8) =  0.0; wt(8)=40/81;
      s(9) =  0.0; t(9) =  0.0; wt(9)=64/81;
      else
	  error('Check Gauss point integration specification')
      end
%
% inner loop over elements    
      for ivtx = 1:4
      xl_v(:,ivtx) = x(mv(:,ivtx));
      yl_v(:,ivtx) = y(mv(:,ivtx)); 
	  end
       ae = zeros(nel,9,9);
       re = zeros(nel,9,9);
     bbxe = zeros(nel,9,9);
     bbye = zeros(nel,9,9);
      bxe = zeros(nel,4,9);
      bye = zeros(nel,4,9);
      mpe = zeros(nel,4,4);
       ge = zeros(nel,4);
% 
% loop over Gauss points
         for igpt = 1:nngpt
         sigpt=s(igpt);
         tigpt=t(igpt);
         wght=wt(igpt);
%  evaluate derivatives etc
         [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
         [psi,dpsidx,dpsidy] = qderiv(sigpt,tigpt,xl_v,yl_v);        
            for j = 1:9
               for i = 1:9
               ae(:,i,j)  = ae(:,i,j)  + wght*dpsidx(:,i).*dpsidx(:,j).*invjac(:);
               ae(:,i,j)  = ae(:,i,j)  + wght*dpsidy(:,i).*dpsidy(:,j).*invjac(:);
               re(:,i,j)  = re(:,i,j)  + wght*psi(:,i).*psi(:,j).*jac(:);
               bbxe(:,i,j) = bbxe(:,i,j) - wght*psi(:,i) .*dpsidx(:,j);             
               bbye(:,i,j) = bbye(:,i,j) - wght*psi(:,i) .*dpsidy(:,j);   
               end
	       for i=1:4
               bxe(:,i,j) = bxe(:,i,j) - wght*phi(:,i) .* dpsidx(:,j);
               bye(:,i,j) = bye(:,i,j) - wght*phi(:,i) .* dpsidy(:,j);
               end
	    end
	    for j=1:4
	       for i=1:4
               mpe(:,i,j) = mpe(:,i,j) + wght*phi(:,i) .*phi(:,j) .*jac(:);
	       end
	    end
%
% end of Gauss point loop
         end  
%
%%  element assembly into global matrices
% component velocity matrices ...    
      for krow=1:9
	  nrow=mv(:,krow);	 
          for kcol=1:9
		  ncol=mv(:,kcol);	  
          a = a + sparse(nrow,ncol,ae(:,krow,kcol),nu,nu);
		  a = a + sparse(nrow+nvtx,ncol+nvtx,ae(:,krow,kcol),nu,nu);
          r = r + sparse(nrow,ncol,re(:,krow,kcol),nu,nu);
		  r = r + sparse(nrow+nvtx,ncol+nvtx,re(:,krow,kcol),nu,nu);
          bbx = bbx + sparse(nrow,ncol,bbxe(:,krow,kcol),nvtx,nvtx);
          bby = bby + sparse(nrow,ncol,bbye(:,krow,kcol),nvtx,nvtx);
          end
          for kcol=1:4
		  ncol=mp(:,kcol);	  
          bx = bx + sparse(ncol,nrow,bxe(:,kcol,krow),np,nvtx);
          by = by + sparse(ncol,nrow,bye(:,kcol,krow),np,nvtx);
		  end
       end
%
% vector velocity matrices ...
	   b = [bx,by];
%   
% pressure matrices ...        
	   for krow=1:4
	   nrow=mp(:,krow);	 
          for kcol=1:4
		  ncol=mp(:,kcol);	  
          m = m + sparse(nrow,ncol,mpe(:,krow,kcol),np,np);
		  end
	  end
%
fprintf('done\n')
return
