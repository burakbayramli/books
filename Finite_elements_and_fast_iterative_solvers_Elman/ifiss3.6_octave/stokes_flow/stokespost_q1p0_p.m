function [error_x, error_y, fex, fey, ae] = stokespost_q1p0_p(jmpx,jmpy,els,xy,ev)
%STOKESPOST_Q1P0_P computes Poisson error estimator for Q1-P0 
%   [err_x, err_y, fex, fey, ae] = stokespost_q1p0_p(jmpx,jmpy,els,xy,ev);
%   input
%          jmpx, jmpy     component elementwise edge stress jumps
%          els            elementwise edge lengths
%          xy             vertex coordinate vector  
%          ev             element mapping matrix
%   output
%          err_x, err_y   component of velocity elementwise error estimate
%          fex, fey       component elementwise rhs vectors
%          ae             elementwise Poisson problem matrices
%
%   IFISS function: DJS; 8 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      fprintf('computing local error estimator... ')
      x=xy(:,1); y=xy(:,2);
      nel=length(ev(:,1));
      error_x=zeros(nel,1);  error_y=zeros(nel,1);
%
% set up 3x3 Gauss points
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
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(ev(:,ivtx));
        yl_v(:,ivtx) = y(ev(:,ivtx)); 
		end
        ae = zeros(nel,5,5); elerrx=zeros(5,nel);  elerry=zeros(5,nel);
        fex = zeros(nel,5); fey = zeros(nel,5);
% loop over Gauss points
         for igpt = 1:9
         sigpt=s(igpt);
         tigpt=t(igpt);
         wght=wt(igpt);
% evaluate derivatives etc
         [jac_v,invjac_v,phi_v,dphidx_v,dphidy_v] = deriv(sigpt,tigpt,xl_v,yl_v);
         [psi_v,dpsidx_v,dpsidy_v] = qderiv(sigpt,tigpt,xl_v,yl_v);        
            for j = 1:5
               for i = 1:5
               ae(:,i,j) = ae(:,i,j)+wght*dpsidx_v(:,i+4).*dpsidx_v(:,j+4).*invjac_v(:);
               ae(:,i,j) = ae(:,i,j)+wght*dpsidy_v(:,i+4).*dpsidy_v(:,j+4).*invjac_v(:);
               end
            end
% end of Gauss point loop
         end
%
% include edge jumps (evaluated at the midpoint)
         for ee = 1:4
         fex(:,ee) = fex(:,ee) - jmpx(:,ee) .* els(:,ee)*(1/3);
         fey(:,ee) = fey(:,ee) - jmpy(:,ee) .* els(:,ee)*(1/3);
          end
%
% solve for local estimate 
%         err=ae\fe;
%         elerr_p(ielem,1) = err'*fe;
%% sequential code
         for ielem = 1:nel
		 elerrx(:,ielem) = squeeze(ae(ielem,1:5,1:5))\(fex(ielem,1:5)'); 
		 elerry(:,ielem) = squeeze(ae(ielem,1:5,1:5))\(fey(ielem,1:5)'); 
	     end
%%
         for ivtx=1:5, 
	     error_x(:) = error_x(:) + fex(:,ivtx) .* elerrx(ivtx,:)';
		 error_y(:) = error_y(:) + fey(:,ivtx) .* elerry(ivtx,:)';
		 end
%%	 
         fprintf('done.\n')	   
		 return
