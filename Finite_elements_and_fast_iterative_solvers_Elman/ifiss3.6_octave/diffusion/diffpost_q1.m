function [errorsq_ele,xx,fe,ae] = diffpost_q1(xy,ev,ebound,q1sol,eex,hx,hy)
%DIFFPOST_Q1 local Poisson error estimator for Q1 solution 
%   [errorsq_ele,elerr,fe,ae] = diffpost_q1(xy,ev,ebound,q1sol,eex,hx,hy);
%   input
%          xy           vertex coordinate vector  
%          ev           element mapping matrix
%          ebound       element edge boundary matrix 
%          q1sol        Q1 solution vector
%          eex          element edge connectivity array
%          hx,hy        element mesh sizes      
%   output
%          errorsq_ele  element error estimate
%          elerr        elementwise error estimate
%          fe           elementwise rhs vectors
%          ae           LDLT factorized element matrices
%
%   calls functions q1fluxjmps, gausspoints_oned, gausspoints_twod
%   IFISS function: DJS; 28 September 2013
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao 
      fprintf('computing Q1 error estimator...  \n')
      x=xy(:,1); y=xy(:,2);
      nel=length(ev(:,1));
      errorsq_ele = zeros(nel,1);
%
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
        ae = zeros(nel,5,5); elerr=zeros(5,nel);
        fe = zeros(nel,5);
% loop over Gauss points
         for igpt = 1:nngpt
         sigpt=s(igpt);
         tigpt=t(igpt);
         wght=wt(igpt);
% evaluate derivatives etc
         [jac_v,invjac_v,phi_v,dphidx_v,dphidy_v] = deriv(sigpt,tigpt,xl_v,yl_v);
         rhs_v = gauss_source(sigpt,tigpt,xl_v,yl_v);
         [psi_v,dpsidx_v,dpsidy_v] = qderiv(sigpt,tigpt,xl_v,yl_v);        
            for j = 1:5
               for i = 1:5
               ae(:,i,j) = ae(:,i,j)+wght*dpsidx_v(:,i+4).*dpsidx_v(:,j+4).*invjac_v(:);
               ae(:,i,j) = ae(:,i,j)+wght*dpsidy_v(:,i+4).*dpsidy_v(:,j+4).*invjac_v(:);
               end
	        fe(:,j) = fe(:,j)  +  wght* rhs_v(:) .* psi_v(:,j+4) .* jac_v(:); 
            end
% end of Gauss point loop
         end
%
% include edge jumps (evaluated at the midpoint)
%        fe(:,ee) = fe(:,ee) - jmp(:,ee) .* els(:,ee)*(1/3);
         njmp = q1fluxjmps(q1sol,eex,xy,ev,ebound,0);    
         fe(:,1) = fe(:,1) - njmp(:,1) .* hx(:) .*(1/3);
         fe(:,2) = fe(:,2) - njmp(:,2) .* hy(:) .*(1/3);
         fe(:,3) = fe(:,3) - njmp(:,3) .* hx(:) .*(1/3);
         fe(:,4) = fe(:,4) - njmp(:,4) .* hy(:) .*(1/3);
%
% solve for local estimate (sequential code)
%         for ielem = 1:nel
%		    elerr(:,ielem) = squeeze(ae(ielem,1:5,1:5))\(fe(ielem,1:5)'); 
%         end
% vectorized code
% LDLT factorization 
nn=5; 
dd=zeros(nel,nn); rr=zeros(nel,nn);
for kk=1:nn-1
    for pp=1:kk-1;
    rr(1:nel,pp)=dd(1:nel,pp).*ae(1:nel,kk,pp);
    end
    dd(1:nel,kk)=ae(1:nel,kk,kk);
    for pp=1:kk-1;
    dd(1:nel,kk)= dd(1:nel,kk)- ae(1:nel,kk,pp).*rr(1:nel,pp);
    end
    for ii=kk+1:nn
        for pp=1:kk-1;
        ae(1:nel,ii,kk)=ae(1:nel,ii,kk)-ae(1:nel,ii,pp).*rr(1:nel,pp);
        end
        ae(1:nel,ii,kk)=ae(1:nel,ii,kk)./dd(1:nel,kk);
    end
end
    for pp=1:nn-1;
    rr(1:nel,pp)=dd(1:nel,pp).*ae(1:nel,nn,pp);
    end
    dd(1:nel,nn)=ae(1:nel,nn,nn);
    for pp=1:nn-1;
    dd(1:nel,nn)= dd(1:nel,nn)- ae(1:nel,nn,pp).*rr(1:nel,pp);
    end
% overwrite diagonal entries
    for kk=1:nn
    ae(1:nel,kk,kk)= dd(1:nel,kk);
    end 
%  forward-backward substitutions ...
   xx = element_lusolve(ae,fe);
   elerr=xx';
 
%
   for ivtx=1:5,
	   errorsq_ele(:) = errorsq_ele(:) + fe(:,ivtx) .* elerr(ivtx,:)';
   end        
   etime=toc;
   fprintf('error estimation took %6.3e seconds\n',etime)  
return
