function [error_x,error_y,fex,fey,ae] =... 
             stokespost_q2p1(xy,mv,mbound,neumannb,q2p1sol,eex,hx,hy)
%STOKESPOST_Q2P1 vectorized Q2-P1 local Poisson error estimator 
%                          for Q2-P1 solution with Q3 error element
%  [error_x,error_y,fex,fey,ae]=stokespost_q2p1(xy,mv,mbound,neumannb,xst,eex,hx,hy);
%   input
%          xy           vertex coordinate vector  
%          mv           element mapping matrix
%          mbound       element edge boundary matrix 
%          neumannb     neumann boundary edge array (set to [] if none) 
%          q2p1sol      vertex solution vector
%          eex          element edge connectivity array
%          hx,hy        element mesh sizes
%   output
%    error_x, error_y   component of velocity elementwise error estimate
%    fex, fey           component elementwise rhs vectors
%    ae                 LDLT factorized element matrices
%
%   calls functions stressjmps_q2p1, gausspoints_oned, gausspoints_twod
%   IFISS function: DJS; 2 January 2011 
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao 
      fprintf('Q2-P1 local error estimator ...  ')
      x=xy(:,1); y=xy(:,2);
      mel=length(mv(:,1));
      error_x=zeros(mel,1);
      error_y=zeros(mel,1);
      nvtx=length(x);
      usol=q2p1sol(1:nvtx); 
      vsol=q2p1sol(nvtx+1:2*nvtx); 
      nump=q2p1sol(2*nvtx+1:end);
      p(:,1)=nump(1:3:end);
      p(:,2)=nump(2:3:end);
      p(:,3)=nump(3:3:end);
%
% construct the integration rule
ngpt=7;
[oneg,onew] = gausspoints_oned(ngpt);
[s,t,wt] = gausspoints_twod(oneg,onew);
nngpt=ngpt^2;
%
tic
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(mv(:,ivtx));
        yl_v(:,ivtx) = y(mv(:,ivtx)); 
        end
        for ivtx=1:9
           xsl_v(:,ivtx)=usol(mv(:,ivtx));
           ysl_v(:,ivtx)=vsol(mv(:,ivtx));
        end
        ae = zeros(mel,12,12); 
        elerrx=zeros(12,mel);
        elerry=zeros(12,mel);
        fex = zeros(mel,12);
        fey = zeros(mel,12);
% loop over Gauss points for interior residual part
         for igpt = 1:nngpt
         sigpt=s(igpt);
         tigpt=t(igpt);
         wght=wt(igpt);
% evaluate derivatives etc
        [jac_v,invjac_v,phi_v,dphidx_v,dphidy_v] = deriv(sigpt,tigpt,xl_v,yl_v);
        [cpsi_v,dcpsidx_v,dcpsidy_v] = cderiv(sigpt,tigpt,xl_v,yl_v);
        [dpsi2dx2,dpsi2dy2] = qderiv_2(sigpt,tigpt,xl_v,yl_v);
        [chi,dchidx,dchidy] = lderiv(sigpt,tigpt,xl_v,yl_v);
        du2dx2=zeros(mel,1);
        du2dy2=zeros(mel,1);
        dv2dx2=zeros(mel,1);
        dv2dy2=zeros(mel,1);
        %get the second derivatives of the Q2 velocity 
        for i=1:9
            du2dx2=du2dx2+xsl_v(:,i).*dpsi2dx2(:,i).*invjac_v.^2;
            du2dy2=du2dy2+xsl_v(:,i).*dpsi2dy2(:,i).*invjac_v.^2;
            dv2dx2=dv2dx2+ysl_v(:,i).*dpsi2dx2(:,i).*invjac_v.^2;
            dv2dy2=dv2dy2+ysl_v(:,i).*dpsi2dy2(:,i).*invjac_v.^2;
        end
        dpdx=zeros(mel,1);
        dpdy=zeros(mel,1);
        %get the first derivatives of the P1 velocity
        for i=1:3
           dpdx=dpdx+p(:,i).*dchidx(:,i).*invjac_v;
           dpdy=dpdy+p(:,i).*dchidy(:,i).*invjac_v;
        end
        for j = 1:12
             for i = 1:12
                 ae(:,i,j) = ae(:,i,j)+wght*dcpsidx_v(:,i+4).*dcpsidx_v(:,j+4).*invjac_v(:);
                 ae(:,i,j) = ae(:,i,j)+wght*dcpsidy_v(:,i+4).*dcpsidy_v(:,j+4).*invjac_v(:);
             end
	         fex(:,j) = fex(:,j)  +  wght* (du2dx2+du2dy2-dpdx) .* cpsi_v(:,j+4) .* jac_v(:);
             fey(:,j) = fey(:,j)  +  wght* (dv2dx2+dv2dy2-dpdy) .* cpsi_v(:,j+4) .* jac_v(:);
        end
% end of Gauss point loop
         end
etime=toc;
fprintf('\ninterior residual RHS assembly took %9.4e seconds',etime)
%
% include edge jumps 
ng=3; % 3 Gauss point rule
[oneg,onew] = gausspoints_oned(ng); 
tic
   % ------------------------------------- start loop over 1D Gauss points
          for igpt=1:ng
              sigpt=oneg(igpt);
              wigpt=onew(igpt);         
              [xjmp,yjmp]=stressjmps_q2p1(1,q2p1sol,eex,...
                                                xy,mv,mbound,neumannb,sigpt);
              [cpsi_v,dcpsidx_v,dcpsidy_v] = cderiv(sigpt,-1,xl_v,yl_v);                  
              fex(:,1) = fex(:,1) - wigpt*1/2.*xjmp(:,1).*cpsi_v(:,5) .* hx(:)./2;
              fey(:,1) = fey(:,1) - wigpt*1/2.*yjmp(:,1).*cpsi_v(:,5) .* hx(:)./2;
              fex(:,2) = fex(:,2) - wigpt*1/2.*xjmp(:,1).*cpsi_v(:,6) .* hx(:)./2;
              fey(:,2) = fey(:,2) - wigpt*1/2.*yjmp(:,1).*cpsi_v(:,6) .* hx(:)./2;
              [cpsi_v,dcpsidx_v,dcpsidy_v] = cderiv(1,sigpt,xl_v,yl_v);
              fex(:,3) = fex(:,3) - wigpt*1/2.*xjmp(:,2).*cpsi_v(:,7) .* hy(:)./2;
              fey(:,3) = fey(:,3) - wigpt*1/2.*yjmp(:,2).*cpsi_v(:,7) .* hy(:)./2; 
              fex(:,4) = fex(:,4) - wigpt*1/2.*xjmp(:,2).*cpsi_v(:,8) .* hy(:)./2;
              fey(:,4) = fey(:,4) - wigpt*1/2.*yjmp(:,2).*cpsi_v(:,8) .* hy(:)./2; 
              [cpsi_v,dcpsidx_v,dcpsidy_v] = cderiv(sigpt,1,xl_v,yl_v);
              fex(:,5) = fex(:,5) - wigpt*1/2.*xjmp(:,3).*cpsi_v(:,9) .* hx(:)./2;
              fey(:,5) = fey(:,5) - wigpt*1/2.*yjmp(:,3).*cpsi_v(:,9) .* hx(:)./2;
              fex(:,6) = fex(:,6) - wigpt*1/2.*xjmp(:,3).*cpsi_v(:,10) .* hx(:)./2;
              fey(:,6) = fey(:,6) - wigpt*1/2.*yjmp(:,3).*cpsi_v(:,10) .* hx(:)./2;
              [cpsi_v,dcpsidx_v,dcpsidy_v] = cderiv(-1,sigpt,xl_v,yl_v);
              fex(:,7) = fex(:,7) - wigpt*1/2.*xjmp(:,4).*cpsi_v(:,11) .* hy(:)./2;
              fey(:,7) = fey(:,7) - wigpt*1/2.*yjmp(:,4).*cpsi_v(:,11) .* hy(:)./2;
              fex(:,8) = fex(:,8) - wigpt*1/2.*xjmp(:,4).*cpsi_v(:,12) .* hy(:)./2;
              fey(:,8) = fey(:,8) - wigpt*1/2.*yjmp(:,4).*cpsi_v(:,12) .* hy(:)./2;
          end
    % -----------------------------------end loop over 1D Gauss points
        
etime=toc;
fprintf('\nflux jump RHS assembly took %9.4e seconds',etime)
%
%solve for local estimate (sequential code)
%        for ielem = 1:mel
%		     elerrx(:,ielem) = squeeze(ae(ielem,1:12,1:12))\(fex(ielem,1:12)'); 
%            elerry(:,ielem) = squeeze(ae(ielem,1:12,1:12))\(fey(ielem,1:12)'); 
%         end


% vectorized code
% LDLT factorization 
tic
nn=12; nel=mel;
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
etime=toc;
fprintf('\nLDLT factorization took %9.4e seconds',etime)  
%
tic
%  forward-backward substitutions ...
   xx = element_lusolve(ae,fex);
   elerrx=xx';
   yy = element_lusolve(ae,fey);
   elerry=yy';
etime=toc;
fprintf('\ntriangular solves took %9.4e seconds\n',etime)  
%
         for ivtx=1:12
             error_x(:) = error_x(:) + fex(:,ivtx) .* elerrx(ivtx,:)';
             error_y(:) = error_y(:) + fey(:,ivtx) .* elerry(ivtx,:)';
         end	   
		 return
