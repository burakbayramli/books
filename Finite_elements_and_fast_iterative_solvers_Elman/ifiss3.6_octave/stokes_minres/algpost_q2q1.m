function [error_x,error_y] = ...
          algpost_q2q1(q2q1sol,ae,xy,mv,mp,mbound,neumannb,eex,hx,hy)
%ALGPOST_Q2Q1 computes Q2-Q1 error estimate inside iteration 
%              process for solving the algebraic system               
%[error_x,error_y] = ...
%          algpost_q2q1(q2q1sol,ae,xy,mv,mp,mbound,neumannb,eex,hx,hy);
%   input
%        q2q1sol        solution iterate  
%             ae        LDLT factorized element matrices
%             xy        vertex coordinate vector  
%             mv        element mapping matrix
%             mp        Q1 element mapping matrix
%         mbound        element edge boundary matrix 
%       neumannb        neumann boundary edge array (set to [] if none) 
%            eex        element edge connectivity array
%          hx,hy        element mesh sizes
%   output
%        error_x, error_y     component elementwise error estimate
%
%   calls function stressjmps_q2q1,  gausspoints_oned, gausspoints_twod
%   IFISS function: DJS; 4 January 2011.
% Copyright (c) 2010 D.J. Silvester, Q. Liao 
      ngpt=3;
      x=xy(:,1); y=xy(:,2);
      mel=length(mv(:,1));
      error_x=zeros(mel,1);
      error_y=zeros(mel,1);
      nvtx=length(x);
      usol=q2q1sol(1:nvtx); 
      vsol=q2q1sol(nvtx+1:2*nvtx); 
      nump=q2q1sol(2*nvtx+1:end);
%
% construct the integration rule
[oneg,onew] = gausspoints_oned(ngpt);
[s,t,wt] = gausspoints_twod(oneg,onew);
nngpt=ngpt^2;
%
%tic
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(mv(:,ivtx));
        yl_v(:,ivtx) = y(mv(:,ivtx));
        p(:,ivtx)=nump(mp(:,ivtx));
        end
        for ivtx=1:9
           xsl_v(:,ivtx)=usol(mv(:,ivtx));
           ysl_v(:,ivtx)=vsol(mv(:,ivtx));
        end
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
        for i=1:4
           dpdx=dpdx+p(:,i).*dphidx_v(:,i).*invjac_v;
           dpdy=dpdy+p(:,i).*dphidy_v(:,i).*invjac_v;
        end
        for j = 1:12
	       fex(:,j) = fex(:,j)  +  wght* (du2dx2+du2dy2-dpdx) .* cpsi_v(:,j+4) .* jac_v(:);
           fey(:,j) = fey(:,j)  +  wght* (dv2dx2+dv2dy2-dpdy) .* cpsi_v(:,j+4) .* jac_v(:);
        end
% end of Gauss point loop
         end
%etime=toc;
%fprintf('\ninterior residual RHS assembly took %9.4e seconds',etime)

% include edge jumps 
ng=3; % 3 Gauss point rule
[oneg,onew] = gausspoints_oned(ng);
%tic
    % -------------------------------------start loop over 1D Gauss points
      for igpt=1:ng
          sigpt=oneg(igpt);
          wigpt=onew(igpt);
          [xjmp,yjmp]=stressjmps_q2q1(1,q2q1sol,eex,...
                                      xy,mv,mp,mbound,neumannb,sigpt);
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
%etime=toc;
%fprintf('\nflux jump RHS assembly took %9.4e seconds',etime)
%
%  forward-backward substitutions ...
%  tic
   xx = element_lusolve(ae,fex);
   elerrx=xx';
   yy = element_lusolve(ae,fey);
   elerry=yy';
%  etime=toc;
%  fprintf('\ntriangular solves took %9.4e seconds\n',etime)  
%
         for ivtx=1:12
             error_x(:) = error_x(:) + fex(:,ivtx) .* xx(:,ivtx);
             error_y(:) = error_y(:) + fey(:,ivtx) .* yy(:,ivtx);
         end
		 return
      
     
