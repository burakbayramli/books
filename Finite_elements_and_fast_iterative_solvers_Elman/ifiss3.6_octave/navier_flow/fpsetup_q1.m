function [a,f] = fpsetup_q1(xy,xyp,mv,mp,flowsol,viscosity,domain)
%FPSETUP_Q1 Q1 pressure convection-diffusion matrix 
%   [Ap,Fp] = fpsetup_q1(xy,xyp,mv,mp,flowsol,viscosity,domain);
%   input
%          xy         Q2 nodal coordinate vector 
%          xyp        Q1 element coordinate vector
%          mv|ev      Q2|Q1 element mapping matrix
%          mp|ev      Q1|Q1 element mapping matrix
%          flowsol    Q1-Q1|Q2-Q1 flow solution
%          viscosity  viscosity parameter
%          domain     domain index
%   output
%          Ap         Q1 scalar diffusion matrix
%          Fp         Q1 scalar convection-diffusion matrix
%
%   Rows and columns are pinned for all nodes on inflow boundaries.
%   IFISS function: DJS; 11 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nngpt=4; 
x=xy(:,1); y=xy(:,2);
xp=xyp(:,1); yp=xyp(:,2);
nvtx=length(x); nu=2*nvtx; np=length(xp);
nel=length(mv(:,1));
usol=flowsol(1:nvtx); vsol=flowsol(nvtx+1:2*nvtx); 
fprintf('setting up Q1 pressure preconditioning matrices... \n')
%
% initialise global matrices
a = sparse(np,np);
f = sparse(np,np);
%
%
% Gauss point integration rules
if (nngpt==4)        % 2x2 Gauss points
   gpt=1.0e0/sqrt(3.0e0);
   s(1) = -gpt; t(1) = -gpt; wt(1)=1;
   s(2) =  gpt; t(2) = -gpt; wt(2)=1;
   s(3) =  gpt; t(3) =  gpt; wt(3)=1; 
   s(4) = -gpt; t(4) =  gpt; wt(4)=1;
elseif (nngpt==1)   % 1x1 Gauss point
   s(1) =    0; t(1) =    0; wt(1)=4;
else
  error('Check Gauss point integration specification')
end
%
% inner loop over elements    
for ivtx = 1:4
   xl_v(:,ivtx) = x(mv(:,ivtx));
   yl_v(:,ivtx) = y(mv(:,ivtx)); 
   xsl(:,ivtx) = usol(mv(:,ivtx));
   ysl(:,ivtx) = vsol(mv(:,ivtx));
end
ae = zeros(nel,4,4); fe = zeros(nel,4,4); de = zeros(nel,4,4);
% 
% loop over Gauss points
for igpt = 1:nngpt
   sigpt=s(igpt);
   tigpt=t(igpt);
   wght=wt(igpt);
%  evaluate derivatives etc
   [jac,invjac,phi,dphidx,dphidy] = deriv(sigpt,tigpt,xl_v,yl_v);
   u_x = zeros(nel,1); u_y=zeros(nel,1);
   for k=1:4
      u_x(:) = u_x(:) + xsl(:,k) .* phi(:,k);
      u_y(:) = u_y(:) + ysl(:,k) .* phi(:,k);	 
   end
   for j = 1:4
       for i = 1:4               
          ae(:,i,j)  = ae(:,i,j)  + wght*dphidx(:,i).*dphidx(:,j).*invjac(:);
          ae(:,i,j)  = ae(:,i,j)  + wght*dphidy(:,i).*dphidy(:,j).*invjac(:);
          fe(:,i,j)  = fe(:,i,j)  + wght*u_x(:).*phi(:,i).*dphidx(:,j);
          fe(:,i,j)  = fe(:,i,j)  + wght*u_y(:).*phi(:,i).*dphidy(:,j);
          de(:,i,j) = de(:,i,j) +  wght*u_x(:).*dphidx(:,i).*u_x(:).*dphidx(:,j).*invjac(:);
          de(:,i,j) = de(:,i,j) +  wght*u_y(:).*dphidy(:,i).*u_x(:).*dphidx(:,j).*invjac(:);
          de(:,i,j) = de(:,i,j) +  wght*u_x(:).*dphidx(:,i).*u_y(:).*dphidy(:,j).*invjac(:);
          de(:,i,j) = de(:,i,j) +  wght*u_y(:).*dphidy(:,i).*u_y(:).*dphidy(:,j).*invjac(:);
       end
	end
%
% end of Gauss point loop
end  
%
%
%% computation of element Peclet number (at the centroid)         
% rectangle specific calculation here
hx=abs(xl_v(:,2)-xl_v(:,1)); hy=abs(yl_v(:,3)-yl_v(:,2));
[jac,invjac,phi,dphidx,dphidy] = deriv(0,0,xl_v,yl_v);
flowx = zeros(nel,1); flowy=zeros(nel,1);
for k=1:4
   flowx(:) = flowx(:) + xsl(:,k) .* phi(:,k);	    
   flowy(:) = flowy(:) + ysl(:,k) .* phi(:,k);	 		   
end      
flow_l2 = sqrt(flowx(:) .* flowx(:) + flowy(:) .* flowy(:));
if all(flowx==0), flow_h=hy;
elseif all(flowy==0), flow_h=hx;
else
   angle = atan(abs(flowy./flowx));
   flow_h = min([hx./cos(angle),hy./sin(angle)],[],2);
end
eph = flow_h;
epe = flow_h.*flow_l2/2; epe = epe/viscosity;
epw = flow_l2;
%
%% include streamline diffusion matrix (if necessary)      
esupg=find(epe<=1); expe=epe; expe(esupg)=0;
if any(expe), 
%  ssupg=default('include/not streamline diffusion 1/0 (default not)',0);
   ssupg=0;  %% switch off streamline diffusion
   if ssupg==1
      expe=0.5*(1-1./expe);
      epp=expe; epp(esupg)=0; epp=epp.*eph./epw;
%
% scale with the appropriate parameter
      acte = find(isfinite(expe)); nacte=length(acte);
      factor = expe(acte); flow_h=eph(acte); flow_l2=epw(acte);
	  lpe =zeros(nel,1); lpe(acte)= factor.*(flow_h./flow_l2);
      for j = 1:4       
         for i = 1:4
            de(:,i,j) = lpe(:) .* de(:,i,j);
		 end
	  end
%   
%%
%% add streamline diffusion to the convection term
      fe = fe + de;
   end
end
%
%%  element assembly into global matrix
for krow=1:4 
   nrow=mp(:,krow);	        
   for kcol=1:4
      ncol=mp(:,kcol);	  
      a = a + sparse(nrow,ncol,ae(:,krow,kcol),np,np);
      f = f + sparse(nrow,ncol,fe(:,krow,kcol),np,np);      
   end       
end
%
%% form the stabilized convection-diffusion matrix
f = viscosity*a + f;
%%
%%% fix inflow pressures for systems with inflow domain
xmin = min(xy(:,1));
if domain == 3 || domain == 10 || domain == 4,  % step, pipe, obstacle
   fprintf('fixed pressure on inflow boundary\n')
   bound=find(xyp(:,1)==xmin);
   nbd=length(bound); 
   null_col=sparse(np,nbd); null_row=sparse(nbd,np);
   dA=zeros(np,1); dA(bound)=ones(nbd,1);
   f(:,bound)=null_col;  f(bound,:)=null_row;   
   f=f+spdiags(dA,0,np,np);   
   a(:,bound)=null_col;  a(bound,:)=null_row;   
   a=a+spdiags(dA,0,np,np); 
   n_null = 0;
else
   fprintf('singular matrices are used here\n')
   %%% fix hydrostatic pressure
   %fprintf('fixed hydrostatic pressure\n')
   %null_col=sparse(np,1); null_row=sparse(1,np);
   %f(:,np)=null_col;  f(np,:)=null_row;   f(np,np)=1;  
   %a(:,np)=null_col;  a(np,:)=null_row;   a(np,np)=1;   
end
return
