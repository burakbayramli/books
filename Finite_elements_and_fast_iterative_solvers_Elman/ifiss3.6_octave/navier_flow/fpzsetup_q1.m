function [a,f] = fpzsetup_q1(xy,xyp,mv,mp,flowsol,viscosity,domain,map)
%FPZSETUP_Q1 modified Q1 PCD matrix 
%   [Ap,Fp] = fpzsetup_q1(xy,xyp,mv,mp,flowsol,viscosity,domain,map);
%   input
%          xy         Q2 nodal coordinate vector 
%          xyp        Q1 element coordinate vector
%          mv|ev      Q2|Q1 element mapping matrix
%          mp|ev      Q1|Q1 element mapping matrix
%          flowsol    Q1-Q1|Q2-Q1 flow solution
%          viscosity  viscosity parameter
%          domain     domain index
%          map        Q2 -> Q1 vertex mapping vector
%   output
%          Ap         Q1 scalar diffusion matrix
%          Fp         Q1 scalar convection-diffusion matrix
%
%   IFISS function: DJS; 22 October 2009; modified HCE, 25 January 2013.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
nngpt=4; 
x=xy(:,1); y=xy(:,2);
xp=xyp(:,1); %yp=xyp(:,2);
nvtx=length(x); np=length(xp);  %nu=2*nvtx; 
nel=length(mv(:,1));
usol=flowsol(1:nvtx); vsol=flowsol(nvtx+1:2*nvtx); 
fprintf('setting up modified Q1 pressure preconditioning matrices... \n')
%
% initialise global matrices
a = sparse(np,np);
f = sparse(np,np);
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
% computation of element Peclet number (at the centroid)         
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
% include streamline diffusion matrix (if necessary)      
esupg=find(epe<=1); expe=epe; expe(esupg)=0;
if any(expe), 
%  ssupg=default('include/not streamline diffusion 1/0 (default not)',0);
   ssupg=0;  %% switch off streamline diffusion
   if ssupg==1,
      expe=0.5*(1-1./expe);
% scale with the appropriate parameter
      acte = find(isfinite(expe)); %nacte=length(acte);
      factor = expe(acte); flow_h=eph(acte); flow_l2=epw(acte);
	  lpe =zeros(nel,1); lpe(acte)= factor.*(flow_h./flow_l2);
      for j = 1:4       
         for i = 1:4
            de(:,i,j) = lpe(:) .* de(:,i,j);
         end
      end
%  add streamline diffusion to the convection term
      fe = fe + de;
   end
end
%
%
% element assembly into global matrix
for krow=1:4 
   nrow=mp(:,krow);	        
   for kcol=1:4
      ncol=mp(:,kcol);	  
      a = a + sparse(nrow,ncol,ae(:,krow,kcol),np,np);
      f = f + sparse(nrow,ncol,fe(:,krow,kcol),np,np);      
   end       
end
%
% form the stabilized convection-diffusion matrix
f = viscosity*a + f;
%
%
% Robin b.c. for Fp at inflow boundary
xmin = min(xy(:,1));
if domain == 3 || domain == 10 || domain == 4,  % step, channel, obstacle
   fprintf('Robin pressure on inflow boundary\n')
% find pressure indices
   boundp=find(xyp(:,1)==xmin);
% sort into ascending order (vertical boundary)
   [dummy,kk]=sort(xyp(boundp,2));
% compute mesh lengths
   hh=diff(dummy);
% loop over edges adding line integral contributions
   for edge=1:length(kk)-1;
      indp1=boundp(kk(edge));
      indp2=boundp(kk(edge+1));
% get normal velocity at edge vertices
      ux1=usol(map(indp1)); ux2=usol(map(indp2)); 
      uxx=0.5*(ux1+ux2); % average inflow velocity
      hedge=hh(edge);
      aver=1; % averaging switch 
      if aver==1,
%     fprintf('averaged inflow boundary velocity\n')
         f(indp1,indp1)=f(indp1,indp1)+uxx*hedge/3;
         f(indp1,indp2)=f(indp1,indp2)+uxx*hedge/6;
         f(indp2,indp1)=f(indp2,indp1)+uxx*hedge/6;
         f(indp2,indp2)=f(indp2,indp2)+uxx*hedge/3;
      else
%     fprintf('exact inflow boundary velocity\n')
         f(indp1,indp1)=f(indp1,indp1)+(uxx+ux1)*hedge/6;
         f(indp1,indp2)=f(indp1,indp2)+uxx*hedge/6;
         f(indp2,indp1)=f(indp2,indp1)+uxx*hedge/6;
         f(indp2,indp2)=f(indp2,indp2)+(uxx+ux2)*hedge/6;
      end
   end
%
% Dirichlet conditions for Ap and Fp at outflow boundary: mimic finite differences
   xmax = max(xy(:,1));
   bound_out_p = find(xyp(:,1)==xmax);
   ab = sparse(np,np);
   fb = sparse(np,np);
   for krowcol=1:4,
      nrowcol = mp(:,krowcol);
      ab = ab + sparse(nrowcol,nrowcol,ae(:,krowcol,krowcol),np,np);
      fb = fb + sparse(nrowcol,nrowcol,fe(:,krowcol,krowcol),np,np);
   end
   fb = viscosity*ab + fb;
% Augment diagonal with values from "ghost elements" outside outflow boundary 
   a(bound_out_p,bound_out_p) = a(bound_out_p,bound_out_p) + ...
                                    ab(bound_out_p,bound_out_p);
   f(bound_out_p,bound_out_p) = f(bound_out_p,bound_out_p) + ...
                                   fb(bound_out_p,bound_out_p);
%{
for i=1:length(bound_out_p), ...
      index = bound_out_p(i);
      a(index,index) = max(diag(a));
   end
%}
%{
% Other choices considered:
% Pin single row of Ap, in the middle of the outflow boundary
   pin_index = bound_out_p(ceil(length(bound_out_p)/2));
   a(:,pin_index)=0;  a(pin_index,:)=0;  a(pin_index,pin_index)=1;
% Pin all rows of Ap, set diag at outflow = 1 or max(diag)
   for i=1:length(bound_out_p), ...
      index = bound_out_p(i);
     %a(:,index)=0; a(index,:)=0; a(index,index)=1;
      a(:,index)=0; a(index,:)=0; a(index,index) = max(diag(a));
   end
%}
%
else
   fprintf('singular matrices are used here\n')
end
return