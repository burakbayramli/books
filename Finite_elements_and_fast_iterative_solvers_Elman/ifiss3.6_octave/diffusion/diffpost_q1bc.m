function [errorsq_cbc] = diffpost_q1bc(errorsq_ele,elerr,xy,ev,ebound);
%DIFFPOST_Q1BC test code| alternative to diffpost_bc 
%   [errorsq_cbc] = diffpost_q1bc(errorsq_ele,elerr,xy,ev,ebound);
%   input
%          elerr      elementwise error estimate 
%          xy         vertex coordinate vector  
%          ev         element mapping matrix
%          ebound     element edge boundary matrix 
%   output
%          errorsq_cbc  corrected element error estimate 
%
%   IFISS function: DJS; 4 January 2011 
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao 
    x=xy(:,1); y=xy(:,2);
    nel=length(ev(:,1));
    nvtx = length(x); nbde=length(ebound(:,1));
    errorsq_cbc = errorsq_ele; %zeros(nel,1);
    elcbc = zeros(nel,5);
    elres=zeros(nel,5);
 %elerr   
 %
 tic
 % compute boundary edge midpoint values
 % bottom edge
 kk=find(ebound(:,2)==1); eedges=ebound(kk,1);
 elcbc(eedges,1)=elerr(eedges,1);
 % right edge
 kk=find(ebound(:,2)==2); eedges=ebound(kk,1);
 elcbc(eedges,2)=elerr(eedges,2);
% top edge
 kk=find(ebound(:,2)==3); eedges=ebound(kk,1);
 elcbc(eedges,3)=elerr(eedges,3);
 % right edge
 kk=find(ebound(:,2)==4); eedges=ebound(kk,1);
 elcbc(eedges,4)=elerr(eedges,4);
 %elcbc
%
% reconstruct the element matrices
% construct the integration rule
ngpt=3;
[oneg,onew] = gausspoints_oned(ngpt);
[s,t,wt] = gausspoints_twod(oneg,onew);
nngpt=ngpt^2; ng=ngpt;  
%
% inner loop over elements    
        for ivtx = 1:4
        xl_v(:,ivtx) = x(ev(:,ivtx));
        yl_v(:,ivtx) = y(ev(:,ivtx)); 
		end
        ae = zeros(nel,5,5);
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
               ae(:,i,j) = ae(:,i,j)+wght*dpsidx_v(:,i+4).*dpsidx_v(:,j+4).*invjac_v(:);
               ae(:,i,j) = ae(:,i,j)+wght*dpsidy_v(:,i+4).*dpsidy_v(:,j+4).*invjac_v(:);
               end 
            end
% end of Gauss point loop
         end
%
% matrix vector multiply (sequential code)
         for ielem = 1:nel
		    elres(ielem,:) = squeeze(ae(ielem,1:5,1:5))*(elcbc(ielem,1:5)'); 
         end

% compute the modified estimate
       for ivtx=1:5,
          errorsq_cbc(:) = errorsq_cbc(:) - 0.5*elres(:,ivtx) .* elcbc(:,ivtx);
       end
                
etime=toc;
fprintf('Boundary correction took %6.3e seconds\n',etime)  
return
