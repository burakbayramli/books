function [bae,fex,fey] = localbc_xycd(viscosity,ae,fex,fey,edges,xl,yl)
%LOCALBC_XYCD imposes vector BC for Poisson error estimator 
%   [bae,fex,fey] = localbc_xycd(viscosity,ae,fex,fey,edges,xl,yl);
%   input
%          viscosity  viscosity parameter
%          ae         Poisson problem matrix
%          fex, fey   component rhs vector
%          edges      boundary edge vector 
%          xl, yl     component vertex coordinates  
%   output
%          bae        Poisson problem matrix
%          fex, fey   compoenent rhs vector
%
%   calls function: specific_flow
%   IFISS function: DJS; 11 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bae=ae; ffex=fex; ffey=fey;
nvtx = length(fex); nbd=length(edges);
zero_col=zeros(nvtx,1); zero_row=zeros(1,nvtx);
%
%% loop over boundary edges
for bd=1:nbd
    ek=edges(bd);
% compute boundary edge coordinates
    xbd(1)=xl(ek);xbd(3)=xl(ek+1);xbd(2)=0.5*(xbd(1)+xbd(3));
    ybd(1)=yl(ek);ybd(3)=yl(ek+1);ybd(2)=0.5*(ybd(1)+ybd(3));
% compute interpolated boundary error
    [bcx,bcy]=specific_flow(xbd,ybd);
    errorx = bcx(2)-0.5*(bcx(1)+bcx(3));
	errory = bcy(2)-0.5*(bcy(1)+bcy(3));
%% DJS/DK mod %% changed 5/2/02 %%back 11/2/03
    fex = fex - viscosity*errorx*bae(:,ek);
	fey = fey - viscosity*errory*bae(:,ek);
    bae(:,ek)=zero_col; bae(ek,:)=zero_row;   
    bae(ek,ek)=1;  
	fex(ek)=errorx*viscosity; fey(ek)=errory*viscosity;
end
return
