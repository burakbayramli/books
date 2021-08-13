function ff = gauss_source(s,t,xl,yl)
%GAUSS_SOURCE evaluates source term at Gauss point 
%   ff = gauss_source(s,t,xl,yl);
%   input
%          s         reference element x coordinate   
%          t         reference element y coordinate
%          xl        physical element x vertex coordinates 
%          yl        physical element y vertex coordinates  
%
%   calls function: specific_rhs
%   IFISS function: DJS; 4 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      nel=length(xl(:,1));
      zero_v = zeros(nel,1); xx=zero_v; yy=xx;
      [phi_e,dphids,dphidt] = shape(s,t);
      for ivtx=1:4 
      xx = xx + phi_e(ivtx) * xl(:,ivtx);
      yy = yy + phi_e(ivtx) * yl(:,ivtx);
	  end
      ff=specific_rhs(xx,yy,nel);
      return


