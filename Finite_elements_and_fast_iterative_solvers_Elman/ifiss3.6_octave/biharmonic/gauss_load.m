function ff = gauss_load(s,t,xl,yl)
%GAUSS_LOAD evaluates load function at Gauss point
%   ff = gauss_load(s,t,xl,yl);
%   input
%          s         reference element x coordinate   
%          t         reference element y coordinate
%          xl        physical element x vertex coordinates 
%          yl        physical element y vertex coordinates  
%
%   calls function: plate_rhs.m
%   IFISS function: DJS; 5 September 2018.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
      nel=length(xl(:,1));
      zero_v = zeros(nel,1); xx=zero_v; yy=xx;
      [phi_e,dphids,dphidt] = shape(s,t);
      for ivtx=1:4 
      xx = xx + phi_e(ivtx) * xl(:,ivtx);
      yy = yy + phi_e(ivtx) * yl(:,ivtx);
	  end
      ff=plate_rhs(xx,yy,nel);
      return


