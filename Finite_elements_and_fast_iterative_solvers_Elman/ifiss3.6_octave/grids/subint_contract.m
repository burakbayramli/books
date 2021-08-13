function x=subint_contract(xm,x3,x4,nb,nc)
%SUBINT_CONTRACT geometrically stretched grid contraction
%   x=subint_contract(x1,x2,x3,x4,na,nb,nc);
%   input
%             x1      left coordinate
%             x2      limit of uniform expansion section
%             x3      limit of equal subinterval section
%             x4      right coordinate and limit of contraction section
%             na      number of uniformly expanded subintervals
%             nb      number of intermediate uniform subintervals
%             nc      number of contracting subintervals
%    output
%              x      vector of coordinates
%
%   calls function fitint
%   sets up global variables global_N, global_INTL, global_LASTDL 
%   IFISS function: DJS; 30 December 2009.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      global global_N global_INTL global_LASTDL 
      intlc=xm-x3;
      intlb=x3-x4;
      npts=nb+nc+1;
      bdl=intlb/nb;
      x=zeros(npts,1);
      x(npts)=xm;
      x(1) = x4;

      dl=bdl;
      for node=2:(nb+1)
         x(node)=x(node-1) + dl;
      end
%
% uniform contraction section
      if nc > 0
%% fixed value here
%        ratio = default('approximate contraction ratio (default 1.2)',
         ratio = 1.2;
         global_N=nc; 
         global_INTL=intlc;
         global_LASTDL=bdl;
         [ratio,initdl]=fitint(ratio);
        fprintf('computed contraction ratio is %10.5g \n',ratio)
         dl=global_LASTDL/ratio;
         for node=nb+2:npts
            x(node)=x(node-1)+dl;
            dl=dl/ratio;
         end
         if (abs(x(npts)-xm) > 1e-6)
            fprintf('\n warning ...\n calculated coordinate is %10.5g',x(npts))
            fprintf('              \n      input coordinate is %10.5g',xm)
         end
      end
      return
