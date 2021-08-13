function x=subint_expand(xm,x1,x2,na,nb)
%SUBINT_EXPAND geometrically stretched grid expansion 
%   x=subint_expand(x1,x2,x3,x4,na,nb,nc);
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
      intla=x1-x2;
      intlb=xm-x1;
      npts=nb+na+1;
      bdl=intlb/nb;
      x=zeros(npts,1);
      x(1)=x2;
      x(npts) = xm;
%
% uniform expansion section
      if na > 0
%% fixed value here
%        ratio = default('approximate expansion ratio (default 1.2)',1.2);       
         ratio = 1.2;
         global_N=na; 
         global_INTL=intla;
         global_LASTDL=bdl;
         [ratio,initdl]=fitint(ratio);
         fprintf('computed expand ratio is %10.5g \n',ratio)
         dl=initdl;
         for node=2:na+1
            x(node)=x(node-1)+dl;
            dl=dl*ratio;
         end
         if (abs(x(na+1)-x1) > 1e-6)
            fprintf('\n warning ...\n calculated coordinate is %10.5g',x(na+1))
            fprintf('              \n      input coordinate is %10.5g',x1)
            x(na+1)=x2;
         end
      end
%
% uniform subinterval section
      dl=bdl;
      for node=na+2:na+nb+1
         x(node)=x(node-1) + dl;
      end
      return
