function [ratio,initdl] = fitint(ratio);
%FITINT computes contraction/expansion ratio
%   [ratio,initdl] = fitint(ratio);
%   input
%          ratio      initial guess  
%   output
%          ratio      converged value
%          initdl     associated first increment
%
%   called by subint
%   IFISS function: DJS; 1 April 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
      global global_N global_INTL global_LASTDL
      if(global_N==1)
      initdl=global_INTL;
      ratio=global_LASTDL/initdl;
      return
      else
      x=ratio;
% call nonlinear equation solver
%     fprintf('\n      x            f   \n')
      ratio=fzero('fint',x,optimset('Display','off'));
      initdl=global_LASTDL/(ratio^global_N);
      end
% check solution validity
      if ratio <= 1,
         fprintf('\n\n\nInfeasible ratio computed for stretch grid.\n');
         fprintf('Try changing the initial value for "ratio" in function "subint.m"\n');
         error(' '); 
      elseif ratio >= 2
         fprintf('\n\n*** warning \n computed ratio is large. \n')
      end
      return
