%DIFFPOST driver for a posteriori error postprocessing
% This implements the Q2 error approximation scheme of Qifeng Liao
%   IFISS scriptfile: DJS; 28 September 2013.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao

neumannb=[]; % default is a Dirichelt problem
if pde>2,
% flow problem code for natural boundary
   if domain == 3 || domain == 10 || domain==4,  % step, channel, obstacle
      fprintf('Neumann condition on right hand vertical boundary ..\n')
      neumannb=out_Neumann_bound(ev,xy);
   else,  fprintf('Enclosed flow ..\n'), end
end
%
if qmethod ==1,
fprintf('FAST a posteriori error estimation for Q1 element \n') 
[hx,hy,eex] = edgegen(xy,ev);
[errorsq_ele,elerr,fe,ae] = diffpost_q1(xy,ev,ebound,x_gal,eex,hx,hy);
error_tot = sqrt(errorsq_ele); errorest=norm(error_tot,2);
fprintf('simplistic estimate energy error is %10.4e \n',errorest)
%   if domain ==2, eplotl(error_tot,ev,xy,x,y,12);
%   else, eplot(error_tot,ev,xy,x,y,12); end
%
% include the boundary correction
[errorsq_cbc] = diffpost_bc(errorsq_ele,fe,xy,ev,ebound);
error_tot = sqrt(errorsq_cbc); errorest=norm(error_tot,2);
fprintf('corrected estimate energy error is %10.4e \n',errorest)
%   if domain ==2, eplotl(error_tot,ev,xy,x,y,13);
%   else, eplot(error_tot,ev,xy,x,y,13); end
%
elseif qmethod ==2,
fprintf('FAST a posteriori error estimation for Q2 element \n')  
[x_q2p1,y_q2p1,xy_q2p1,xyp_q2p1,eezz] = q2p1grid(x,y,xy,mv,bound);
[hx,hy,eex] = eexgen(xy_q2p1,xyp_q2p1,mv,eezz); % check element numbering
[error_ele,fe,ae] = diffpost_q2_with_q4(xy,mv,mbound,x_gal,eex,hx,hy,neumannb,3);
error_tot = sqrt(error_ele); errorest=norm(error_tot,2);
fprintf('estimated energy error is %10.4e \n',errorest)
end
