%STOKESPOST estimates Stokes error distribution 
%This implements error estimation for stable Stokes Approximation
%for further details see
% "Qifeng Liao and David Silvester",
% "A simple yet effective a posteriori error estimator for classical 
%       mixed approximation of Stokes equations.",
% "Applied Numerical Mathematics, 2011",
% "http://eprints.ma.man.ac.uk/1488/"

%   IFISS scriptfile: DJS; 27 July 2015.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao

% code for outflow boundary
if domain == 3 || domain == 10 || domain==4,  % step, channel, obstacle
      fprintf('Natural outflow on right hand vertical boundary ..\n')
      neumannb=out_Neumann_bound(mv,xy);
else, fprintf('Enclosed flow ..\n'),  neumannb=[]; end
%
if qmethod==3,
fprintf('FAST Stokes Q2-P1 a posteriori error estimation \n') 
[hx,hy,eex] = eexgen(xy,xyp,mv,ee); % check element numbering
[error_x,error_y,fex,fey,ae]=stokespost_q2p1(xy,mv,mbound,neumannb,xst,eex,hx,hy);
elseif qmethod==2,
fprintf('FAST Stokes Q2-Q1 a posteriori error estimation \n')
[x_q2p1,y_q2p1,xy_q2p1,xyp_q2p1,ee] = q2p1grid(x,y,xy,mv,bound);
[hx,hy,eex] = eexgen(xy_q2p1,xyp_q2p1,mv,ee); % check element numbering
[error_x,error_y,fex,fey,ae]=stokespost_q2q1(xy,mv,mp,mbound,neumannb,xst,eex,hx,hy);
else, error('Undefined mixed approximation!'), end
%
error_div = q2div(xy,mv,xst);
error_tot = sqrt(error_x+error_y+error_div.^2);
errorest=norm(error_tot,2);
fprintf('estimated energy error is %10.4e \n',errorest)
if domain==3, eplotl2(error_tot,mv,xy,x,y,34,'Estimated energy error');
elseif domain==1, eplot2(error_tot,mv,xy,x,y,34,'Estimated energy error');  end
