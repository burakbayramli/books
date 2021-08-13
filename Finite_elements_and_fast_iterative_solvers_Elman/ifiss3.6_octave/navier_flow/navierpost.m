%NAVIERPOST estimates Q2-P1 or Q2-Q1 NS error distribution 
%   IFISS scriptfile: DJS; 30 September 2013.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao
if qmethod <2,
error('This code is designed for inf-sup stable approximation!'), end  
% code for outflow boundary
if domain == 3 || domain == 10 || domain==4,  % step, channel, obstacle
      fprintf('Natural outflow on right hand vertical boundary ..\n')
      neumannb=out_Neumann_bound(mv,xy);
else, fprintf('Enclosed flow ..\n'),  neumannb=[];
end
if qmethod==3,
fprintf('FAST Q2-P1 NS a posteriori error estimation \n')
[hx,hy,eex] = eexgen(xy,xyp,mv,ee); % check element numbering
[error_x,error_y,fex,fey,ae]=navierpost_q2p1(viscosity,xy,mv,mbound,neumannb,xns,eex,hx,hy);
elseif qmethod==2,
fprintf('FAST Q2-Q1 NS a posteriori error estimation \n')
[x_q2p1,y_q2p1,xy_q2p1,xyp_q2p1,ee] = q2p1grid(x,y,xy,mv,bound);
[hx,hy,eex] = eexgen(xy_q2p1,xyp_q2p1,mv,ee); % check element numbering
[error_x,error_y,fex,fey,ae]=navierpost_q2q1(viscosity,xy,mv,mp,mbound,neumannb,xns,eex,hx,hy);
else, error('undefined mixed approximation!'), end
error_div = q2div(xy,mv,xns);
error_tot = sqrt(error_x+error_y+error_div.^2);
errorest=norm(error_tot,2);
fprintf('estimated energy error is %10.4e \n',errorest)
eplot(error_tot,mv,xy,x,y,67,'Estimated energy error'); 
