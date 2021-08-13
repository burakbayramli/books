function [u,p] = defaultAxB(A,B,C,f,g,ctr)
%DEFAULTAXB standard MATLAB saddlepoint system solver      
%   [u,p] = defaultAxB(A,B,C,f,g,ctr);
%   input
%          A,B,C    saddle point system component matrices  
%          f,g      system RHS vectors
%          ctr      call counter 
%   output
%          u,p      system solution vectors
%   IFISS function: DJS;  1 May 2012  
% Copyright (c) 2011 D.J. Silvester, H.C. Elman, A. Ramage
[np,nu]=size(B); 
%%% check the sparse solver logistics
%%% if ctr==13, spparms('spumoni',2), end; %% output level (set to 0,1 or 2)
sol = [A,B';B,-C]\[f;g];
u=sol(1:nu);  p=sol(nu+1:nu+np);  spparms('spumoni',0)
return
