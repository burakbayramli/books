function [x, histout, complete_history]=lc_imfil
% LC_IMFIL
%
% Use imfil.m to solve the example in Chapter 3. Add the direction
% v = (-1, 1/2)^T to the usual stencil.
%
VS=[0 1; 0 -1; 1 0; -1 0; -1, .5]';
x0=[.5, .5]';
%
% Comment this line out and the iteration will completely stagnate.
%
options=imfil_optset('vstencil',VS,'target',1.d-8);
bounds=[0 0; 1 1]';
[x,histout,complete_history]=imfil(x0,@lc_obj,100,bounds,options);

function [fout,ifail,icount]=lc_obj(x)
% LC_OBJ
%
% Hardwire the linear constraint x_1 + x_2 \le 1 into
% the objective to apply the extreme barrier approach
% to constraints.
%
if x(1)+x(2) > 1
   fout=NaN;
   ifail=1;
   icount=0;
else
   fout=1-x(2);
   ifail=0;
   icount=1;
end
