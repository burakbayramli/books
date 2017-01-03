function [xo,fo]=opt_Nelder(f,x0,TolX,TolFun,MaxIter)
N=length(x0);
if N==1 %for 1-dimensional case
  [xo,fo]=opt_quad(f,x0,TolX,TolFun); return
end
S= eye(N);
for i=1:N %repeat the procedure for each subplane
   i1=i+1; if i1>N, i1=1; end
   abc=[x0; x0+S(i,:); x0+S(i1,:)]; %each directional subplane
   fabc=[feval(f,abc(1,:)); feval(f,abc(2,:)); feval(f,abc(3,:))];
   [x0,fo]=Nelder0(f,abc,fabc,TolX,TolFun,MaxIter);
   if N<3, break; end %No repetition needed for a 2-dimensional case
end
xo=x0;
