function r = maratos(F,x,d,y,Parmeter);
% Maratos effect
g0 = feval(F,x+d,2,Parmeter); 
gradg  = feval(F,x,5,Parmeter); 
N = 0; MM = 0; r = 0;
J = find(y > 0);
if ~isempty(J)
   N = gradg(J,:).'; g0 = g0(J);
end   
MM = N.'*N; CC = MM\g0; r = N*CC;
