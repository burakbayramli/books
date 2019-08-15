function r = maratos(F,x,d,y,Parmeter);
% Maratos effect
g0 = feval(F,x+d,2,Parmeter); h0 = feval(F,x+d,3,Parmeter);
gradg  = feval(F,x,5,Parmeter); 
gradh  = feval(F,x,6,Parmeter);
if ~isempty(g0)
   N = gradh.'; J = find(y > 0);
   if ~isempty(J)
      N = [gradh.',gradg(J,:).']; h0 = [h0;g0(J)];
   end   
else
   N = gradh.';
end      
MM = N.'*N; CC = MM\h0; r = N*CC;
