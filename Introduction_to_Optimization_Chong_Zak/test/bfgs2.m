k=0;
H=eye(2);
x = [0;1];
g = g_rosenb(x);
d = -H*g;
while norm(g)>1e-6
  disp(x)
  disp('-----')
  alpha=linesearch_secant2('g_rosenb',x,d);
  x = x + alpha*d;  
  g1 = g_rosenb(x);
  delx = alpha*d;
  delg = g1-g;
  H1 = H + ...
       (1+(delg'*H*delg)/(delg'*delx))*(delx*delx')/(delx'*delg) - ...
       (H*delg*delx'+(H*delg*delx')')/(delg'*delx);
  k = k+1;
  if rem(k,6)==0
      disp('eye')
      H1 = eye(2);
  end
  H = H1;
  g = g1;
  d = -H*g;
  
end

disp(x)
