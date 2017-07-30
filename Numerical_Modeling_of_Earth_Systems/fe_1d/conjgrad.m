function [ x counts residue ] = conjgrad(A,b,x0,tol,max_i)

imp = 2;				% 1: wikipedia 2: shewshuk
if (imp == 1)

  %
  % simplified conjugate gradient solver
  %
  r = b - A*x0;
  w = -r;
  z = A*w;
  a = (r'*w)/(w'*z);
  x = x0 + a*w;
  B = 0;
  
  counts = 1;
  while counts < max_i
    r = r - a*z;
    residue = norm(r);
    if( residue < tol )
      break;
    end
    B = (r'*z)/(w'*z);
    w = -r + B*w;
    z = A*w;
    a = (r'*w)/(w'*z);
    x = x + a*w;
    counts = counts + 1;
  end
  
else
  
  % Shewchuk implementation, almost same as above
  x = x0;
  i = 0;
  r = b - A*x;
  d = r;
  delta_new = r'*r;
  delta_0 = delta_new;
  while ((i<max_i) && (delta_new > tol*delta_0))
    q = A * d;
    alpha = delta_new / (d' * q);
    x = x + alpha * d;
    if(mod(i,50)==0)
      r = b - A*x;
    else
      r = r - alpha*q;
    end
    delta_old = delta_new;
    delta_new = r'*r;
    beta = delta_new / delta_old;
    d = r + beta * d;
    i = i+1;
  end
  
end
  

  
end
