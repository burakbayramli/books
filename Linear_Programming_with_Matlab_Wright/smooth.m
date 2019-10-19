function z = smooth(z,M,q)

n = length(z);
l = zeros(n,1);
step_tol = 1e-2; sigma = 0.05;

z = max(l,z);
f = M*z+q;

for iter=1:100
  meritval = norm(z-max(z-f,l)),
  if (meritval < 1e-6) 
    iter,
    return; 
  end

  alpha = sqrt(n)/meritval;
  halpha = z-pfun(z-f,alpha);
  residual = 0.5*halpha'*halpha;
  
  PP = ones(n,1)./(ones(n,1) + exp(-alpha*(z-f)));
  B = eye(n) - diag(PP) + diag(PP)*M;

  d = -B\halpha; 

  delta = 1.0;
  while (delta > step_tol) 
    znew = z + delta*d;
    fnew = M*znew + q;
    hnew = znew - pfun(znew-fnew,alpha);
    resnew = 0.5*hnew'*hnew;
    if (resnew < (1-sigma*delta)*residual)
      break;
    end
    delta = delta * 0.5;
  end
  
  if (delta <= step_tol)
    disp('linesearch failure');
    return;
  else
    z = znew; f = fnew;
  end

end;
