function alpha=linesearch_secant(f,d,x)

epsilon=10^(-5); %line search tolerance
max = 500; %maximum number of iterations
alpha_curr=0;
alpha=10^(-5);
[y,grad]=feval(f,x);
dphi_zero=grad'*d;
dphi_curr=dphi_zero;
 
i=0;
while abs(dphi_curr)>epsilon*abs(dphi_zero),
  alpha_old=alpha_curr;
  alpha_curr=alpha;
  dphi_old=dphi_curr;
  [y,grad]=feval(f,x+alpha_curr*d);
  dphi_curr=grad'*d;
  alpha=(dphi_curr*alpha_old-dphi_old*alpha_curr)/(dphi_curr-dphi_old);
  i=i+1;
  if (i >= max) & (abs(dphi_curr)>epsilon*abs(dphi_zero)),
    disp('Line search terminating with number of iterations:');
    disp(i);
    disp(alpha);
    break;
  end  
end 
