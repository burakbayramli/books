% contour plot of q
% enter matlab and type plotq at the prompt to invoke.
%

clf
nplot = 1;
for n = 0:nplot
  n1 = 200+n;
  [t,mx,my,meqn,data] = readq2(n1);
  q = reshape(data(:,1),mx,my);  
  contour(q',30)
  axis('square')
  title(['q at time t = ', num2str(t)])
  if n < nplot
    query
    end
  end

