function u_out = adv_cauchy_rea_fe( N, c, k, h, nslope, u_in )
  % Setup index for periodic boundary condition
  % i denotes the index for F, from 1 to N+1
  % the function returns the index for cell
  rght_c = @(i)(i<N)*(i+1)+(i==N)*1;
  left_c = @(i)(i==1)*N+(i>1)*(i-1);
  rght_f = @(i)(i<N+1)*i+(i==N+1)*1;
  left_f = @(i)(i==1)*N+(i>1)*(i-1);

  slope = zeros(1,N);
  F = zeros(1,N+1);
  u_out = zeros(1,N);

  mu = abs(c)*k/h;

  % Compute the slopes
  for i = 1:N
    slope(i) = nslope(h,u_in(left_c(i)),u_in(i),u_in(rght_c(i)));
  end
  % Compute the numerical flux
  if c > 0
    for i = 1:N+1
      F(i) = c*u_in(left_f(i))+0.5*c*h*(1-mu)*slope(left_f(i));
    end
  else
    for i = 1:N+1
      F(i) = c*u_in(rght_f(i))-0.5*c*h*(1-mu)*slope(rght_f(i));
    end
  end
  % Update the solutions
  for i = 1:N
    u_out(i) = u_in(i) - (k/h) * (F(i+1)-F(i));
  end
end
