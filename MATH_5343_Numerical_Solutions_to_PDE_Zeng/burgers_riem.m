function [x, u] = burgers_riem(N,cfl,x_l,x_r,T,u_l,u_r)
  x = linspace(x_l,x_r,N+1);
  x = (x(2:end)+x(1:end-1))/2;
  h = (x_r-x_l)./N;
  flux = @(w).5*w*w;
  nflux = @(u_mnus,u_plus)burgers_nf_godunov(u_mnus,u_plus);
  vmax = @(u_mnus,u_plus)max([abs(u_mnus) abs(u_plus) eps]);  

  u = u_l*(x<0) + u_r*(x>=0);
  F = zeros(N+1,1);
  u_next = zeros(size(u));
  finish = 0;
  k = cfl * h / max(abs(u_l),abs(u_r));
  for i = 2:N
    k = min(k,cfl*h/vmax(u(i-1),u(i)));
  end

  while finish==0
    if k > T % Check if the remaining time is smaller than k
      k = T;
      finish = 1;
    end
    F(1) = flux(u_l); F(end) = flux(u_r);
    for i = 2:N
      F(i) = nflux(u(i-1),u(i));
    end
    for i = 1:N
      u_next(i) = u(i) - (k/h) * (F(i+1)-F(i));
    end
    u = u_next;
    k = cfl * h / max(abs(u_l),abs(u_r));
    for i = 2:N
      k = min(k,cfl*h/vmax(u(i-1),u(i)));
    end

    T = T - k; % Remove time step size from remaining time.
    disp(sprintf('Remaining time: %f.\n', T));
  end
end
