function F = burgers_nf_roe(u_mnus,u_plus)
  % Roe average
  A = 0.5*(u_mnus+u_plus);
  % Numerical flux
  F = 0.25*(u_mnus*u_mnus+u_plus*u_plus)-0.5*abs(A)*(u_plus-u_mnus);
end
