function [Fx, Fy] = body_force(f,feq,omega,nu_c,dh)
% omega: inverse of relaxation time.
% nu_c: counteracting viscosity.

df = f-feq;
S11_term = compute_Sij_term(df,1,1,omega);
S22_term = compute_Sij_term(df,2,2,omega);
S12_term = compute_Sij_term(df,1,2,omega);
dS11dx = spatial_difference_x(S11_term,dh);
dS22dy = spatial_difference_y(S22_term,dh);
dS12dx = spatial_difference_x(S12_term,dh);
dS12dy = spatial_difference_y(S12_term,dh);

Fx = -nu_c * ( dS11dx + dS12dy );
Fy = -nu_c * ( dS22dy + dS12dx );


