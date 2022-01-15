function F = burgers_nf_godunov(u_mnus,u_plus)
  if ((u_plus<u_mnus)&&(u_mnus<-u_plus)) || ...
     ((u_plus>=u_mnus)&&(u_plus<0))
    F = 0.5*u_plus*u_plus;
  elseif (u_mnus>abs(u_plus)) || ((u_plus>=u_mnus)&&(u_mnus>0))
    F = 0.5*u_mnus*u_mnus;
  else
    F = 0;
end
