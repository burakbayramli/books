function slope = lim_minmod(h,u_mnus,u_this,u_plus)
  if u_plus > u_this
    slope = max( 0.0, min(u_plus-u_this,u_this-u_mnus) ) / h;
  elseif u_plus < u_this
    slope = min( 0.0, max(u_plus-u_this,u_this-u_mnus) ) / h;
  else
    slope = 0.0;
  end
end
