function slope = lim_mc(h,u_mnus,u_this,u_plus)
  if u_plus > u_this
    slope = max( 0.0, min([2*(u_plus-u_this) 2*(u_this-u_mnus) (u_plus-u_mnus)/2]) ) / h;
  elseif u_plus < u_this
    slope = min( 0.0, max([2*(u_plus-u_this) 2*(u_this-u_mnus) (u_plus-u_mnus)/2]) ) / h;
  else
    slope = 0.0;
  end
end
