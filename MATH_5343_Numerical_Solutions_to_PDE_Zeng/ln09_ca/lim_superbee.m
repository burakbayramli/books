function slope = lim_superbee(h,u_mnus,u_this,u_plus)
  if u_plus > u_this
    slope = max( 0.0, max( min(2*(u_this-u_mnus),u_plus-u_this), min(u_this-u_mnus,2*(u_plus-u_this)) ) )/h;
  elseif u_plus < u_this
    slope = min( 0.0, min( max(2*(u_this-u_mnus),u_plus-u_this), max(u_this-u_mnus,2*(u_plus-u_this)) ) )/h;
  else
    slope = 0.0;
  end
end
