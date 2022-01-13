function slope = lim_vanleer(h,u_mnus,u_this,u_plus)
  if (u_plus-u_this)*(u_this-u_mnus) < 0.0
    slope = 0.0;
  elseif abs(u_this-u_mnus) < abs(u_plus-u_this)
    theta = (u_this-u_mnus)/(u_plus-u_this);
    if u_plus > u_this
      slope = ((u_this-u_mnus)+abs(u_this-u_mnus))/(1+abs(theta))/h;
    elseif u_plus < u_this
      slope = ((u_this-u_mnus)-abs(u_this-u_mnus))/(1+abs(theta))/h;
    else
      slope = 0.0;
    end
  elseif abs(u_this-u_mnus) > abs(u_plus-u_this)
    theta = (u_plus-u_this)/(u_this-u_mnus);
    if u_this > u_mnus
      slope = ((u_plus-u_this)+abs(u_plus-u_this))/(1+abs(theta))/h;
    elseif u_this < u_mnus
      slope = ((u_plus-u_this)-abs(u_plus-u_this))/(1+abs(theta))/h;
    else
      slope = 0.0;
    end
  else
    if u_plus == u_mnus
      slope = 0.0;
    else
      slope = (u_plus-u_this)/h;
    end
  end
end
