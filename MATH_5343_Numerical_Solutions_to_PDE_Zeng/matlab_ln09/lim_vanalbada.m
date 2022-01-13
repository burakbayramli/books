function slope = lim_vanalbada(h,u_mnus,u_this,u_plus)
  if (u_plus-u_this)*(u_this-u_mnus) < 0.0
    slope = 0.0;
  elseif abs(u_this-u_mnus) < abs(u_plus-u_this)
    theta = (u_this-u_mnus)/(u_plus-u_this);
    slope = ((1+theta)/(1+theta*theta))*((u_this-u_mnus)/h);
  elseif abs(u_this-u_mnus) > abs(u_plus-u_this)
    theta = (u_plus-u_this)/(u_this-u_mnus);
    slope = ((1+theta)/(1+theta*theta))*((u_plus-u_this)/h);
  else
    if u_plus == u_mnus
      slope = 0.0;
    else
      slope = (u_plus-u_this)/h;
    end
  end
end
