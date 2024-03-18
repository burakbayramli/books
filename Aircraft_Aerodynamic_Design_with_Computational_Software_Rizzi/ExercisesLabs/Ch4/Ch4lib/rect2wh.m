function xy = rect2wh(xyi)
  w = xyi(2,1)-xyi(1,1);
  h = xyi(3,2)-xyi(1,2);
  xy = [xyi(1,:) w h];
endfunction
