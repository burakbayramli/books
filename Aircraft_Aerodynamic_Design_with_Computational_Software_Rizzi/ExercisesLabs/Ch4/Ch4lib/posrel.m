function xywh = posrel(xywh1,xywh0)
  sc = [1/xywh0(3) 1/xywh0(4)];
  xywh = xywh0;
  xywh(1:2) = (xywh1(1:2)-xywh0(1:2)).*sc;
  xywh(3:4) =  xywh1(3:4).*sc;
endfunction
