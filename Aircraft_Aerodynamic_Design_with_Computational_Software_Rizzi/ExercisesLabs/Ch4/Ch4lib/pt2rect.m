function r = pt2rect(p)
  global ButWH
  r =  ones(4,1)*p+ [-1 -1; 1 -1; 1 1; -1 1]*ButWH;;
endfunction
