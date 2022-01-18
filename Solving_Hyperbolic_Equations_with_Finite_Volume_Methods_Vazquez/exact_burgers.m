function [w] = exact_burgers(x,t,xb,m,wl,wr,s)
%
% Entropic solution for Burgers equation
%
  if wl >= wr
    %
    % Entropy shock
    %
    for i=1:m+1
      if (x(i)-xb)/t < s
	w(i) = wl ;
      else
	w(i) = wr ;
      end
    end
  else
    %
    % Expansion wave
    %
    for i=1:m+1
      if (x(i)-xb)/t <wl
	w(i) = wl ;
      elseif (x(i)-xb)/t >=wr

	w(i) = wr;
      else
	w(i) = (x(i)-xb)/t ;
      end
    end
  end
  
