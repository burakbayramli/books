function plradbut(butcell,rr)
  if nargin < 2
    rr = 8;
  end
  np = 12;
  fi = linspace(0,2*pi,np);
  efi = exp(1i*fi);
  n = length(butcell);
  for k = 1:n
    p = butcell{k};
    plot(p(1)+real(rr*efi),p(2)+rr*imag(efi),'k','linewidth',2);
  end
