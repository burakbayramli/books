function [wn]=god_btbc(wa,dtdx,m)
			     %
			     % Godunov scheme for the Burgers equation
			     %
  fw=zeros(1,m+1);
  fwn=zeros(1,m+1);
  wn=zeros(1,m+1);
  fw(1:m+1)=0.5*wa(1:m+1).^2;
				%
  for i=1:m
    wl=wa(i);
    wr=wa(i+1);
    s=(wl+wr)/2;
    if wl > wr
				% Entropy shock
      if s < 0
	fw(i) = 0.5*wr^2
      end
    elseif wl < wr
				% Expansion wave
      if wr < 0
	fw(i) = 0.5*wr^2 ;

      elseif wl > 0.
	fw(i) = 0.5*wl^2 ;
      else
	fw(i) = 0;
      end
    end
  end
				%
  fwn(2:m+1)=fw(1:m);
				% Transmissive boundary conditions
  fwn(1)=fw(1);
				%
  wn(1:m+1)=wa(1:m+1)-dtdx*(fw(1:m+1)-fwn(1:m+1));
				%
end
