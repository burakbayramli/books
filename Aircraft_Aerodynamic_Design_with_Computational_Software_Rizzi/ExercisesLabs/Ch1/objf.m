function FoM = objf(x)
  global DEMO XTab Itno
  Itno = Itno+1;
  if DEMO==1
    prop = cfd(x);
    FoM  = x(2)^2*prop(2)/cos(x(1));
    XTab(end+1,:) = x';
  elseif DEMO == 2
    prop = cfd(x);
    FoM = -x(2)*sin(x(3));
    XTab(end+1,:) = [-FoM,x'];
    if rem(Itno,100) == 0
      [cn cl] = constr(x);
      disp(['it: ' num2str(Itno) 'FoM ' num2str(-FoM) ' x: ' num2str(x') ' cl ' num2str(cl) ])
    end
  else
    error(['DEMO ' num2str(DEMO) ' not impl'])
  end
