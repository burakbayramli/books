function status = mybound(t,y,gflag,flag,parmeter3)
% Schranke : begrenzt absolute Integrationswerte
Schranke = 1.0E4;
status = 0;
if norm(y) > Schranke
   status = 1;
end
