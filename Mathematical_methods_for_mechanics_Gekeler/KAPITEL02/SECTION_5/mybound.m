function status = mybound(t,y,gflag,flag,Parmeter3)
% muss gleiche Parameter wie BSP haben!
% Schranke : begrenzt absolute Integrationswerte
Schranke = 1.1E1;
status = 0;
if norm(y) > Schranke
   status = 1;
end
