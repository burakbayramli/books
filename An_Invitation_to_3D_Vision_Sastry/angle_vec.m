function ang = angle(a,b)
% Angle between two vectors.
  if (norm(a) ~= 0) & (norm(b) ~= 0)
    % numerics up to 5 decimal places
    arg = round((dot(a,b)/(norm(a)*norm(b)))*10^6)/10^6;
    ang = acos(arg)/pi*180;
     if ang > 90
       ang = 180 - ang;
     end
   else
   ang = 0;
 end;
