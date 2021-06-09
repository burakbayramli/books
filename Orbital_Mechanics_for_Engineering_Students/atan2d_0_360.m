
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function t = atan2d_0_360(y,x)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%{
  This function calculates the arc tangent of y/x in degrees
  and places the result in the range [0, 360].

  t - angle in degrees

%}
% ----------------------------------------------

if x == 0
    if y == 0
        t = 0;
    elseif y > 0
        t = 90;
    else
        t = 270;
    end    
elseif x > 0
    if y >= 0
        t = atand(y/x);
    else
        t = atand(y/x) + 360;
    end    
elseif x < 0   
    if y == 0
        t = 180;
    else
        t = atand(y/x) + 180;
    end       
end

end

% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~