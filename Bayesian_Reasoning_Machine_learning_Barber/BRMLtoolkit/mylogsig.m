function y=mylogsig(x)
% if abs(x)>5
%     if x>0
%         z=exp(-x);y=-(z-0.5*z.^2);
%     else
%         z=exp(x);y=x-(z-0.5*z.^2);       
%     end
% else
x=cap(x,500);
    if x>0
        y=x-log(1+exp(x));
    else
        y=-log(1+exp(-x));
    end
%end