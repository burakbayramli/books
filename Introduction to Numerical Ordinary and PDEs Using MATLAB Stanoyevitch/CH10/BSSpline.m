function y = BSSpline(x)
%Basic cubic spline function of Chapter 10, (51)
%Function built to accept vector arguments.
for i=1:length(x)
    if x(i)>=0 & x(i)<=1
        y(i)=((2-x(i))^3-4*(1-x(i))^3)/4;
    elseif x(i)>1 & x(i)<=2
        y(i)=(2-x(i))^3/4;
    elseif x(i)>2
        y(i)=0;
    else, y(i) = BSSpline(-x(i));
    end
end
