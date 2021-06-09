function y = BSprime(x)
%Derivative of basic cubic spline function of Chapter 10, (52)
%Function built to accept vector arguments.
for i=1:length(x)
    if x(i)>=0 & x(i)<=1
        y(i)=3/4*(4*(1-x(i))^2-(2-x(i))^2);
    elseif x(i)>1 & x(i)<=2
        y(i)=-3/4*(2-x(i))^2;
    elseif x(i)>2
        y(i)=0;
    else, y(i) = -BSprime(-x(i));
    end
end
