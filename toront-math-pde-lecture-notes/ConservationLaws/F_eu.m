function y = F(uL,uR,dt,dx)

% define the flux
fuL = (uL.^2)/2;
fuR = (uR.^2)/2;

slope = (fuR-fuL)./(uR-uL);
y = fuL.*(1-(slope>=0)-(slope<0)) + fuL.*(slope>=0) + fuR.*(slope<0);

% below is the non-vectorized code which does the same as the above.
%
% for j=1:length(fuL)
%     slope = (fuR(j)-fuL(j))/(uR(j)-uL(j));
%     if uR(j) == uL(j)
%         z(j) = fuL(j);
%     elseif slope >= 0
%         z(j) = fuL(j);
%     else
%         z(j) = fuR(j);
%     end
% end
