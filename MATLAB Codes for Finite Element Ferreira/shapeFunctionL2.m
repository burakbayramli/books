function [shape,naturalDerivatives] = shapeFunctionL2(xi)
  shape = ([1-xi,1+xi]/2)';
  naturalDerivatives = [-1;1]/2;
end % end function shapeFunctionL2
