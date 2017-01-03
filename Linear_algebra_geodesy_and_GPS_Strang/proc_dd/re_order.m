function [L, D, a, Zti] = re_order(n, L, D, a, Zti)
% RE_ORDER Computation of the Z-transformation matrix
%
%      The final Z-transformation is constructed from a sequence of
%      interchanges of two neighbouring ambiguities (this function) and
%      integer Gauss transformations (function ztransi) that pair-wise
%      decorrelate the ambiguities.

% n    dimension of the system
% L    lower triangular matrix L
% D    diagonal matrix D (only diagonal stored)
% a    Z^T* a,  a being the original vector of ambiguities
% Zti  inverse Z transposed transformation matrix

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Recoded into MATLAB by Kai Borre 12-04-96

i1 = n - 1;
sw = 'true ';   % The logical sw traces whether an
                % interchange has occured or not
while sw == 'true '
   i = n;
   sw = 'false';
   while (sw == 'false') & (i > 1)
      i = i - 1;
      if (i <= i1)
         [L, a, Zti] = ztransi(i, i, n, L, a, Zti); 
      end
      delta = D(i) + L(i+1,i)^2 * D(i+1);
      if (delta < D(i+1))
         lambda(3) = D(i+1) * L(i+1,i) / delta;
         eta = D(i) / delta;
         D(i) = eta * D(i+1);
         D(i+1) = delta;
         for j = 1:i-1
            lambda(1) = L(i,j);
            lambda(2) = L(i+1,j);
            L(i,j)   = lambda(2) - L(i+1,i) * lambda(1);
            L(i+1,j) = lambda(3) * lambda(2) + eta * lambda(1);
         end
         L(i+1,i) = lambda(3);
         for j = i+2:n   % to be coded effectively:
                         % interchange of columns L(i+2:n,i)
                         % and L(i+2:n,i+1)
            help = L(j,i);
            L(j,i) = L(j,i+1);
            L(j,i+1) = help;
         end
         for j = 1:n    % to be coded effectively:
                        % swap Zti(1:n,i) with Zti(1:n,i+1)
            help = Zti(j,i);
            Zti(j,i) = Zti(j,i+1);
            Zti(j,i+1) = help;
         end
         help = a(i);
         a(i) = a(i+1);
         a(i+1) = help;
         i1 = i;
         sw = 'true ';
      end
   end
end
%%%%%%%%%%%%%% end re_order.m %%%%%%%%%%%%%%%%%%%%%%%%
