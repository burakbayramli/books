function SEQL = bsp01b(X)
SEQL = 0;
Y = feval('bsp01',X,3); Y = Y(:);
if ~isempty(Y), SEQL = SEQL + Y.'*Y; end
Y = feval('bsp01',X,2); Y = Y(:);
if ~isempty(Y),
   J = find(Y < 0);
   if ~isempty(J), SEQL = SEQL + Y(J).'*Y(J); end
end
SEQL = sqrt(SEQL);
