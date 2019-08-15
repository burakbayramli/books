function VN = bsp04h(p,e,V,T,Parmeter);
A = Parmeter(1); Period = Parmeter(2);
VN = V;

I = find(e(5,:) == 1); LI = length(I); % Rand 1
E = [e(1,I),e(2,I(LI))];
VN(2,E) = 0;

I = find(e(5,:) == 2); LI = length(I); % Rand 2
E = [e(1,I),e(2,I(LI))];
VN(:,E) = 0;

I = find(e(5,:) == 3); LI = length(I); % Rand 3
E = [e(1,I),e(2,I(LI))];
VN(2,E) = 0;

