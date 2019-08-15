function test03
% Testen der Orientierung beim QR-Algorithmus
clc
flag = 4;
if flag == 1
   A = rand(6,6);
   b = rand(6,1);
   %[Q,R] = qr(A);
   %SS = det(Q)
   [Q,R,E] = qr(A); z = Q'*b; y = R\z; x = E*y;
   DIFF1 = norm(A*x - b)
end
% -- A quadratisch und singulaer ----
if flag == 2
   A = zeros(5,10);
   A(1,:) = rand(1,10);
   A(2,:) = rand(1,10);
   A(3,:) = A(1,:) + sqrt(2)*A(2,:);
   A(4,:) = sqrt(2)*A(1,:) + A(2,:);
   A(5,:) = rand(1,10);
   A = [A;A];
   b = rand(10,1);
   x1 = A\b
   disp('--------------------')
   [Q,R,E] = qr(A);
   z = Q'*b;
   y = R\z;
   x2 = E*y
   DIFF = A*x2 - b
   %DIFFNORM = norm(x1 - x2)
   x3 = pinv(A)*b
end
% -- m > n und Rangdefekt
if flag == 3
   A = zeros(5,10);
   A(1,:) = rand(1,10);
   A(2,:) = rand(1,10);
   A(3,:) = A(1,:) + sqrt(2)*A(2,:);
   A(4,:) = sqrt(2)*A(1,:) + A(2,:);
   A(5,:) = rand(1,10);
   A = [A;A;A];
   b = rand(15,1);
   x1 = A\b
   disp('--------------------')
   [Q,R,E] = qr(A);
   z = Q'*b;
   y = R\z;
   x2 = E*y
   %DIFF = A*x2 - b
   DIFFNORM = norm(x1 - x2)
   x3 = pinv(A)*b
end
if flag == 4
   A = zeros(5,10);
   A(:,1) = rand(5,1);
   A(:,2) = rand(5,1);
   A(:,3) = A(:,1) + sqrt(2)*A(:,2);
   A(:,4) = sqrt(2)*A(:,1) + A(:,2);
   A(:,5) = rand(5,1);
   A = [A, A, A];
   b = rand(5,1);
   x1 = A\b
   disp('--------------------')
   [Q,R,E] = qr(A);
   z = Q'*b;
   y = R\z;
   x2 = E*y
   %DIFF = A*x2 - b
   DIFFNORM = norm(x1 - x2)
   x3 = pinv(A)*b
end