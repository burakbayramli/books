
% Trilateration algorithm
% paper "An algebraic solution to the multilateration problem"
% Author: Norrdine, Abdelmoumen  (norrdine@hotmail.de)
% https://www.researchgate.net/publication/275027725_An_Algebraic_Solution_to_the_Multilateration_Problem
% usage: [N1 N2] = RecTrilateration(P,S,W) 
% P = [P1 P2 P3 P4 ..] Reference points matrix
% S = [s1 s2 s3 s4 ..] distance matrix.
% W : Weights Matrix (Statistics).
% N : calculated solution
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY!!
function Nmat = RecTrilateration(P,S,W)
[mp,np] = size(P);
ns = length(S);
if (ns~=np)
    error('Anzahl der Referenzpunkte und der Strecken sind nicht gleich');
end
A=[]; b=[];
for i1=1:np
    x = P(1,i1); y = P(2,i1); z = P(3,i1);
    s = S(i1);
    A = [A ; 1 -2*x  -2*y  -2*z]; 
    b= [b ; s^2-x^2-y^2-z^2 ];
end
if (np==3)
    warning off;
    Xp= A\b;  % Gaussian elimination 
    % or Xp=pinv(A)*b; 
   % the matrix  inv(A'*A)*A' or inv(A'*C*A)*A'*C or pinv(A)
    % depend only on the reference points
    % it could be computed only once
    xp = Xp(2:4,:);
    Z = null(A);
    z = Z(2:4,:);
    if rank (A)==3
        %Polynom coeff.
        a2 = z(1)^2 + z(2)^2 + z(3)^2 ;
        a1 = 2*(z(1)*xp(1) + z(2)*xp(2) + z(3)*xp(3))-Z(1);
        a0 = xp(1)^2 +  xp(2)^2+  xp(3)^2-Xp(1);
        p = [a2 a1 a0];
        t = roots(p);

        %L�sungen
        N1 = Xp + t(1)*Z;
        N2 = Xp + t(2)*Z;
        Nmat(:,1) = N1;
        Nmat(:,2) = N2;
    end
end
A0 = A(1:3,:);
if  (np>3)
    P10=P(:,1:3);S10=S(:,1:3);W0=W(1:3,1:3);
    N0mat = RecTrilateration(P10,S10,W0);
    N01 = N0mat(:,1);
    N02 = N0mat(:,2);

    %select N0
    C = W'*W;
    Xpdw =inv(A'*C*A)*A'*C*b;
    % the matrix  inv(A'*A)*A' or inv(A'*C*A)*A'*C or pinv(A)
    % depend only on the reference points
    % it could be computed only once
    NormErrorXpdw = Xpdw(1)-norm(Xpdw(2:4))^2;
    if (norm(Xpdw(2:4)-N01(2:4))<norm(Xpdw(2:4)-N02(2:4)))
        N0 = N01;
    else
        N0= N02;
    end
    
    Nmat(:,1)= N01;
    Nmat(:,2)= N02;
    
    W0 = W(1:3,1:3);
    C0 = W0*W0';
    P_0 = inv(A0'*C0*A0);
    
    %Start solution
    invP_i_1 = inv(P_0); 
    xi_1 = N0;
  
    
     % Rekursive Least square (Introduction to applied Math Strang pp 147)
    x0 = N0;
    [x,P] = lsrec(x0,1);
    for i=1:np-3
        An = A(i+3,:);
        Wn = W(i+3,i+3);
        yn = b(i+3);
        [xn,Pn] = lsrec(yn,An,1,x,P);
        x=xn; P=Pn;
        Nmat(:,i+2) = xn;
    end
      Nmat(:,i+3)= Xpdw;
end