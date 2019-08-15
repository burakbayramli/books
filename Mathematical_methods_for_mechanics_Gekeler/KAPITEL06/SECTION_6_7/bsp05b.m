function Y = bsp05b(T,X,Parmeter);
% Differential system for top 
% in Euler angles, mass matrix
T1  = Parmeter(1);
T3  = Parmeter(2);
m   = Parmeter(3);
gl  = Parmeter(4);
T1  = m*T1; T3  = m*T3; mgl = m*gl;
MM  =  [T1*sin(X(2))^2+T3*cos(X(2))^2,  0, T3*cos(X(2));
        0                            , T1, 0;
        T3*cos(X(2)),                   0, T3];
Y   = [eye(3), zeros(3,3); zeros(3,3), MM];
%MM = sparse(MM);
