A=[3 4 6 0; 0 5 0 0; 0 0 7 -1; 0 0 0 -3]; % A matrix (dense)
b=[-2 1 -2 4]';                           % b vector (dense)
x=A\b                                     % solve Ax=b
AS=sparse(4,4);                           % A matrix (sparse) 
AS(1,1)=3; AS(1,2)=4; AS(1,3)=6;          % set the entries 
AS(2,2)=5; AS(3,3)=7; AS(3,4)=-1; AS(4,4)=-3; % of A matrix
bs=sparse([-2 1 -2 4]');                  % b vector (sparse)
x=AS\bs                                   % solve Ax=b