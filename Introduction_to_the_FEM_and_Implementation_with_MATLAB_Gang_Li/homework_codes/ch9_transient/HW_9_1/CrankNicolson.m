% Time integration using the central difference (CrankNicolson) scheme
% Input: dt: time step
% Input: K, C, F: condution matrix, heat capacity matrix, heat source
%        vector, respectively 
% Input: nodalTemp: temperature boundary condition vector
% Input: prevTemp,currTemp: temperature vector at previous and current 
%        time step, respectively 
% Input: penalty: penalty number for apply nodal temperature BC
% Output: currTemp: current temperature vector
function currTemp=CrankNicolson(dt,K,C,F,nodalTemp,prevTemp,currTemp,penalty)
Cp=C*(1.0/dt);
Kp=K*0.5;
LHS = Cp+ Kp;    %left hand side matrix 
RHS = F + (Cp - Kp)*prevTemp;   %right hand side vector
% for-loop: apply nodal temperature BC
for i=1:size(nodalTemp,1);
  row=nodalTemp(i,1);
  RHS(row,1) =nodalTemp(i,2)*penalty;
  LHS(row, row)=penalty;
end
currTemp=LHS\RHS; % solve for the current temperature vector