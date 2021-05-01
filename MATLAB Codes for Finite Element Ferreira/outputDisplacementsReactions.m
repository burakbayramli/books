function outputDisplacementsReactions(displacements,stiffness,GDof,prescribedDof)
  disp('Displacements')
  jj = 1:GDof; format
  [jj' displacements]
  F = stiffness*displacements;
  reactions = F(prescribedDof);
  disp('reactions')
  [prescribedDof reactions]
end
