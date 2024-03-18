function plquad(rektcell)
  n = length(rektcell);
  for k = 1:n
    rekt = rektcell{k};
    plot([rekt(:,1);rekt(1,1)],[rekt(:,2);rekt(1,2)],'-r','linewidth',3)
  end
