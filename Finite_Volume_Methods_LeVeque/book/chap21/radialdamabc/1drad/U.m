function U = U(data);
% compute the velocity from the momentum and depth:
U = data(:,2)/data(:,1);
