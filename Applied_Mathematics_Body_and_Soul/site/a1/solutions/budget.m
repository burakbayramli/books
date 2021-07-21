function [individual_costs, total_cost] = budget(n, p)
% budget(n, p)
%
% Computes the individual costs and total cost of newspapers, movies and CDs.
% n is a vector containing the number of each item and p is a vector containing
% the prices of the items.

  individual_costs = n .* p;
  total_cost = sum(individual_costs);
