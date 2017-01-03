function printTable(obj)
% Print the elements in the table
% where we show the assigment to all but the last variables in each row
% and iterate over the last varibale in each column (as in a stochastic matrix)

display_CPT(obj.T)


%%%%%%%%


function display_CPT(CPT)

n = ndims(CPT);
parents_size = size(CPT);
parents_size = parents_size(1:end-1);
child_size = size(CPT,n);
c = 1;
for i=1:prod(parents_size)
  parent_inst = ind2subv(parents_size, i);
  fprintf(1, '%d ', parent_inst);
  fprintf(1, ': ');
  index = num2cell([parent_inst 1]);
  index{n} = ':';
  fprintf(1, '%6.4f ', CPT(index{:}));
  fprintf(1, '\n');
end
