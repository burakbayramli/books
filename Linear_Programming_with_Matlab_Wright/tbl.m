function tbl(H)
% syntax: tbl(H)
% print out the tableau given in H
% row and column labels in last two (or four) rows and columns

[m,n] = size(H.val);
if isfield(H,'dualbas')
  fprintf('            ');
  for j=1:n
    fprintf('      %-3s= ',H.dualbas{j});
  end
  fprintf('\n            ');
  for j=1:n
    fprintf('       %-3s ',H.nonbas{j});
  end
  fprintf('\n           ');
  for j=1:n
    fprintf('-----------');
  end
  fprintf('\n');
  for i=1:m
    if ~isempty(H.obj) & (i == H.obj)
      fprintf('           ');
      for j=1:n
	fprintf('-----------');
      end
      fprintf('\n');
      fprintf(' %-3s %-3s= | ',H.dualnonbas{i},H.bas{i});
    else
      fprintf('-%-3s %-3s= | ',H.dualnonbas{i},H.bas{i});
    end
    for j=1:n
      fprintf('%10.4f ',H.val(i,j));
    end
    fprintf('\n');
  end
else
  fprintf('       ');
  for j=1:n
    fprintf('       %-3s ',H.nonbas{j});
  end
  fprintf('\n       ');
  for j=1:n
    fprintf('-----------');
  end
  fprintf('\n');
  for i=1:m
    if ~isempty(H.obj) & (i == H.obj)
      fprintf('       ');
      for j=1:n
	fprintf('-----------');
      end
      fprintf('\n');
    end
    fprintf(' %-3s= | ',H.bas{i});
    for j=1:n
      fprintf('%10.4f ',H.val(i,j));
    end
    fprintf('\n');
  end
end

return;
