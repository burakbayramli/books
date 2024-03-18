function ca = addto(ca, cb);
  n = length(cb);
  for k= 1:n
    tmp = cb{k};
    ca{end+1}=tmp;
  end
