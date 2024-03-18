function tab = sort_col_1(tab)
[s, j]= sort(tab(:,1));
tab = tab(j,:);
end