function y = year(d)
% Return the year, given a string date
c = datevec(datenum(d(:)));
y = c(:,1);
