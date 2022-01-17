function ind = findNearest( a, x )
%FINDNEAREST   Finds the nearest value of a in x.
%   Returns a vector 'ind' with the property that for each
%   k=1,...,length(a), ind(k) is the smallest natural number such that
%      |x(ind(k)) - a(k)| <= |x(m) - a(k)|		for all m.


	s = size(a);
	a = a(:)';
	x = x(:);
	n = length(a);
	ind = zeros(1,n);
	for k = 1:n
		[dummy, ind(k)] = min(abs(x - a(k)));
	end
	ind = reshape(ind, s);
end