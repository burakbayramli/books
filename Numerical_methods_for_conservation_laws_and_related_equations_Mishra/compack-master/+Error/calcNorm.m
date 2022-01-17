function ret = calcNorm( mesh, u, p )
%CALCNORM   Calculate L^p norm.
%   Calculates the L^p (for 1 <= p <= inf) norm of u.
%   If p<0, then the return value is the integral of u over the domain.

	if p < 0
		p = 1;
		uAbs = u;
	else
		uAbs = abs(u);
	end
		

	if isinf(p)
		ret = max(max(uAbs));
	else
		if isa(mesh, 'Mesh.Tri')
			ret = (sum(mesh.dx .* (uAbs.^p))) ^ (1/p);
		elseif mesh.ndims == 1
			ret = sum(mesh.dx{1}.*uAbs.^p) ^ (1/p);
		elseif mesh.ndims == 2
			ret = sum(sum(mesh.dx{1}.*mesh.dx{2}.*uAbs.^p)) ^ (1/p);
		end
	end

end

