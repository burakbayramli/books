function UNext = FE(L, U, t, dt, updateBoundary)
%FE   Forward Euler discretization

	netFlux = L(U, t, dt);
	UNext = U + dt*netFlux;
	UNext = updateBoundary(UNext, t+dt);

end