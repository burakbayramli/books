function UNext = RK2(L, U, t, dt, updateBoundary)
%RK2   Second-order SSB Runge-Kutta

	U1 = U + dt*L(U, t, dt);
	U1 = updateBoundary(U1, t+dt);
	U2 = U1 + dt*L(U1, t, dt);
	UNext = 0.5*(U + U2);
	UNext = updateBoundary(UNext, t+dt);

end