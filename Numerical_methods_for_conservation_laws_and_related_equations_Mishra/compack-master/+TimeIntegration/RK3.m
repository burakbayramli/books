function UNext = RK3(L, U, t, dt, updateBoundary)
%RK3   Third-order SSB Runge-Kutta

	U1 = U + dt*L(U, t, dt);
	U1 = updateBoundary(U1, t+dt);
	U2 = 3*U/4 + (U1 + dt*L(U1, t, dt))/4;
	U2 = updateBoundary(U2, t+dt);
	UNext = U/3 + 2/3*(U2 + dt*L(U2, t, dt));
	UNext = updateBoundary(UNext, t+dt);
	
end