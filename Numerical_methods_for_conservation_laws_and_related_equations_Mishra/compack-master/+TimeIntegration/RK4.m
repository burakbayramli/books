function UNext = RK4(L, U, t, dt, updateBoundary)
%RK4   Fourth-order Runge-Kutta (the original RK method)

	U1 = U + dt/2*L(U, t, dt);
	U1 = updateBoundary(U1, t+dt);
	U2 = U + dt/2*L(U1, t, dt/2);
	U2 = updateBoundary(U2, t+dt/2);
	U3 = U + dt*L(U2, t, dt/2);
	U3 = updateBoundary(U3, t+dt/2);
	U4 = U3 + dt/2*L(U3, t, dt);
	UNext = (-U + U1 + 2*U2 + U4)/3;
	UNext = updateBoundary(UNext, t+dt);
	
end