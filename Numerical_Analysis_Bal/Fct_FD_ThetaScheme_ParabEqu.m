function u=fdtheta(u,x,h,T,lambda,theta);


%%% Data:
%%% u: initial data
%%% x: spatial grid
%%% h: cell size
%%% T: final time
%%% lambda, theta: characteristics of discretization

%%% Finite Differences Theta Scheme for Parabolic Equation

Nx=length(x)-1;

dT=lambda*h^2;
Nt=floor(T/dT);

%%%% Construction of matrices A and B
D=zeros(Nx-1,1);
D(1)=1-2*(1-theta)*lambda;
D(2)=(1-theta)*lambda; 
A=toeplitz(D);  
D(1)=1+2*theta*lambda;
D(2)=-theta*lambda; 
B=toeplitz(D); 


for i=1:Nt
    u(2:Nx)=B\(A*u(2:Nx));
    figure(1); plot(x,u); axis([-10 10 -1 1]); drawnow;
end;
[Nt*dT 1000*dT]