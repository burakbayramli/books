function tam_HW5_2
    %Order of variables in vectors: 1=h 2=q2
    A=1;
    R=0.1;
    h0=4;

    %Set up mass matrix
    M=[1 0; 0 0];

    %Define q1
    q1=@(t)0.1*sin(0.02*pi*t).^2;

    %Define f
    fun=@(t,x)[(q1(t)-x(2))/A; x(2)-R*sqrt(x(1))];

    %Define options
    opt=odeset('Mass', M, 'MassSingular', 'yes');

    %Consistent initial conditions
    ic=[h0 R*sqrt(h0)];

    %Solve and plot
    [T, X]=ode15s(fun,[0 200],ic, opt);
    plot(T,X(:,1), T, X(:,2));
    legend('h(t)','q2(t)');
    xlabel('t');
    ylabel('h (m),q_2 (m^3/s)')
end