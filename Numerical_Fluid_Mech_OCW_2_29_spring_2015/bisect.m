% Root finding by bi-section
f=inline(' a*x -1','x','a');

a=2
figure(1)
clf
hold on
x1=0;
x2=1.5;
x=[x1 x2];
eps=1e-3;
err=max(abs(x(1)-x(2)),abs(f(x(1),a)-f(x(2),a)));
while (err>eps & f(x(1),a)*f(x(2),a) <= 0)
    xo=x;
    x=[xo(1) 0.5*(xo(1)+xo(2))];
    if ( f(x(1),a)*f(x(2),a) > 0 )
        x=[0.5*(xo(1)+xo(2)) xo(2)]
    end
    x
    err=max(abs(x(1)-x(2)),abs(f(x(1),a)-f(x(2),a)));
    b=plot(x,f(x,a),'.b');
    set(b,'MarkerSize',20);
    grid on;
end
