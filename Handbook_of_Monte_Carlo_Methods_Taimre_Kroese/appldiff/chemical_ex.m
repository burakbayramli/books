% chemical_ex.m
c = [1,1,1,1,1,10]; Vs=[10^0,10^1,10^2]; nV=length(Vs);
w0 = 100; x0=100; y0=100; z0=100;
% Deterministic Limit
t0=0; t1=0.25;
options = odeset('RelTol',1e-4);
[T,Y] = ode45(@(t,y) gillespie_ode(t,y,c),[t0 t1], ...
              [w0,x0,y0,z0],options);
for k=1:nV
    V=Vs(k); % Volume (scaling) parameter
    w=w0*V; x=x0*V; y=y0*V; z=z0*V;
    ww=[];xx=[];yy=[];zz=[];tt=[];
    t = 0;
    while t<t1
        ww=[ww,w]; xx=[xx,x]; yy=[yy,y]; zz=[zz,z]; tt=[tt,t];
        v = [x, y, x*(x-1)/(2*V), z, w*x/V, x*(x-1)/(2*V)]; 
        a = c.*v;
        lam = sum(a);
        p = a/sum(a);
        t = t -log(rand)/lam;
        r = min(find(cumsum(p)>=rand));
        switch r
            case 1
                x = x-1; y = y+1;
            case 2
                x = x+1; y = y-1;
            case 3
                x = x-2; z = z+1;
            case 4
                x = x+2; z = z-1;
            case 5
                x = x+1; w = w-1;
            case 6
                x = x-1; w = w+1;
        end
    end

%    Uncomment the following for plotting purposes

%    figure,plot(T,Y(:,1),'k-'),hold on,
%    plot([tt(1:end-1);tt(2:end)],[ww(1:end-1);ww(1:end-1)]./V,'k-');
%    xlabel('$t$','Interpreter','Latex','Fontsize',12)
%    ylabel('$w_V(t)$','Interpreter','Latex','Fontsize',12)
%    ax=axis;axis([t0 t1 ax(3) ax(4)]);hold off
    
%    figure,plot(T,Y(:,2),'k-'),hold on,
%    plot([tt(1:end-1);tt(2:end)],[xx(1:end-1);xx(1:end-1)]./V,'k-');
%    ax=axis;axis([t0 t1 ax(3) ax(4)]);hold off
%    xlabel('$t$','Interpreter','Latex','Fontsize',12)
%    ylabel('$x_V(t)$','Interpreter','Latex','Fontsize',12)
    
%    figure,plot(T,Y(:,3),'k-'),hold on,
%    plot([tt(1:end-1);tt(2:end)],[yy(1:end-1);yy(1:end-1)]./V,'k-');
%    ax=axis;axis([t0 t1 ax(3) ax(4)]);hold off
%    xlabel('$t$','Interpreter','Latex','Fontsize',12)
%    ylabel('$y_V(t)$','Interpreter','Latex','Fontsize',12)
    
%    figure,plot(T,Y(:,4),'k-'),hold on,
%    plot([tt(1:end-1);tt(2:end)],[zz(1:end-1);zz(1:end-1)]./V,'k-');
%    ax=axis;axis([t0 t1 ax(3) ax(4)]);hold off
%    xlabel('$t$','Interpreter','Latex','Fontsize',12)
%    ylabel('$z_V(t)$','Interpreter','Latex','Fontsize',12)
    
end

