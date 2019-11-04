function zlist=plot_landscape(v, nl, n1, n2, xlab, ylab, bounds, fobj, optarg)
z=0:(nl-1); z=z'/(nl-1);
d1=bounds(n1,2)-bounds(n1,1);
x1=bounds(n1,1) + d1*z;
d2=bounds(n2,2)-bounds(n2,1);
x2=bounds(n2,1) + d2*z;
mv=length(v);
vlist=zeros(mv,nl*nl);
for i=1:nl*nl
   vlist(:,i)=v;
end
for j=1:nl
    jset=(j-1)*nl;
    for i=1:nl
        vlist(n1,jset+i)=x1(i);
        vlist(n2,jset+i)=x2(j);
    end
end
[fz,ifz,icostz]=feval(fobj, vlist, optarg);
zlist=reshape(fz,nl,nl);
zlist=zlist';
p2=subplot(1,1,1);
p1=mesh(x1,x2,zlist,'EdgeColor','black');
xlabel(xlab,'FontSize',12,'FontWeight','bold');
ylabel(ylab,'FontSize',12,'FontWeight','bold');
set(p2,'FontSize',14);

