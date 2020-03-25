clear all, clf
Deltas=2.^(-(2+(0:1:3).*2)); T=1; mu=2; sigma=1;
X0=1;
base_d=2;
d=length(Deltas);
h=min(Deltas);
n=T/h;

% Common Gaussians
V=randn(1,n);

% Exact
tmin=(0:1:(n)).*h;
S=X0.*exp((mu-((sigma^2)/2)).*tmin + sigma.*sqrt(h).*[0,cumsum(V)]);

h_temp=figure;
plot(tmin,S,'g-','LineWidth',1)
ax_common=axis;
ax_common(3)=ax_common(3)-2*max(Deltas);
ax_common(4)=ax_common(4)+2*max(Deltas);
close(h_temp)

for k=1:d
    delta=Deltas(k);
    nd=T/delta;
    td=(0:1:(nd)).*delta;
    
    % Euler
    X=zeros(size(td));
    X(1)=X0;
    for i=2:(nd+1)
        X(i)=X(i-1)*(1+mu*delta+sigma*sqrt(h)*sum(V((td(i-1)/h)+1:1:td(i)/h)));
    end
    
    % Milstein
    Y=zeros(size(td));
    Y(1)=X0;
    for i=2:(nd+1)
        Y(i)=Y(i-1)*(1+mu*delta+sigma*sqrt(h)*sum(V((td(i-1)/h)+1:1:td(i)/h)) +((sigma^2)/2)*(h*((sum(V((td(i-1)/h)+1:1:td(i)/h)))^2)-delta));
    end
    
    ax=subplot(2,2,k);
    hold on
    plot(td,S(1+td./h),'k-','LineWidth',1)
    plot(td,X,'k--','LineWidth',1)
    plot(td,Y,'k:','LineWidth',1)
    hold off
    set(ax,'FontSize',14,'Box','off')
    xlabel(sprintf('$t$ $(\\Delta = %g^{%g})$',base_d,log(delta)/log(base_d)),'FontSize',16,'Interpreter','Latex'),ylabel('$X_t$','FontSize',16,'Interpreter','Latex')
    axis(ax_common)
end
legend('Exact','Euler','Milstein')






