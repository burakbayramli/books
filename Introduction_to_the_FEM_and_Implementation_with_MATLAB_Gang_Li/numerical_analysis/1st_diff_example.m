
clear all;

x0=1.0
dx=10.^[-3:.1:0];

for k=1:length(delta);
    dudx_f(k) = ((a+delta(k))^3-(a+delta(k))^2 ...
                  -a^3 + a^2)/delta(k);
    dudx_b(k) = (a^3-a^2-(a-delta(k))^3...
                 +(a-delta(k))^2)/delta(k);
    dudx_c(k) = ((a+delta(k))^3-(a+delta(k))^2...
                -(a-delta(k))^3+(a-delta(k))^2)...
                /(2*delta(k));
end

semilogx(delta,dudx_f,'k--','LineWidth',2);
hold on
semilogx(delta,dudx_b,'k:','LineWidth',2);
semilogx(delta,dudx_c,'k','LineWidth',2);
hold off

set(gca,'fontsize',16);
xlabel('x','fontsize',18);
ylabel('du/dx','fontsize',18);
legend('Forward difference',...
      'Backward diffference',...
      'Central difference');
print -depsc diff_example.eps