clear all; close all;

n = 7;
A = diag([1,1.5,2, 5,5.25,5.5, 7]);
rand('state',364);
b = rand(7,1);
t = 0:0.1:10;

x_star =  inv(A)*b; f_star = -0.5*b'*inv(A)*b;
b2 = (A^(-0.5))*b;
C = []; eta = []; relres = [];

figure; 
plot(eig(A), zeros(n,1), 'k.', 'MarkerSize', 15);
hold on; colorchoice =['b', 'g', 'm', 'r'];
for i=1:7
    C = [C (A^(i-0.5))*b];
    u = C\b2;
    p = [-flipud(u)' 1];    
    p_plot = polyval(p, t);
    if(i < 5)
        plot(t, p_plot, colorchoice(i));
    end    
    
    eta(i) =  0.5*norm(C*u - b2)/-f_star;
    relres(i) = norm(b - A^(0.5)*C*u)/norm(b);
    
end
plot(t, p_plot, 'k');

hold off; axis([0 10 -2 2]);
set(gca,'FontSize', 16, 'FontName', 'Times');
xlabel('x'); ylabel('p(x)');
print('-depsc', 'ex1_poly.eps');

relres = [1, relres];
figure;  plot(0:n, relres);
set(gca,'FontSize', 16, 'FontName', 'Times');
xlabel('cgiter'); ylabel('relres');
print('-depsc', 'ex1_relres.eps');

eta = [1, eta];
figure;  plot(0:n, eta);
set(gca,'FontSize', 16, 'FontName', 'Times');
xlabel('cgiter'); ylabel('eta');
print('-depsc', 'ex1_eta.eps');

    
    