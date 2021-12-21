%%
clear
close all
n_conv = [10 20 40 80 160 320 640];

nEval = 2000;
rad = 2;

nG = 5; % number of Gauss quad points

thetaEval = linspace(0,2*pi,nEval+1);

sigma = zeros(nEval,length(n_conv));

for i_conv = 1:length(n_conv)
    n = n_conv(i_conv);
    
    theta = linspace(0,2*pi,n+1);
    
    x = (rad*exp(1i*theta)).';
    
    side_length = abs(diff(x)); % lengths of each artificial side
    
    tv_abs = diff(x);   % absolute tangent vectors (not normalized)
    tv = tv_abs./side_length; % tangent vectors (note normalization)
    nv = imag(tv) - 1i*real(tv); % normal vectors to sides
    
    x_c = x(1:end-1) + tv.*side_length./2;
    
    [xL,wL] = quad_gengauss_log(5); % quadrature points and weight for sing int.
    
    [xG,wG] = gauss(nG);
    A=zeros(n,n);
    for i=1:n
        for j=1:n
            if i == j
                xGauss = [x_c(i) - flipud(xL)*tv(i)*side_length(i)/2;x_c(i) + xL*tv(i)*side_length(i)/2];
                wGauss = [flipud(wL)*side_length(i)/2;wL*side_length(i)/2];
                A(i,j) = sum(wGauss.*log(abs(xGauss-x_c(i))));
            else
                
                xGauss = x(j) + tv_abs(j) * (xG+1)/2;
                A(i,j) = side_length(j)/2 * wG*log(abs(xGauss-x_c(i)));
            end
        end
    end
    
    f = @(x,y) exp(x).*cos(y);
    
    b = f(real(x_c),imag(x_c));
    
    sol = A\b;
    
    count=2;
    for i=1:nEval
        if thetaEval(i) <= theta(count)
            %count stays same
        else
            count=count+1;
        end
        sigma(i,i_conv) = sol(count-1);
    end
end

figure
plot(thetaEval(1:nEval),sigma)

% %%
% % Now let's evaluate the error for each n, using the solution obtained via
% % the finest discretization as the reference solution.
% for i_conv=1:length(n_conv)-1
%     rel_err(i_conv) = norm(sigma(:,i_conv)-sigma(:,end))/norm(sigma(:,end));
% end
% 
% % Plot this on a loglog plot
% figure
% loglog(n_conv(1:end-1),rel_err)
% % Plot alongside 1/n curve to see that scheme converges as 1/n
% hold on
% loglog(n_conv(1:end-1),n_conv(1:end-1).^(-1))


%% Galerkin method
sigma_G = zeros(nEval,length(n_conv));
for i_conv = 1:length(n_conv)
    n = n_conv(i_conv);
    
    theta = linspace(0,2*pi,n+1);
    x = (rad*exp(1i*theta)).';
    
    side_length = abs(diff(x)); % lengths of each artificial side
    
    tv_abs = diff(x);   % absolute tangent vectors (not normalized)
    tv = tv_abs./side_length; % tangent vectors (note normalization)
    nv = imag(tv) - 1i*real(tv); % normal vectors to sides
    
    [xL,wL] = quad_gengauss_log(5); % quadrature points and weight for sing int.
    
    [xG,wG] = gauss(nG);
    A_G = zeros(n,n);
    b_G = zeros(n,1);
    for i=1:n
        [xG1,wG1] = gauss(nG+1);
        xGauss1 = x(i) + tv_abs(i) * (xG1+1)/2;
        
        for j=1:n
            
            if i==j
                [xL,yL,rL,wL] = DuffyLog(0,side_length(j), 2);
                A_G(i,j) = sum(wL.*log(abs(xL-yL)));
            else               
                xGauss = x(j) + tv_abs(j) * (xG+1)/2;
                [xx,yy]=meshgrid(xGauss1,xGauss);
                A_G(i,j) = side_length(i)/2*side_length(j)/2 * wG*log(abs(xx-yy))*wG1';
            end
        end
        
        b_G(i) = side_length(i)/2 * wG1*(f(real(xGauss1),imag(xGauss1)));
   
        
    end
    
    sol_G = A_G\b_G;
   
    count=2;
    for i=1:nEval
        if thetaEval(i) <= theta(count)
            %count stays same
        else
            count=count+1;
        end
        sigma_G(i,i_conv) = sol_G(count-1);
    end
end

% for i_conv=1:length(n_conv)-1
%     rel_err_G(i_conv) = norm(sigma_G(:,i_conv)-sigma_G(:,end))/norm(sigma_G(:,end));
% end
% 
% % Plot this on a loglog plot
% figure
% loglog(n_conv(1:end-1),rel_err_G)
% % Plot alongside 1/n curve to see that scheme converges as 1/n
% hold on
% loglog(n_conv(1:end-1),n_conv(1:end-1).^(-1))

