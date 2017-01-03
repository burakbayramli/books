% EASY12   Detailed explanations of a Matlab implementation 
%          of the Lambda method. The notation follows the one
%          introduced in Strang and Borre, pages 495--499

%Kai Borre 30-06-2008
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 2008/06/30  $

fprintf('\n')
echo on
% Test example
hat_I = [5.45;3.1;2.97];
%hat_I =[0; 1];
n = size(hat_I,1);
Q_hat_I = [6.29 5.978 .544; 5.978 6.292 2.34; .544 2.34 6.288];
%Q_hat_I = [53.40 38.40;38.40 28.00];
echo off

% Given float ambiguities
fprintf('\nFloat ambiguities hat_I')
for i = 1:n
    fprintf('\n%12.3f', hat_I(i,1))
end

% Original covariance matrix for hat_I
fprintf('\n\nCovariance matrix for hat_I') 
for i = 1:n
    fprintf('\n')
    for j = 1:n
        fprintf('%12.3f', Q_hat_I(i,j))
    end
end

fprintf('\n\n')
echo on
% Shift of hat_I in order to secure that -1 < hat_I <= +1
echo off

shifts = hat_I-rem(hat_I,1);
fprintf('\n\nInteger shifts of hat_I')
for i = 1:n
    fprintf('\n%12.0f', shifts(i,1))
end

% Remainders of hat_I
hat_I_r = rem(hat_I,1);
fprintf('\n\nRemainders of hat_I')
for i = 1:n
    fprintf('\n%12.3f', hat_I_r(i,1))
end

% L^T*D*L factorization of Q_hat_I
% For consistency we do not use the Matlab ldl function
[L,D] = ldldecom(Q_hat_I);
fprintf('\n\nQ_hat_I is factorized into L^T*D*L')
fprintf('\n\nThe lower triangular L')
for i = 1:n
    fprintf('\n')
    for j = 1:n
        fprintf('%12.3f',L(i,j))
    end
end
fprintf('\n\nThe diagonal matrix D\n')
for i = 1:n
    fprintf('%12.3f',D(i))
end

% Computing the size of the search volume
chi2 = chistart(D,L,hat_I_r,1);
fprintf('\n\nchi^2 = %5.3f\n',chi2)

% Doing the decorrelation
[Q_bar_I,Z,L_t,D_t] = decorrel(Q_hat_I,hat_I_r);
fprintf('\nInteger transformation matrix Z')
for i = 1:n
    fprintf('\n')
    for j = 1:n
        fprintf('%12.0f',Z(i,j))
    end
end

% hat_I transformed: I_s = Z'*hat_I_r
fprintf('\n\nTransformed, shifted ambiguities I_s = Z^T*hat_I_r')
I_s = Z'*hat_I_r;
for i = 1:n
    fprintf('\n%12.3f',I_s(i))
end

% The transformed, decorrelated covariance matrix: Q_bar_I = Z^T*Q_hat_I*Z
fprintf('\n\nThe transformed, decorrelated covariance matrix Q_bar_I = Z^T*Q_hat_I*Z.')
fprintf('\n\nQ_bar_I = Z^T*Q_hat_I*Z')
for i = 1:n
    fprintf('\n')
    for j = 1:n
        fprintf('%12.3f',Q_bar_I(i,j))
    end
end
fprintf('\nNote the deminished off-diagonal terms!')
fprintf('\n\nThe lower triangular L_t used in the search')
for i = 1:n
    fprintf('\n')
    for j = 1:n
        fprintf('%12.3f',L_t(i,j))
    end
end
fprintf('\n\nThe diagonal matrix D_t used in the seach\n')
for i = 1:n
    fprintf('%12.3f',D_t(i))
end

fprintf('\n\n')
echo on
% Determining the size of the search volume for the transformed L_t and D_t
echo off

chi2 = chistart(D_t,L_t,I_s,1); 
fprintf('\n\nchi^2 for the transformed problem  =  %5.3f\n',chi2)

fprintf('\nThe search domain is defined as')
fprintf('\n(I-hat_I_r)^T*(Z^T*Q_hat_I*Z)*(I-hat_I_r) < chi^2,  for I integer')

[bar_I,sqnorm,ierr] = lsearch(I_s,L_t,D_t,chi2,1);
bar_I = (bar_I' * inv(Z))'+shifts;
fprintf('\n\nFixed ambiguities bar_I')
for i = 1:n
    fprintf('\n%12.0f', bar_I(i,1))
end
fprintf('\n\n')
%%%%%%%%%%%%%%%%%%%%%%%%%%% end easy12.m  %%%%%%%%%%%%
