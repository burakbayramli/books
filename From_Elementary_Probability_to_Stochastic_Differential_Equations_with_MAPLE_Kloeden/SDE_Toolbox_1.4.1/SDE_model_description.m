function SDE_model_description()

% Gives the description of the chosen SDE model
%
% usage: SDE_model_description; 

% Copyright (C) 2007, Umberto Picchini  
% umberto.picchini@biomatematica.it
% http://www.biomatematica.it/Pages/Picchini.html
%
% This program is free software; you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation; either version 2 of the License, or
% (at your option) any later version.
% 
% This program is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
% 
% You should have received a copy of the GNU General Public License
% along with this program.  If not, see <http://www.gnu.org/licenses/>.

fprintf('\n\n:::::::::::::::::::::::::::::::::::::::::::::::::::   MODEL LIBRARY   ::::::::::::::::::::::::::::::::::::::::::::::::::::::::');
fprintf('\n\nMODEL 1');
fprintf('\n\nM1a: (Ito SDE)     dXt = -a * Xt * dt + sigma * dWt,   X(0) = X0');
fprintf('\n\n or equivalently');
fprintf('\n\nM1b: (Stratonovich SDE)     dXt = -a * Xt * dt + sigma o dWt,   X(0) = X0');
fprintf('\n------------------------------------------------------------------------------------------------------------------------------')
fprintf('\n\nMODEL 2');
fprintf('\n\nM2a: (Ito SDE)     dXt = (a * Xt + b) * dt + sigma * dWt,   X(0) = X0');
fprintf('\n\n or equivalently');
fprintf('\n\nM2b: (Stratonovich SDE)     dXt = (a * Xt + b) * dt + sigma o dWt,   X(0) = X0');
fprintf('\n------------------------------------------------------------------------------------------------------------------------------')
fprintf('\n\nMODEL 3');
fprintf('\n\nM3a: (Ito SDE)     dXt = (a - sigma^2/2) * dt + sigma * dWt,   X(0) = X0');
fprintf('\n\n or equivalently');
fprintf('\n\nM3b: (Stratonovich SDE)     dXt = (a - sigma^2/2) * dt + sigma o dWt,   X(0) = X0');
fprintf('\n------------------------------------------------------------------------------------------------------------------------------')
fprintf('\n\nMODEL 4');
fprintf('\n\nM4a: (Ito SDE)     dXt = a * Xt * dt + b * Xt * dWt,   X(0) = X0');
fprintf('\n\n or equivalently');
fprintf('\n\nM4b: (Stratonovich SDE)     dXt = (a * Xt - 1/2 * b^2 * Xt) * dt + b * Xt o dWt,    X(0) = X0');
fprintf('\n------------------------------------------------------------------------------------------------------------------------------')
fprintf('\n\nMODEL 5');
fprintf('\n\nM5a: (Ito SDE)     dXt = (a * Xt + c) * dt + (b * Xt + d) * dWt,   X(0) = X0');
fprintf('\n\n or equivalently');
fprintf('\n\nM5b: (Stratonovich SDE)     dXt = [(a - 1/2 * b) * Xt + c - 1/2 * b * d] * dt + (b * Xt + d) o dWt,    X(0) = X0');
fprintf('\n------------------------------------------------------------------------------------------------------------------------------')
fprintf('\n\nMODEL 6');
fprintf('\n\nM6a: (Ito SDE)     dXt = [1/2 * a * (a - 1) * Xt ^ (1-2/a)] * dt + a * Xt ^ (1-1/a) * dWt,   X(0) = X0');
fprintf('\n\n or equivalently');
fprintf('\n\nM6b: (Stratonovich SDE)     dXt = [a * Xt ^ (1-1/a)] o dWt,    X(0) = X0');
fprintf('\n------------------------------------------------------------------------------------------------------------------------------')
fprintf('\n\nMODEL 7');
fprintf('\n\nM7a: (Ito SDE)     dXt = [-1/2 * a^2 * Xt] * dt + a * sqrt(1 - Xt^2) * dWt,   X(0) = X0');
fprintf('\n\n or equivalently');
fprintf('\n\nM7b: (Stratonovich SDE)     dXt = a * sqrt(1 - Xt^2) o dWt,    X(0) = X0');
fprintf('\n------------------------------------------------------------------------------------------------------------------------------')
fprintf('\n\nMODEL 8');
fprintf('\n\nM8a: (Ito SDE)     dXt = [a^2 * Xt * (1 + Xt^2)] * dt + a * (1 + Xt^2) * dWt,   X(0) = X0');
fprintf('\n\n or equivalently');
fprintf('\n\nM8b: (Stratonovich SDE)     dXt = a * (1 + Xt^2) o dWt,    X(0) = X0');
fprintf('\n------------------------------------------------------------------------------------------------------------------------------')
fprintf('\n\nMODEL 9 (Two-dimensional Ornstein-Uhlenbeck process)');
fprintf('\n\nM9a: (Ito SDE)     dXt1 = [beta11 * alpha1 + beta12 * alpha2 - beta11 * Xt1 - beta12 * Xt2] * dt + sigma1 * dWt1,   X1(0) = X01');
fprintf('\n                   dXt2 = [beta21 * alpha1 + beta22 * alpha2 - beta21 * Xt1 - beta22 * Xt2] * dt + sigma2 * dWt2,   X2(0) = X02');
fprintf('\n\n or equivalently');
fprintf('\n\nM9b: (Stratonovich SDE)     dXt1 = [beta11 * alpha1 + beta12 * alpha2 - beta11 * Xt1 - beta12 * Xt2] * dt + sigma1 o dWt1,   X1(0) = X01');
fprintf('\n                            dXt2 = [beta21 * alpha1 + beta22 * alpha2 - beta21 * Xt1 - beta22 * Xt2] * dt + sigma2 o dWt2,   X2(0) = X02');
fprintf('\n------------------------------------------------------------------------------------------------------------------------------')
fprintf('\n\nMODEL 10 (Cox-Ingersoll-Ross model)');
fprintf('\n\nM10a: (Ito SDE)     dXt = a * (b - Xt) * dt + sigma * sqrt(Xt) * dWt,   X(0) = X0');
fprintf('\n\n or equivalently');
fprintf('\n\nM10b: (Stratonovich SDE)     dXt = [a * (b - Xt) -1/4 * sigma^2] * dt + sigma * sqrt(Xt) o dWt,   X(0) = X0');
fprintf('\n------------------------------------------------------------------------------------------------------------------------------')

fprintf('\n\n::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::');