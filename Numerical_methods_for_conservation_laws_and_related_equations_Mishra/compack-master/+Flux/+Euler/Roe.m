classdef Roe < Flux.NumFlux
	%ROE   Roe flux for the Euler equations
	
	properties
		name = 'Roe'
	end
	
	methods
		function ret = F(o, d, Ul, Ur, varargin)
			if d == 1
				ret = o.Fx(Ul, Ur);
			else
				ret = o.Fy(Ul, Ur);
			end
		end		
		
		
		function [u, v, H] = roeAvg(o, Ul, Ur)
			[rhoL, m1L, m2L, El] = o.model.getConservedVars(Ul);
			[rhoR, m1R, m2R, Er] = o.model.getConservedVars(Ur);
			rhoLSq = sqrt(rhoL);
			rhoRSq = sqrt(rhoR);
			ul = m1L ./ rhoL;
			ur = m1R ./ rhoR;
			vl = m2L ./ rhoL;
			vr = m2R ./ rhoR;
			pl = o.model.pressure(Ul);
			pr = o.model.pressure(Ur);
			Hl = (El + pl) ./ rhoL;
			Hr = (Er + pr) ./ rhoR;
			
			% Compute Roe average
			w1 = (rhoLSq + rhoRSq);
			u = (rhoLSq.*ul + rhoRSq.*ur) ./ w1;
			v = (rhoLSq.*vl + rhoRSq.*vr) ./ w1;
			H = (rhoLSq.*Hl + rhoRSq.*Hr) ./ w1;
		end
	end
	
	
	methods(Access=private)
		function ret = Fx(o, Ul, Ur)
			gamma = o.model.gamma;
			[u, v, H] = o.roeAvg(Ul, Ur);
			c = sqrt((gamma-1) * (H - 0.5*u.*u));
			s = u.*u + v.*v;	% Square speed			
			% Eigenvalues
			l1 = u-c;
			l4 = u+c;
			
			% Advection part
			flux = 0.5*(o.f(Ul,1)+o.f(Ur,1));
			% Cell interface jump
			jmp = Ur-Ul;
			% Diffusion operator
			diff = zeros(size(Ul));
			k = s-2*H;
			for i = 1:numel(u)
				% Roe matrix
				R = [1, 1, 0, 1;
					l1(i), u(i), 0, l4(i);
					v(i), v(i), 1, v(i);
					H(i)-u(i)*c(i), 0.5*s(i), v(i), H(i)+u(i)*c(i)];
				RInv = [(u(i)*k(i)-c(i)*s(i))/(2*c(i)), u(i)-k(i)/(2*c(i)), v(i), -1;
					2*(s(i)-H(i)), -2*u(i), -2*v(i), 2;
					-v(i)*k(i), 0, k(i), 0;
					-(u(i)*k(i) + c(i)*s(i))/(2*c(i)), u(i)+k(i)/(2*c(i)), v(i), -1] / k(i);
				% Eigenvalues
				Gamma = diag(abs([l1(i), u(i), u(i), l4(i)]));
				% Set the Roe diffusion operator
				diff(:, i) =  0.5*R*Gamma*RInv*jmp(:,i);
% 				diff(:, i) =  0.5*R*Gamma*(R\jmp(:,i));
			end
			ret = flux - diff;
		end
		
		
		function ret = Fy(o, Ul, Ur)
			gamma = o.model.gamma;
			[u, v, H] = o.roeAvg(Ul, Ur);
			c = sqrt((gamma-1) * (H - 0.5*v.*v));
			s = u.*u + v.*v;	% Square speed			
			% Eigenvalues
			l1 = v-c;
			l4 = v+c;
			
			% Advection part
			flux = 0.5*(o.f(Ul,2)+o.f(Ur,2));
			% Cell interface jump
			jmp = Ur-Ul;
			% Diffusion operator
			diff = zeros(size(Ul));
			k = s-2*H;
			for i = 1:numel(u)
				% Roe matrix
				R = [1, 1, 0, 1;
					u(i), u(i), 1, u(i);
					l1(i), v(i), 0, l4(i);
					H(i)-v(i)*c(i), 0.5*s(i), u(i), H(i)+v(i)*c(i)];
				RInv = [(v(i)*k(i)-c(i)*s(i))/(2*c(i)), u(i), v(i)-k(i)/(2*c(i)), -1;
					2*(s(i)-H(i)), -2*u(i), -2*v(i), 2;
					-u(i)*k(i), k(i), 0, 0;
					-(v(i)*k(i)+c(i)*s(i))/(2*c(i)), u(i), k(i)/(2*c(i))+v(i), -1] / k(i);
				% Eigenvalues
				Gamma = diag(abs([l1(i), v(i), v(i), l4(i)]));
				% Set the Roe diffusion operator
				diff(:, i) =  0.5*R*Gamma*RInv*jmp(:,i);
% 				diff(:, i) =  0.5*R*Gamma*(R\jmp(:,i));
			end
			ret = flux - diff;
		end
	end
end