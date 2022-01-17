classdef Roe < Flux.NumFlux
	%ROE   Roe flux for the shallow water equations
	
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
	end
	
	
	methods (Access=private)
		function ret = Fx(o, Ul, Ur)
			grav = o.model.grav;
			
			hl = Ul(1,:,:); hr = Ur(1,:,:);
			ul = Ul(2,:,:)./Ul(1,:,:); ur = Ur(2,:,:)./Ur(1,:,:);
			vl = Ul(3,:,:)./Ul(1,:,:); vr = Ur(3,:,:)./Ur(1,:,:);
			hlSqrt = sqrt(hl);
			hrSqrt = sqrt(hr);
			hRoe = (hl+hr)/2;
			uRoe = (hlSqrt.*ul + hrSqrt.*ur) ./ (hlSqrt+hrSqrt);
			vRoe = (hlSqrt.*vl + hrSqrt.*vr) ./ (hlSqrt+hrSqrt);
			cRoe = sqrt(grav*hRoe);
			% Eigenvalues
			l1 = uRoe-cRoe;
			l2 = uRoe;
			l3 = uRoe+cRoe;
			
			% Advection part
			flux = 0.5*(o.f(Ul, 1)+o.f(Ur, 1));
			% Cell interface jump
			jmp = Ur-Ul;
			% Diffusion operator
			diff = zeros(size(Ul));
			for i = 1:numel(hl)
				% Roe matrix
				R = [1, 0, 1; ...
					l1(i), 0, l3(i); ...
					vRoe(i), hRoe(i), vRoe(i)];
				RInv = [l3(i), -1, 0; ...
					-2*vRoe(i), 0, 2; ...
					-l1(i), 1, 0] / (2*cRoe(i));
				% Eigenvalues
				Gamma = diag(abs([l1(i), l2(i), l3(i)]));
				% Set the Roe diffusion operator
				diff(:, i) =  0.5*R*Gamma*RInv*jmp(:,i);
			end
			ret = flux - diff;
		end
		
		
	
		function ret = Fy(o, Ul, Ur)
			grav = o.model.grav;
			
			hl = Ul(1,:,:); hr = Ur(1,:,:);
			ul = Ul(2,:,:)./Ul(1,:,:); ur = Ur(2,:,:)./Ur(1,:,:);
			vl = Ul(3,:,:)./Ul(1,:,:); vr = Ur(3,:,:)./Ur(1,:,:);
			hlSqrt = sqrt(hl);
			hrSqrt = sqrt(hr);
			hRoe = (hl+hr)/2;
			uRoe = (hlSqrt.*ul + hrSqrt.*ur) ./ (hlSqrt+hrSqrt);
			vRoe = (hlSqrt.*vl + hrSqrt.*vr) ./ (hlSqrt+hrSqrt);
			cRoe = sqrt(grav*hRoe);
			% Eigenvalues
			l1 = vRoe-cRoe;
			l2 = vRoe;
			l3 = vRoe+cRoe;
			
			% Advection part
			flux = 0.5*(o.f(Ul, 2)+o.f(Ur, 2));
			% Cell interface jump
			jmp = Ur-Ul;
			% Diffusion operator
			diff = zeros(size(Ul));
			for i = 1:numel(hl)
				% Roe matrix
				R = [1, 0, 1;...
					uRoe(i), cRoe(i), uRoe(i);...
					l1(i), 0, l3(i)];
				RInv = [l3(i), 0, -1;...
					-2*uRoe(i), 2, 0;
					-l1(i), 0, 1] / (2*cRoe(i));
				% Eigenvalues
				Gamma = diag(abs([l1(i), l2(i), l3(i)]));
				% Set the Roe diffusion operator
				diff(:, i) =  0.5*R*Gamma*RInv*jmp(:,i);
			end
			ret = flux - diff;
		end
	end
end