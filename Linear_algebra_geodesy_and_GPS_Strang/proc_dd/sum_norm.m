function sum_norm(time1, sats1, obs1, ant_delta1, ...
            			 time2, sats2, obs2, ant_delta2, Lambda)
%SUM_NORM  Sums normals for double differenced GPS data

%Kai Borre and C.C. Goad 03-31-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/25 $

% Units are either seconds, meters, or cycles
global Obs_types1 Obs_types2
global Eph ambi ty
global ATA ATb
global X_ista1 X_ista2

% Assume phase has a standard deviation of 10 mm
std_dev = 0.010;
c1 = fobs_typ(Obs_types1,'L1'); % or L1 or L2
c2 = fobs_typ(Obs_types2,'L1'); % or L1 or L2
c3 = fobs_typ(Obs_types1,'P1');
c4 = fobs_typ(Obs_types2,'P1');
num_of_matches = 0;

% Find common satellites
for i = 1:size(sats1,1)
   for j = 1:size(sats2,1)
      % In the following line you may specify PRNs to be omitted
      if sats1(i) == 31, break, end
      if sats1(i) == sats2(j)
	 num_of_matches = num_of_matches+1;
	 match_prns(num_of_matches) = sats1(i);
	 target_obs(num_of_matches) = obs2(j,c2);
	 base_obs(num_of_matches) = obs1(i,c1);
	 pseudo_1(num_of_matches) = obs1(i,c3);
	 pseudo_2(num_of_matches) = obs2(j,c4);
	 location_1(num_of_matches) = i;
	 location_2(num_of_matches) = j;
      end
   end
end
if num_of_matches < 2, break, end;

for i = 1:num_of_matches
   col_Eph(i) = find_eph(Eph,match_prns(i),time1);
end
double = zeros(num_of_matches,1);

for j = 1:num_of_matches
   k = col_Eph(j);
   loc11 = location_1(j);  %  Sat j, sta 1
   % Distance satellite to mark
   [rhoj1, X_Satj1] = get_rho(time1, obs1(loc11,c3), Eph(:,k), X_ista1);
   % We store satellite positions for forming partials
   X_Sat1(:,j) = X_Satj1;
   rho1(j) = rhoj1;
   loc12 = location_2(j);  %  Sat j, sta 2
   [rhoj2, X_Satj2] = get_rho(time2, obs2(loc12,c4), Eph(:,k), X_ista2);
   X_Sat2(:,j) = X_Satj2;
   rho2(j) = rhoj2;
   [phi1,lambda1,h1] = togeod(6378137,298.257223563, ...
				      X_ista1(1),X_ista1(2),X_ista1(3));
   % Sequence of offsets in header is H/E/N
   ant_H = ant_delta1(1);
   ant_E = ant_delta1(2);
   ant_N = ant_delta1(3);
   % Caution: order needed is E, N, U(H)
   [dx1(1),dx1(2),dx1(3)] = enu2xyz(phi1, lambda1, ant_E, ant_N, ant_H);
   distance_phase_center1 = norm(X_Satj1 - (X_ista1 + dx1'));
   delta_rhoj1 = rhoj1 - distance_phase_center1;
   obsj1 = base_obs(j);
   corrected_obsj1 = obsj1 + delta_rhoj1;
   [phi2,lambda2,h2] = togeod(6378137,298.257223563, ...
				      X_ista2(1),X_ista2(2),X_ista2(3));
   ant_H = ant_delta2(1);
   ant_E = ant_delta2(2);
   ant_N = ant_delta2(3);
   [dx2(1),dx2(2),dx2(3)] = enu2xyz(phi2,lambda2, ant_E, ant_N, ant_H);
   distance_phase_center2 = norm(X_Satj2- (X_ista2 + dx2'));
   delta_rhoj2 = rhoj2 - distance_phase_center2;
   obsj2 = target_obs(j);
   corrected_obsj2 = obsj2 + delta_rhoj2;
   corr_single_diff(j) = corrected_obsj2 - corrected_obsj1;
end % j loop

bias_1 = locate(match_prns(1));
par(4) =	1;
par(5) = -1;
A = [];
b = [];

for i = 2:num_of_matches
   corr_double_diff = corr_single_diff(i) - corr_single_diff(1);
   double(i,1) = corr_double_diff;
   calc_double_diff = (rho1(i)-rho2(i)-rho1(1)+rho2(1))/Lambda;
   omc = corr_double_diff + calc_double_diff;
   if ambi(i) == 0
      ambi(i) = omc; % Record of preliminary ambiguities
   end
   par(1) = (X_Sat2(1,i)-X_ista2(1))/rho2(i) ...
      - (X_Sat2(1,1)-X_ista2(1))/rho2(1);
   par(2) = (X_Sat2(2,i)-X_ista2(2))/rho2(i) ...
			      - (X_Sat2(2,1)-X_ista2(2))/rho2(1);
   par(3) = (X_Sat2(3,i)-X_ista2(3))/rho2(i) ...
			       - (X_Sat2(3,1)-X_ista2(3))/rho2(1);
   par(1) = par(1)/Lambda;
   par(2) = par(2)/Lambda;
   par(3) = par(3)/Lambda;
   fprintf('sv1:%3.0f and sv2:%3.0f    omc-omc_0 [cycles]:%7.3f\n', ....
			     match_prns(1), match_prns(i),omc -ambi(i))
   bias_2 = locate(match_prns(i));
   Ai = zeros(1,ty);
   Ai(1,1) = par(1);
   Ai(1,2) = par(2);
   Ai(1,3) = par(3);
   Ai(1,bias_1) = -1;
   Ai(1,bias_2) = 1;
   A = [A; Ai];
   b = [b; omc];
end % i loop

%  Decorrelation and weight normalization of correlated
%  observations due to differencing
s = num_of_matches;
D = std_dev*[ones(s-1,1) -eye(s-1) -ones(s-1,1) eye(s-1)];
C = inv(D*D');
U = chol(C);
Linv = inv(U');
b =  Linv*b;
A =  Linv*A;
ATA_to_add = A'*C*A;
ATb_to_add = A'*C*b;
accum0(ATA_to_add, ATb_to_add)
fprintf('\n')
%%%%%%%%%%%%% end sum_norm.m %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
