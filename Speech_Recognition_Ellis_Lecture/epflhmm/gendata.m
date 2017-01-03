% GENDATA Generation of simulation data for HMM lab
%

% Specification of simulated vowels statistics

Pa = 0.25;
mu_a = [730 1090]';
std_a = [35  20
         20  230];
var_a = std_a * std_a;

Pe = 0.3;
mu_e = [530 1840]';
std_e = [120  25
         25  190];
var_e = std_e * std_e;

Pi = 0.25;
mu_i = [270 2290]';
std_i = [50  5
         5  190];
var_i = std_i * std_i;

Po = 0.15;
mu_o = [570 840]';
std_o = [40  20
         20 140];
var_o = std_o * std_o;

Py = 0.05;
mu_y = [440 1020]';
std_y = [80 40
         40 130];
var_y = std_y * std_y;

% Specification of some HMMs for classification

% 1: ergodic /aiy/, "unstable"
hmm1.means = {[],mu_a,mu_i,mu_y,[]};
hmm1.vars  = {[],var_a,var_i,var_y,[]};
hmm1.trans = [ 0.0 1.0 0.0 0.0 0.0
               0.0 0.4 0.3 0.3 0.0
	       0.0 0.3 0.4 0.3 0.0
	       0.0 0.3 0.3 0.3 0.1
	       0.0 0.0 0.0 0.0 1.0 ];

% 2: ergodic /aiy/, "stable"
hmm2.means = {[],mu_a,mu_i,mu_y,[]};
hmm2.vars  = {[],var_a,var_i,var_y,[]};
hmm2.trans = [ 0.0 1.0   0.0   0.0   0.0
               0.0 0.95  0.025 0.025 0.0
	       0.0 0.025 0.95  0.025 0.0
	       0.0 0.02  0.02  0.95  0.01
	       0.0 0.0   0.0   0.0   1.0 ];

% 3: left-right /aiy/, unstable
hmm3.means = {[],mu_a,mu_i,mu_y,[]};
hmm3.vars  = {[],var_a,var_i,var_y,[]};
hmm3.trans = [ 0.0 1.0 0.0 0.0 0.0
               0.0 0.5 0.5 0.0 0.0
	       0.0 0.0 0.5 0.5 0.0
	       0.0 0.0 0.0 0.5 0.5
	       0.0 0.0 0.0 0.0 1.0 ];

% 4: left-right /aiy/, stable
hmm4.means = {[],mu_a,mu_i,mu_y,[]};
hmm4.vars  = {[],var_a,var_i,var_y,[]};
hmm4.trans = [ 0.0 1.0  0.0  0.0  0.0
               0.0 0.95 0.05 0.0  0.0
	       0.0 0.0  0.95 0.05 0.0
	       0.0 0.0  0.0  0.95 0.05
	       0.0 0.0  0.0  0.0  1.0 ];

% 5: left-right /yia/, stable
hmm5.means = {[],mu_y,mu_i,mu_a,[]};
hmm5.vars  = {[],var_y,var_i,var_a,[]};
hmm5.trans = [ 0.0 1.0  0.0  0.0  0.0
               0.0 0.95 0.05 0.0  0.0
	       0.0 0.0  0.95 0.05 0.0
	       0.0 0.0  0.0  0.95 0.05
	       0.0 0.0  0.0  0.0  1.0 ];

% 6: left-right /aie/, stable
hmm6.means = {[],mu_a,mu_i,mu_e,[]};
hmm6.vars  = {[],var_a,var_i,var_e,[]};
hmm6.trans = [ 0.0 1.0  0.0  0.0  0.0
               0.0 0.95 0.05 0.0  0.0
	       0.0 0.0  0.95 0.05 0.0
	       0.0 0.0  0.0  0.95 0.05
	       0.0 0.0  0.0  0.0  1.0 ];

% 7: left-right /aiy/, stable w/ unequal stay probas
hmm7.means = {[],mu_a,mu_i,mu_y,[]};
hmm7.vars  = {[],var_a,var_i,var_y,[]};
hmm7.trans = [ 0.0 1.0  0.0  0.0  0.0
               0.0 0.7  0.3  0.0  0.0
	       0.0 0.0  0.95 0.05 0.0
	       0.0 0.0  0.0  0.7  0.3
	       0.0 0.0  0.0  0.0  1.0 ];


% Save the relevant variables
save data ...
    mu_a mu_e mu_i mu_o mu_y var_a var_e var_i var_o var_y ...
    hmm1 hmm2 hmm3 hmm4 hmm5 hmm6 hmm7 ...
    -append;
