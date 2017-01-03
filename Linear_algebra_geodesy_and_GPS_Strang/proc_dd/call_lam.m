% M-file for call of the LAMBDA routines for integer estimation of the
% GPS double difference ambiguities

% disall squared norms \hat{a}-\check{a} (of candidates inside ellipsoid)
% n	   dimension of integer estimation problem; number of ambiguities
% Q	   variance covariance matrix (symmetric)
%	      Only the lower triangular part is stored (columnwise) and
%	      accessed. A 2-dimensional array is used, to avoid the
%	      necessity of a dedicated storage scheme
% a	   vector with real valued estimates \hat{a} (float solution);
%	      the integer estimates are computed
% truth  true integer values for the ambiguities

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Recoded into MATLAB by Kai Borre 12-04-96

delete lambda.log
disp(' 1. 3D-example of sections 3.8 and 4.12');
disp(' 2. Example read from file: covar.dat');
iopt = input(' Enter your choice:  ');
if (iopt <= 0) | (iopt > 2), break, end

if (iopt == 1)
   Q = [6.290 5.978 0.544;
        5.978 6.292 2.340;
        0.544 2.340 6.288];
   n = size(Q,1);
   a = [5.45; 3.10; 2.97];
   truth = [5; 3; 4];
   fidlog = fopen('lambda.log','wt');
else
   fidlog = fopen('lambda.log','wt');
   fiddat = fopen('covar.dat','rt');
   line = fgetl(fiddat);
   fprintf(fidlog,'  %50s\n', str2num(line));
   line = fgetl(fiddat);
   fprintf(fidlog,'  %50s\n', str2num(line));
   
   % reading \hat{a}
   line = fgetl(fiddat);
   [i,line] = strtok(line);
   n = str2num(i);
   j = floor((n-2)/3);
   for t = 1:2
      [a0,line] = strtok(line);
      a(t,1) = str2num(a0);
   end
   
   for i = 1:j
      line = fgetl(fiddat);
      for t = 3*i:3*i+2
         [a0,line] = strtok(line);
         a(t,1) = str2num(a0);
      end
   end
   
   line = fgetl(fiddat);
   for t = 12
      [a0,line] = strtok(line);
      a(t,1) = str2num(a0);
   end
   
   % Reading covariance matrix
   line = fgetl(fiddat);
   [i,line] = strtok(line);
   ii = str2num(i);
   j = floor((ii-2)/3);
   Q = zeros(ii,1);
   
   for t = 1:2
      [a0,line] = strtok(line);
      Q(t) = str2num(a0);
   end
   
   for i = 1:j
      line = fgetl(fiddat);
      for t = 3*i:3*i+2
         [a0,line] = strtok(line);
         Q(t,1) = str2num(a0);
      end
   end
   
   line = fgetl(fiddat);
   for t = 144
      [a0,line] = strtok(line);
      Q(t,1) = str2num(a0);
   end
   Q = reshape(Q,12,12);
   
   % Reading truth
   line = fgetl(fiddat);
   [i,line] = strtok(line);
   ii = str2num(i);
   truth = zeros(ii,1);
   j = floor((ii-2)/6);
   
   for t = 1:5
      [a0,line] = strtok(line);
      truth(t,1) = str2num(a0);
   end
   
   for i = 1:j
      line = fgetl(fiddat);
      for t = 6:11
         [a0,line] = strtok(line);
         truth(t,1) = str2num(a0);
      end
   end
   
   line = fgetl(fiddat);
   for t = 12
      [a0,line] = strtok(line);
      truth(t,1) = str2num(a0);
   end
   fclose(fiddat);
end

[a, disall] = lambda(fidlog, n, Q, a);
fprintf(fidlog,'\n minimum squared norm: %14.4f\n', disall(1));
fprintf(fidlog,'   idem, second best : %14.4f\n', disall(2));
v = a-truth;
fprintf(fidlog,'\n truth \n');
for i = 1:n
   fprintf(fidlog,'%12.0f\n', truth(i));
end
dout(fidlog,' a_check-truth', v, 1, n)
fprintf(fidlog,'\n');
fclose(fidlog);
%%%%%%%%%%%% end call_lam.m  %%%%%%%%%%%%%%%%%%%%%%%%%%%
