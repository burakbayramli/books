function pest_conc_stitch(Nprocs)

%load all of the conc data files
start=1;
finish=0;
for i=1:Nprocs
	filename=sprintf('conc_data_proc%04d%s',i-1,'.dat');
    fid=fopen(filename);
	temp_conc=fscanf(fid, "%f", Inf);
    fclose(fid);
	finish = finish + size(temp_conc,1);
	for j=start:finish
	  conc(j)=temp_conc(j+1-start);
	end
	start = start + size(temp_conc,1);
end
%conc
%load all of the data-ordering files
start=1;
finish=0;
for i=1:Nprocs
	filename=sprintf('conc_order_proc%04d%s',i-1,'.dat');
    fid=fopen(filename);
	temp_order=fscanf(fid, "%d", Inf);
    fclose(fid);
	finish = finish + size(temp_order,1);
	for j=start:finish
	  order(j)=temp_order(j+1-start);
	end
	start = start + size(temp_order,1);
end

%allocate final conc array
final_conc = zeros(size(order));
final_order= zeros(size(order));

%fill array conc with ordered results

for i=1:size(order,2)
	final_conc(order(i)+1) = conc(i);
	final_order(order(i)+1) = order(i);
end


%replace processor 0 version with final version (for compatability with serial mode)
filename=sprintf('conc_data_proc%04d%s',0,'.dat');
fid=fopen(filename,"w+");

for j=1:size(order,2)
	fprintf(fid, "%20.17f\n", final_conc(j));
end
fclose(fid);

filename=sprintf('conc_order_proc%04d%s',0,'.dat');
fid=fopen(filename,"w+");

for j=1:size(order,2)
	fprintf(fid, "%d\n", final_order(j));
end
fclose(fid);


endfunction
