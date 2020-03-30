profile -memory on
for N=[40 400 4000]
    N
    for d=[1 2 3 10 20 40] 
        d
        dsites = CreatePoints(N,d,'h');
        tic; DistanceMatrixBook(dsites,dsites); tend=toc;
        fprintf('Time for DistanceMatrixBook:   %f seconds.\n',tend)
        tic; DistanceMatrixRepmat(dsites,dsites); tend=toc;
        fprintf('Time for DistanceMatrixRepmat:       %f seconds.\n',tend)
        tic; DistanceMatrixA(dsites,dsites); tend=toc;
        fprintf('Time for DistanceMatrixA:      %f seconds.\n',tend)
        tic; DistanceMatrix(dsites,dsites); tend=toc;
        fprintf('Time for DistanceMatrix:      %f seconds.\n',tend)
    end
end
profile off
profile viewer