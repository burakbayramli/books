pushd src
cp flags_taylor_pressure_y_infile.h flags.h
popd
make
./lb2d_prime ./in/params_taylor_pressure_y_infile.in
