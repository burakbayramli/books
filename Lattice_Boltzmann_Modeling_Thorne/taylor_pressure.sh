pushd src
cp flags_taylor_pressure.h flags.h
popd
make
./lb2d_prime ./in/params_taylor_pressure.in
