pushd src
cp flags_poiseuille_pressure_y.h flags.h
popd
make
./lb2d_prime ./in/params_poiseuille_pressure_y.in
