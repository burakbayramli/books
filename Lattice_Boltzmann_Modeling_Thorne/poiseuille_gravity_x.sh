pushd in
cp 17x17_x.bmp 17x17.bmp
popd
pushd src
cp flags_poiseuille_gravity_x.h flags.h
popd
make
./lb2d_prime ./in/params_poiseuille_gravity_x.in
