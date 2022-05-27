g++ main.cc -std=c++11 -o Godunov.out
#g++ myroe.cc -std=c++11 -o Godunov.out
./Godunov.out < inp.dat
python3 animate.py
