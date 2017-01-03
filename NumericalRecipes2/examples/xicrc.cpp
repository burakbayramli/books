#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

// Driver for routine icrc

inline unsigned char lobyte(const unsigned short x)
{return (unsigned char)(x & 0xff);}

inline unsigned char hibyte(const unsigned short x)
{return (unsigned char) ((x >> 8) & 0xff);}

int main(void)
{
        string lin;
        unsigned short i1,i2;
        unsigned long n;

        for (;;) {
          cout << "Enter string: " << endl;
          getline(cin,lin);
          n=lin.length();
          if (n == 0) break;
          cout << lin << endl;
          i1=NR::icrc(0,lin,0,1);
          lin += hibyte(i1);
          lin += lobyte(i1);
          i2=NR::icrc(i1,lin,0,1);
          cout << "    XMODEM: String CRC, Packet CRC=    0x" << hex << i1;
          cout << "   0x" << i2 << endl;
          lin.erase(n,2);
          i1=NR::icrc(i2,lin,0xff,-1);
          lin += ~lobyte(i1);
          lin += ~hibyte(i1);
          i2=NR::icrc(i1,lin,0xff,-1);
          cout << "      X.25: String CRC, Packet CRC=    0x" << hex << i1;
          cout << "   0x" << hex << i2 << endl;
          lin.erase(n,2);
          i1=NR::icrc(i2,lin,0,-1);
          lin += lobyte(i1);
          lin += hibyte(i1);
          i2=NR::icrc(i1,lin,0,-1);
          cout << " CRC-CCITT: String CRC, Packet CRC=    0x" << hex << i1;
          cout << "   0x" << hex << i2 << endl;
        }
        cout << "Normal completion" << endl;
        return 0;
}
