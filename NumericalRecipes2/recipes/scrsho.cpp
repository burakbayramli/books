#include <string>
#include <iostream>
#include <iomanip>
#include "nr.h"
using namespace std;

void NR::scrsho(DP fx(const DP))
{
	const int ISCR=60, JSCR=21;
	const char BLANK=' ', ZERO='-', YY='l', XX='-', FF='x';
	int jz,j,i;
	DP ysml,ybig,x2,x1,x,dyj,dx;
	Vec_DP y(ISCR);
	string scr[JSCR];

	for (;;) {
		cout << endl << "Enter x1 x2 (x1=x2 to stop):" << endl;
		cin >> x1 >> x2;
		if (x1 == x2) break;
		scr[0]=YY;
		for (i=1;i<(ISCR-1);i++)
			scr[0] += XX;
		scr[0] += YY;
		for (j=1;j<(JSCR-1);j++) {
			scr[j]=YY;
			for (i=1;i<(ISCR-1);i++)
				scr[j] += BLANK;
			scr[j] += YY;
		}
		scr[JSCR-1]=scr[0];
		dx=(x2-x1)/(ISCR-1);
		x=x1;
		ysml=ybig=0.0;
		for (i=0;i<ISCR;i++) {
			y[i]=fx(x);
			if (y[i] < ysml) ysml=y[i];
			if (y[i] > ybig) ybig=y[i];
			x += dx;
		}
		if (ybig == ysml) ybig=ysml+1.0;
		dyj=(JSCR-1)/(ybig-ysml);
		jz=int(-ysml*dyj);
		for (i=0;i<ISCR;i++) {
			scr[jz][i]=ZERO;
			j=int((y[i]-ysml)*dyj);
			scr[j][i]=FF;
		}
		cout << fixed << setprecision(3);
		cout << setw(11) << ybig << " " << scr[JSCR-1] << endl;
		for (j=JSCR-2;j>=1;j--)
			cout << "            " << scr[j] << endl;
		cout << setw(11) << ysml << " " << scr[0] << endl;
		cout << setw(19) << x1 << setw(55) << x2;
	}
}
