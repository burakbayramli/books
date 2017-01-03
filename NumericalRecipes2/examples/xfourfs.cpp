#include <string>
#include <iostream>
#include <fstream>
#include <iomanip>
#include <cmath>
#include "nr.h"
using namespace std;

// Driver for routine fourfs

int main(void)
{
        const int NX=8,NY=32,NZ=4,NDAT=2*NX*NY*NZ;
        const string fnames[4]={"frfstmp1","frfstmp2","frfstmp3","frfstmp4"};
        int cc,i,j,k,l,ll,nwrite,idum=(-23),dim_d[3]={NX,NY,NZ};
        DP diff,smax,sum,sum1=0.0,sum2=0.0,tot;
        Vec_INT dim(dim_d,3);
        Vec_DP data1(NDAT),data2(NDAT);
        Vec_FSTREAM_p file(4);
        fstream *flswap;

        tot=DP(NX)*DP(NY)*DP(NZ);
        for (j=0;j<4;j++) {
          file[j]=new fstream(fnames[j].c_str(),
            ios_base::in|ios_base::out|ios_base::binary|ios_base::trunc);
        }
        for (i=0;i<dim[2];i++)
          for (j=0;j<dim[1];j++)
            for (k=0;k<dim[0];k++) {
              l=k+j*dim[0]+i*dim[1]*dim[0];
              l=(l<<1);
              data2[l]=data1[l]=2*NR::ran1(idum)-1;
              l++;
              data2[l]=data1[l]=2*NR::ran1(idum)-1;
            }
        nwrite=NDAT >> 1;
        (*file[0]).write((char *)&data1[0],nwrite*sizeof(DP));
        cc=(*file[0]).tellp()/sizeof(DP);
        if (cc != nwrite) NR::nrerror("write error in xfourfs");
        (*file[1]).write((char *)&data1[nwrite],nwrite*sizeof(DP));
        cc=(*file[1]).tellp()/sizeof(DP);
        if (cc != nwrite) NR::nrerror("write error in xfourfs");
        for(j=0;j<4;j++) (*file[j]).seekp(0);
        cout << "**************** now doing fourfs *********" << endl;
        NR::fourfs(file,dim,1);
        for (j=0;j<4;j++) (*file[j]).seekg(0);
        (*file[2]).read((char *)&data1[0],nwrite*sizeof(DP));
        cc=(*file[2]).tellg()/sizeof(DP);
        if (cc != nwrite) NR::nrerror("read error in xfourfs");
        (*file[3]).read((char *)&data1[nwrite],nwrite*sizeof(DP));
        cc=(*file[3]).tellg()/sizeof(DP);
        if (cc != nwrite) NR::nrerror("read error in xfourfs");
        cout << "**************** now doing fourn *********" << endl;
        NR::fourn(data2,dim,1);
        sum=smax=0.0;
        for (i=0;i<dim[2];i++)
          for (j=0;j<dim[1];j++)
            for (k=0;k<dim[0];k++) {
              l=k+j*dim[0]+i*dim[1]*dim[0];
              l=(l<<1);
              ll=i+j*dim[2]+k*dim[2]*dim[1];
              ll=(ll<<1);
              diff=sqrt(SQR(data2[ll]-data1[l])+SQR(data2[ll+1]-data1[l+1]));
              sum2 += SQR(data1[l])+SQR(data1[l+1]);
              sum += diff;
              if (diff > smax) smax=diff;
            }
        sum2=sqrt(sum2/tot);
        sum=sum/tot;
        cout << scientific << setprecision(2);
        cout << "(r.m.s.) value, (max,ave) discrepancy= ";
        cout << setw(13) << sum2 << setw(13) << smax;
        cout << setw(13) << sum << endl << endl;
        // now check the inverse transforms
        SWAP(dim[0],dim[2]);
        // This swap step is conceptually a reversal
        flswap=file[0]; file[0]=file[2]; file[2]=flswap;
        flswap=file[3]; file[3]=file[1]; file[1]=flswap;
        for (j=0;j<4;j++) (*file[j]).seekp(0);
        cout << "**************** now doing fourfs *********" << endl;
        NR::fourfs(file,dim,-1);
        for (j=0;j<4;j++) (*file[j]).seekg(0);
        (*file[2]).read((char *)&data1[0],nwrite*sizeof(DP));
        cc=(*file[2]).tellg()/sizeof(DP);
        if (cc != nwrite) NR::nrerror("read error in xfourfs");
        (*file[3]).read((char *)&data1[nwrite],nwrite*sizeof(DP));
        cc=(*file[3]).tellg()/sizeof(DP);
        if (cc != nwrite) NR::nrerror("read error in xfourfs");
        SWAP(dim[0],dim[2]);
        cout << "**************** now doing fourn *********" << endl;
        NR::fourn(data2,dim,-1);
        sum=smax=0.0;
        Vec_DP data1p(data1),data2p(data2);
        for (j=0;j<NDAT;j+=2) {
          sum1 += SQR(data2p[j])+SQR(data2p[j+1]);
          diff=sqrt(SQR(data2p[j]-data1p[j])+SQR(data2p[j+1]-data1p[j+1]));
          sum += diff;
          if (diff > smax) smax=diff;
        }
        sum=sum/tot;
        sum1=sqrt(sum1/tot);
        cout << "(r.m.s.) value, (max,ave) discrepancy= ";
        cout << setw(13) << sum1 << setw(13) << smax;
        cout << setw(13) << sum << endl << endl;
        cout << "ratio of r.m.s. values, expected ratio= ";
        cout << setw(12) << sum1/sum2 << setw(13) << sqrt(tot) << endl;
        for (j=0;j<4;j++) (*file[j]).close();
        for (j=0;j<4;j++) {
          if (remove(fnames[j].c_str()) != 0)
            NR::nrerror("Couldn't delete temporary file");
        }
        for (j=0;j<4;j++)
          delete file[j];
        return 0;
}
