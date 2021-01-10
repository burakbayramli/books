//##############################################################################
//
// Copyright (C), 2005, Danny Thorne
//
// spy.c
//
//  - "spy" a bitmap file and print info about it.
//
//  - If it is small, print matrix of pixel values.
//

#include <stdio.h>
#include <math.h>

//#if SWAP_BYTE_ORDER || OSTYPE==darwin
#if SWAP_BYTE_ORDER
// Swap byte order.
#define ENDIAN2(w) ((((w)&0x00ff)<<8)|(((w)&0xff00)>>8))
#define ENDIAN4(w) ((((w)&0x000000ff)<<24)|(((w)&0xff000000)>>24)|(((w)&0x0000ff00)<<8)|(((w)&0x00ff0000)>>8))
#else /* !( SWAP_BYTE_ORDER) */
#define ENDIAN2(w) (w)
#define ENDIAN4(w) (w)
#endif /* SWAP_BYTE_ORDER */



// Reference:
//   http://www.fortunecity.com/skyscraper/windows/364/bmpffrmt.html
//   (See text at bottom of this file.)

struct bitmap_file_header
{
  // 1 2 bfType 19778 must always be set to 'BM' to declare that this is
  // a .bmp-file.
  char bfType[2];

  // 3 4 bfSize ?? specifies the size of the file in bytes.
  char bfSize[4];

  // 7 2 bfReserved1 0 must always be set to zero.
  char bfReserved1[2];

  // 9 2 bfReserved2 0 must always be set to zero.
  char bfReserved2[2];

  // 11 4 bfOffBits 1078 specifies the offset from the beginning of the
  // file to the bitmap data.
  char bfOffBits[4];

};

struct bitmap_info_header
{
  // 15 4 biSize 40 specifies the size of the BITMAPINFOHEADER structure,
  // in bytes.
  char biSize[4];
  // 19 4 biWidth 100 specifies the width of the image, in pixels.
  char biWidth[4];
  // 23 4 biHeight 100 specifies the height of the image, in pixels.
  char biHeight[4];
  // 27 2 biPlanes 1 specifies the number of planes of the target device,
  // must be set to zero. [DT: Should be set to one, right? Not zero.]
  char biPlanes[2];
  // 29 2 biBitCount 8 specifies the number of bits per pixel.
  char biBitCount[2];
  // 31 4 biCompression 0 Specifies the type of compression, usually set
  // to zero (no compression).
  char biCompression[4];
  // 35 4 biSizeImage 0 specifies the size of the image data, in bytes.
  // If there is no compression, it is valid to set this member to zero.
  char biSizeImage[4];
  // 39 4 biXPelsPerMeter 0 specifies the the horizontal pixels per meter
  // on the designated targer device, usually set to zero.
  char biXPelsPerMeter[4];
  // 43 4 biYPelsPerMeter 0 specifies the the vertical pixels per meter
  // on the designated targer device, usually set to zero.
  char biYPelsPerMeter[4];
  // 47 4 biClrUsed 0 specifies the number of colors used in the bitmap,
  // if set to zero the number of colors is calculated using the biBitCount
  // member.
  char biClrUsed[4];
  // 51 4 biClrImportant 0 specifies the number of color that are
  // 'important' for the bitmap, if set to zero, all colors are important.
  char biClrImportant[4];

};

struct rgb_quad
{
  // 1 1 rgbBlue - specifies the blue part of the color.
  char Blue;
  // 2 1 rgbGreen - specifies the green part of the color.
  char Green;
  // 3 1 rgbRed - specifies the red part of the color.
  char Red;
  // 4 1 rgbReserved - must always be set to zero.
  char Reserved;
};


main( int argc, int **argv)
{
  int **spy,

      height, width,

      i, j;

  char filename[1024];

  FILE *o;

  //printf("\n");
  //printf("main() -- Hi!\n");

  if( argc == 1)
  {
    printf("\n");
    printf("usage>> ./spy [ input_file_name [output_file_name]]\n");
    printf("\n");
    process_exit(1);
  }
  else
  {
    sprintf( filename, "%s", *(argv+1));
    printf("\n");
    printf("Input filename is \"%s\".\n", filename);
  }
  read_bmp( filename);
  spy_bmp( filename, &spy, &height, &width);

  printf(">> spy( 1:%d, 1:%d)\n", height, width);
  printf("\n");

  if( height <= 10 && width <= 40)
  {
    for( j=0; j<height; j++)
    {
      for( i=0; i<width; i++)
      {
        printf(" %d", spy[j][i]);
      }
      printf("\n");
    }
    printf("\n");
  }

  if( argc > 2)
  {
    if( argc != 3)
    {
      printf("usage>> spy [ input_file_name [output_file_name]]\n");
    }
    sprintf( filename, "%s", *(argv+2));
    if( !( o = fopen( filename, "w+")))
    {
      printf("Error opening \"%s\".\n", filename);
    }
    else
    {
      printf("Outputting spy results to \"%s\".\n", filename);
      for( j=0; j<height; j++)
      {
        for( i=0; i<width; i++)
        {
          fprintf( o, " %d", spy[j][i]);
        }
        fprintf( o, "\n");
      }
      fclose(o);
    }
  }

  // Deallocate memory used by the spy matrix.
  for( j=0; j<height; j++)
  {
    free( spy[j]);
  }
  free( spy);


  printf("\n");
  printf("Done.\n");
  printf("\n");

  return 0;

} /* main( int argc, int **argv) */

// read_bmp( char *filename)
//##############################################################################
//
// R E A D   B M P
//
read_bmp( char *filename)
{
  FILE *in;
  int i, n, m;
  int pad, bytes_per_row;
  char k;
  char b;
  struct bitmap_file_header bmfh;
  struct bitmap_info_header bmih;
  struct rgb_quad rgb;
  int *int_ptr;
  short int *short_int_ptr;
  int *width_ptr;
  int *height_ptr;
  short int *bitcount_ptr;
  int width, height, depth;

  printf("\n");
  printf("read_bmp() -- Hi!\n");
  printf("\n");

  //printf("sizeof(char) = %d\n", sizeof(char));
  //printf("sizeof(int)  = %d\n", sizeof(int));
  //printf("\n");

  //printf("sizeof( struct bitmap_file_header) = %d\n",
  //    sizeof(struct bitmap_file_header));
  //printf("sizeof( struct bitmap_info_header) = %d\n",
  //    sizeof(struct bitmap_info_header));
  //printf("\n");

  //in = fopen( "junk.bmp", "r");
  in = fopen( filename, "r");

  // n = fread( void *BUF, size_t SIZE, size_t COUNT, FILE *FP);

  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  printf("struct bitmap_file_header:\n");
  //char bfType[2];
  printf("  bfType:      \"%c%c\"\n", bmfh.bfType[0], bmfh.bfType[1]);
  //char bfSize[4];
  //int_ptr = ENDIAN4(((int)(*((int*)(bmfh.bfSize)))));
  printf("  bfSize:       %d bytes\n", ENDIAN4(((int)(*((int*)(bmfh.bfSize))))));
  //char bfReserved1[2];
  short_int_ptr = (short int*)bmfh.bfReserved1;
  printf("  bfReserved1:  %d\n", *short_int_ptr);
  //char bfReserved2[2];
  short_int_ptr = (short int*)bmfh.bfReserved2;
  printf("  bfReserved2:  %d\n", *short_int_ptr);
  //char bfOffBits[4];
  //int_ptr = ENDIAN4(((int)(*((int*)(bmfh.bfOffBits)))));
  printf("  bfOffBits:    %d\n", ENDIAN4(((int)(*((int*)(bmfh.bfOffBits))))));
  printf("\n");

  n = fread( &bmih, sizeof(struct bitmap_info_header), 1, in );
  int_ptr = (int*)bmih.biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  printf("struct bitmap_info_header:\n");
  // char biSize[4]
  //int_ptr = (int*)bmih.biSize;
  printf("  biSize:          %d bytes\n", ENDIAN4(((int)(*((int*)(bmih.biSize))))));
  // char biWidth[4]
  //int_ptr = (int*)bmih.biWidth;
  printf("  biWidth:         %d pixels\n", ENDIAN4(((int)(*((int*)(bmih.biWidth))))));
  // char biHeight[4]
  //int_ptr = (int*)bmih.biHeight;
  //printf("  biHeight:        %d pixels\n", *int_ptr);
  printf("  biHeight:        %d pixels\n", ENDIAN4(((int)(*((int*)(bmih.biHeight))))));
  // char biPlanes[2]
  //short_int_ptr = (short int*)bmih.biPlanes;
  printf("  biPlanes:        %d\n", ENDIAN2(((int)(*((int*)(bmih.biPlanes))))));
  // char biBitCount[2]
  //short_int_ptr = (short int*)bmih.biBitCount;
  printf("  biBitCount:      %d bits per pixel\n", ENDIAN2(((int)(*((int*)(bmih.biBitCount))))));
  // char biCompression[4]
  int_ptr = (int*)bmih.biCompression;
  printf("  biCompression:   %d\n", *int_ptr);
  // char biSizeImage[4]
  int_ptr = (int*)bmih.biSizeImage;
  printf("  biSizeImage:     %d bytes\n", *int_ptr);
  // char biXPelsPerMeter[4]
  int_ptr = (int*)bmih.biXPelsPerMeter;
  printf("  biXPelsPerMeter: %d\n", *int_ptr);
  // char biYPelsPerMeter[4]
  int_ptr = (int*)bmih.biYPelsPerMeter;
  printf("  biYPelsPerMeter: %d\n", *int_ptr);
  // char biClrUsed[4]
  int_ptr = (int*)bmih.biClrUsed;
  printf("  biClrUsed:       %d\n", *int_ptr);
  // char biClrImportant[4]
  int_ptr = (int*)bmih.biClrImportant;
  printf("  biClrImportant:  %d\n", *int_ptr);
  printf("\n");

  width = ENDIAN4(((int)(*((int*)(bmih.biWidth)))));
  height = ENDIAN4(((int)(*((int*)(bmih.biHeight)))));
  depth = ENDIAN4(((int)(*((int*)(bmih.biBitCount)))));

  if( depth < 24)
  {
    //printf("(double)*(bmih.biBitCount) = %f\n", (double)*(bmih.biBitCount));
    n = (double)pow(2.,depth);
    printf("n = %d entries in palette.\n", n);
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
#if 0
      printf("%4d: [ \"%c\" \"%c\" \"%c\"] (\"%c\")\n",
        i, rgb.Red, rgb.Green, rgb.Blue, rgb.Reserved );
#else
      printf("%4d: #", i);
      printf("%1x%1x", (rgb.Red & 0xf0)>>4, (rgb.Red & 0x0f));
      printf("%1x%1x", (rgb.Green & 0xf0)>>4, (rgb.Green & 0x0f));
      printf("%1x%1x", (rgb.Blue & 0xf0)>>4, (rgb.Blue & 0x0f));
      printf(" ");
      printf("( %3d, %3d, %3d)",
        (rgb.Red+256)%256,
        (rgb.Green+256)%256,
        (rgb.Blue+256)%256   );
      printf("\n");
#endif
    }
  }
  else
  {
    printf("BitCount = %d, so no color table.\n",(int)*(bmih.biBitCount));
  }
  printf("\n");

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)width)*((double)depth))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  printf("----------------------------------------\n");
  printf("Pixel Data:  ");
  if( !( width <= 10 && height <= 20))
  {
    printf("<Too large to display.>\n");
  }
  else
  {
    printf("\n");
  }
  n = 0;
  m = 0;
  n+=( k = fread( &b, 1, 1, in ));
  while( !feof(in))
  {
#if 0
    printf("%d", (b & 0x80) != 0);
    printf("%d", (b & 0x40) != 0);
    printf("%d", (b & 0x20) != 0);
    printf("%d", (b & 0x10) != 0);
    printf("%d", (b & 0x08) != 0);
    printf("%d", (b & 0x04) != 0);
    printf("%d", (b & 0x02) != 0);
    printf("%d", (b & 0x01) != 0);
    printf(" (%3d)", (b+256)%256);
    printf(" (%2d,%2d) |", (b&0xf0)>>4, b&0x0f);
#else
  if( width <= 10 && height <= 20)
  {
    printf(" ");
    switch(depth)
    {
      case 1: // Monochrome.
        printf("%d", (b & 0x80) != 0);
        printf("%d", (b & 0x40) != 0);
        printf("%d", (b & 0x20) != 0);
        printf("%d", (b & 0x10) != 0);
        printf("%d", (b & 0x08) != 0);
        printf("%d", (b & 0x04) != 0);
        printf("%d", (b & 0x02) != 0);
        printf("%d", (b & 0x01) != 0);
        break;
      case 4: // 16 colors.
        printf("%2d", (b&0xf0)>>4);
        printf("  ");
        printf("%2d", b&0x0f);
        break;
      case 8: // 256 colors.
        printf("%3d", (b+256)%256);
        break;
      case 24: // 24-bit colors.
        printf("%d", (b & 0x80) != 0);
        printf("%d", (b & 0x40) != 0);
        printf("%d", (b & 0x20) != 0);
        printf("%d", (b & 0x10) != 0);
        printf("%d", (b & 0x08) != 0);
        printf("%d", (b & 0x04) != 0);
        printf("%d", (b & 0x02) != 0);
        printf("%d", (b & 0x01) != 0);
        break;
      default: // 32-bit colors?
        printf("ERROR: Unhandled color depth, "
            "BitCount = %d. Exiting!\n", depth);
        process_exit(1);
        break;
    }
    printf(" |");
  }
#endif
    if( !(n%(bytes_per_row+pad)))
    {
      if( width <= 5 && width <= 10) { printf("\n");}
      m++;
    }
    n+=( k = fread( &b, 1, 1, in ));

  } /* while( !feof(in)) */

  if( width <= 5 && width <= 10) { printf("\n");}
  printf("----------------------------------------\n");

  printf("\n");
  printf("%d bytes.\n", n);
  printf("%d rows.\n", m);
  printf("%d bytes containing data per row.\n", bytes_per_row);
  printf("%d bytes of padding per row.\n", pad);
  printf("( %d + %d) * %d = %d\n",
    bytes_per_row, pad, m, (bytes_per_row+pad)*m);
  printf("\n");

  if( (bytes_per_row+pad)*m!=n)
  {
    printf("WARNING: Num bytes read = %d versus num bytes predicted = %d .\n",
        n, (bytes_per_row+pad)*m);
  }

  if( m != height)
  {
    printf("WARNING: m (%d) != bmih.biHeight (%d).\n", m, height);
  }

  fclose(in);

  printf("read_bmp() -- Bye!\n");
  printf("\n");

} /* read_bmp( char *filename) */

// spy_bmp( char *filename, int ***spy)
//##############################################################################
//
// S P Y   B M P
//
spy_bmp( char *filename, int ***spy, int *arg_height, int *arg_width)
{
  FILE *in;
  int i, j, n, m;
  int pad, bytes_per_row;
  char k;
  char b;
  struct bitmap_file_header bmfh;
  struct bitmap_info_header bmih;
  struct rgb_quad rgb;
  int *int_ptr;
  short int *short_int_ptr;
  int *width_ptr;
  int *height_ptr;
  short int *bitcount_ptr;
  int width, height, depth;

  printf("spy_bmp() -- Hi!\n");

  in = fopen( filename, "r");

  // n = fread( void *BUF, size_t SIZE, size_t COUNT, FILE *FP);

  n = fread( &bmfh, sizeof(struct bitmap_file_header), 1, in );
  if( strncmp(bmfh.bfType,"BM",2))
  {
    printf("ERROR: Can't process this file type.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }
  n = fread( &bmih, sizeof(struct bitmap_info_header), 1, in );
  int_ptr = (int*)bmih.biCompression;
  if( *int_ptr != 0)
  {
    printf("ERROR: Can't handle compression.  Exiting!\n");
    printf("\n");
    process_exit(1);
  }

  width_ptr = (int*)bmih.biWidth;
  height_ptr = (int*)bmih.biHeight;
  bitcount_ptr = (short int*)bmih.biBitCount;

  width = ENDIAN4(((int)(*((int*)(bmih.biWidth)))));
  *arg_width = width;
  height = ENDIAN4(((int)(*((int*)(bmih.biHeight)))));
  *arg_height = height;
  depth = ENDIAN4(((int)(*((int*)(bmih.biBitCount)))));

  if( depth < 24)
  {
    n = (double)pow(2.,(double)depth); // Num palette entries.
    for( i=0; i<n; i++)
    {
      k = fread( &rgb, sizeof(struct rgb_quad), 1, in );
      if( k!=1)
      {
        printf("Error reading palette entry %d.  Exiting!\n", i);
        process_exit(1);
      }
    }
  }

  // Allocate memory for the spy matrix.
  *spy = (int**)malloc( (height)*sizeof(int*));
  for( m=0; m<height; m++)
  {
    (*spy)[m] = (int*)malloc( (width)*sizeof(int));
  }
#if 0
  memset( &((*spy)[0][0]), 0, (*width)*(*height)*sizeof(int));
#else

  for( j=0; j<height; j++)
  {
    for( i=0; i<width; i++)
    {
      (*spy)[j][i] = 0;
    }
  }
#endif

  // Bytes per row of the bitmap.
  bytes_per_row =
    ((int)ceil(( (((double)(width))*((double)(depth)))/8.)));

  // Bitmaps pad rows to preserve 4-byte boundaries.
  // The length of a row in the file will be bytes_per_row + pad .
  pad = ((4) - bytes_per_row%4)%4;

  n = 0;
  m = 0;
  n+=( k = fread( &b, 1, 1, in ));
  i = 0;
  j = height-1;
  while( !feof(in))
  {
    switch(depth)
    {
      case 1: // Monochrome.
        if( i < width) { (*spy)[j][i] = ( (b & 0x80) == 0); }
        i++;
        if( i < width) { (*spy)[j][i] = ( (b & 0x40) == 0); }
        i++;
        if( i < width) { (*spy)[j][i] = ( (b & 0x20) == 0); }
        i++;
        if( i < width) { (*spy)[j][i] = ( (b & 0x10) == 0); }
        i++;
        if( i < width) { (*spy)[j][i] = ( (b & 0x08) == 0); }
        i++;
        if( i < width) { (*spy)[j][i] = ( (b & 0x04) == 0); }
        i++;
        if( i < width) { (*spy)[j][i] = ( (b & 0x02) == 0); }
        i++;
        if( i < width) { (*spy)[j][i] = ( (b & 0x01) == 0); }
        i++;
        break;

      case 4: // 16 colors.
        if( i < width) { (*spy)[j][i] = ( (b&0xf0)>>4 != 15); }
        i++;
        if( i < width) { (*spy)[j][i] = ( (b&0x0f) != 15); }
        i++;
        break;

      case 8: // 256 colors.
        if( i < width) { (*spy)[j][i] = ( (b&0xff) != 255); }
        i++;
        break;

      case 24: // 24-bit colors.
        if( i < 3*(width))
        {
//printf("(*spy)[%d][%d] = %d -> ", j, (int)floor((double)i/3.),
//    (*spy)[j][(int)floor((double)i/3.)]);
          (*spy)[j][(int)floor((double)i/3.)]  |= ( (b&0xff) != 255);
//printf("%d\n", (*spy)[j][(int)floor((double)i/3.)]);
//printf("( (%d&0xff) != 255) = %d\n", b, ( (b&0xff) != 255));
        }
        i++;
        break;

      default: // 32-bit colors?
        printf("ERROR: Unhandled color depth, "
            "BitCount = %d. Exiting!\n", depth);
        process_exit(1);
        break;

    } /* switch(*(bmih.biBitCount)) */

    if( !(n%(bytes_per_row+pad))) { m++; i=0; j--;}
    n+=( k = fread( &b, 1, 1, in ));

  } /* while( !feof(in)) */

  if( (bytes_per_row+pad)*m!=n)
  {
    printf("WARNING: Num bytes read = %d versus num bytes predicted = %d .\n",
        n, (bytes_per_row+pad)*m);
  }

  if( m != height)
  {
    printf("WARNING: m (%d) != bmih.biHeight (%d).\n", m, height);
  }

  fclose(in);

  printf("spy_bmp() -- Bye!\n");
  printf("\n");

} /* spy_bmp( char *filename, int ***spy, int *height, int *width) */

/*

<http://www.fortunecity.com> 	web hosting
<http://www.fortunecity.com/> 	domain names
<http://www.fortunecity.com/> 	email addresses <http://www.v3.com/> 	
search <http://www.fortunecity.com/marketplace/> 	

<http://ads.fortunecity.com/RealMedia/ads/click_nx.cgi/en/m_computers_and_internet/410390@Top>


  The .bmp file format


      Introduction:

The .bmp file format (sometimes also saved as .dib) is the standard for
a Windows 3.0 or later DIB(device independent bitmap) <bmsinwin.html>
file. It may use compression (though I never came across a compressed
.bmp-file) and is (by itself) not capable of storing animation. However,
you can animate a bitmap using different methods but you have to write
the code which performs the animation. There are different ways to
compress a .bmp-file, but I won't explain them here because they are so
rarely used. The image data itself can either contain pointers to
entries in a color table or literal RGB values (this is explained later).


      Basic structure:

A .bmp file contains of the following data structures:

BITMAPFILEHEADER    bmfh;
BITMAPINFOHEADER    bmih;
RGBQUAD             aColors[];
BYTE                aBitmapBits[];

/bmfh/ contains some information about the bitmap file (about the file,
not about the bitmap itself). /bmih/ contains information about the
bitmap such as size, colors,... The /aColors array/ contains a color
table. The rest is the image data, which format is specified by the
/bmih/ structure.


      Exact structure:

The following tables give exact information about the data structures
and also contain the settings for a bitmap with the following
dimensions: size 100x100, 256 colors, no compression. The /start/-value
is the position of the byte in the file at which the explained data
element of the structure starts, the /size/-value contains the nuber of
bytes used by this data element, the /name/-value is the name assigned
to this data element by the Microsoft API documentation. /Stdvalue/
stands for standard value. There actually is no such a thing as a
standard value but this is the value Paint assigns to the data element
if using the bitmap dimensions specified above (100x100x256). The
/meaning/-column gives a short explanation of the purpose of this data
element.


      The BITMAPFILEHEADER:

*start* 	*size* 	*name* 	*stdvalue* 	*purpose*
1 	2 	bfType 	19778 	must always be set to 'BM' to declare that this is
a .bmp-file.
3 	4 	bfSize 	?? 	specifies the size of the file in bytes.
7 	2 	bfReserved1 	0 	must always be set to zero.
9 	2 	bfReserved2 	0 	must always be set to zero.
11 	4 	bfOffBits 	1078 	specifies the offset from the beginning of the
file to the bitmap data.


      The BITMAPINFOHEADER:

start 	size 	name 	stdvalue 	purpose
15 	4 	biSize 	40 	specifies the size of the BITMAPINFOHEADER structure,
in bytes.
19 	4 	biWidth 	100 	specifies the width of the image, in pixels.
23 	4 	biHeight 	100 	specifies the height of the image, in pixels.
27 	2 	biPlanes 	1 	specifies the number of planes of the target device,
must be set to zero.
29 	2 	biBitCount 	8 	specifies the number of bits per pixel.
31 	4 	biCompression 	0 	Specifies the type of compression, usually set
to zero (no compression).
35 	4 	biSizeImage 	0 	specifies the size of the image data, in bytes.
If there is no compression, it is valid to set this member to zero.
39 	4 	biXPelsPerMeter 	0 	specifies the the horizontal pixels per meter
on the designated targer device, usually set to zero.
43 	4 	biYPelsPerMeter 	0 	specifies the the vertical pixels per meter
on the designated targer device, usually set to zero.
47 	4 	biClrUsed 	0 	specifies the number of colors used in the bitmap,
if set to zero the number of colors is calculated using the biBitCount
member.
51 	4 	biClrImportant 	0 	specifies the number of color that are
'important' for the bitmap, if set to zero, all colors are important.


Note that /biBitCount/ actually specifies the color resolution of the
bitmap. The possible values are: 1 (black/white); 4 (16 colors); 8 (256
colors); 24 (16.7 million colors). The biBitCount data element also
decides if there is a color table in the file and how it looks like. In
1-bit mode the color table has to contain 2 entries (usually white and
black). If a bit in the image data is clear, it points to the first
palette entry. If the bit is set, it points to the second. In 4-bit mode
the color table must contain 16 colors. Every byte in the image data
represents two pixels. The byte is split into the higher 4 bits and the
lower 4 bits and each value of them points to a palette entry. There are
also standard colors for 16 colors mode (16 out of Windows 20 reserved
colors <rescolrs.html> (without the entries 8, 9, 246, 247)). Note that
you do not need to use this standard colors if the bitmap is to be
displayed on a screen which support 256 colors or more, however (nearly)
every 4-bit image uses this standard colors. In 8-bit mode every byte
represents a pixel. The value points to an entry in the color table
which contains 256 entries (for details see Palettes in Windows
<palinwin.html>. In 24-bit mode three bytes represent one pixel. The
first byte represents the red part, the second the green and the third
the blue part. There is no need for a palette because every pixel
contains a literal RGB-value, so the palette is omitted.


      The RGBQUAD array:

The following table shows a single RGBQUAD structure:
start 	size 	name 	stdvalue 	purpose
1 	1 	rgbBlue 	- 	specifies the blue part of the color.
2 	1 	rgbGreen 	- 	specifies the green part of the color.
3 	1 	rgbRed 	- 	specifies the red part of the color.
4 	1 	rgbReserved 	- 	must always be set to zero.


Note that the term /palette/ does not refer to a RGBQUAD array, which is
called /color table/ instead. Also note that, in a color table
(RGBQUAD), the specification for a color starts with the blue byte. In a
palette a color always starts with the red byte. There is no simple way
to map the whole color table into a LOGPALETTE structure, which you will
need to display the bitmap. You will have to write a function that
copies byte after byte.


      The pixel data:

It depens on the BITMAPINFOHEADER structure how the pixel data is to be
interpreted (see above <bmpffrmt.html#bmih>).
It is important to know that the rows of a DIB are stored upside down.
That means that the uppest row which appears on the screen actually is
the lowest row stored in the bitmap, a short example:

	
pixels displayed on the screen 	pixels stored in .bmp-file

You do not need to turn around the rows manually. The API functions
which also display the bitmap will do that for you automatically.
Another important thing is that the number of bytes in one row must
always be adjusted to fit into the border of a multiple of four. You
simply append zero bytes until the number of bytes in a row reaches a
multiple of four, an example:

6 bytes that represent a row in the bitmap: 	A0 37 F2 8B 31 C4
must be saved as: 	A0 37 F2 8B 31 C4 *00 00*

to reach the multiple of four which is the next higher after six
(eight). If you keep these few rules in mind while working with
.bmp files it should be easy for you, to master it.

Back to the main page <index.html>


------------------------------------------------------------------------
Copyright 1998 Stefan Hetzl. If you have questions or comments or have
discovered an error, send mail <mailto:hetzl@teleweb.at> to
hetzl@teleweb.at. You may forward this document or publish it on your
webpage as long as you don't change it and leave this notice at the end.

<http://www.fortunecity.com/?sid=fcfootergif>
web hosting <http://www.fortunecity.com/?sid=fcfooterhosting> ? domain
names <http://www.fortunecity.com/?sid=fcfooterdomains> ? web design
<http://www.fortunecity.com/web-design.shtml?sid=fcfooterdesign>
online games <http://www.hotgames.com/> ? digital cameras
<http://www.onlinereviewguide.com/> ? photo album
<http://www.myphotoalbum.com/>
advertising online <http://www.ampiramedia.com/> ? online casino
<http://www.newyorkscasinos.com/>

*/

