#include "stdio.h"
#include "stdlib.h"
#include "math.h"

/*
   The following is rather crude demonstration code to read 
   uncompressed and compressed TGA files of 16, 24, or 32 bit 
   TGA. 
   Hope it is helpful.
*/

typedef struct {
   char  idlength;
   char  colourmaptype;
   char  datatypecode;
   short int colourmaporigin;
   short int colourmaplength;
   char  colourmapdepth;
   short int x_origin;
   short int y_origin;
   short width;
   short height;
   char  bitsperpixel;
   char  imagedescriptor;
} HEADER;

typedef struct {
   unsigned char r,g,b,a;
} PIXEL;

void MergeBytes(PIXEL *,unsigned char *,int);

int main(int argc,char **argv)
{
   int n=0,i,j;
   int bytes2read,skipover = 0;
   unsigned char p[5];
   FILE *fptr;
   HEADER header;
   PIXEL *pixels;

   if (argc < 2) {
      fprintf(stderr,"Usage: %s tgafile\n",argv[0]);
      exit(-1);
   }

   /* Open the file */
   if ((fptr = fopen(argv[1],"r")) == NULL) {
      fprintf(stderr,"File open failed\n");
      exit(-1);
   }

   /* Display the header fields */
   header.idlength = fgetc(fptr);
   fprintf(stderr,"ID length:         %d\n",header.idlength);
   header.colourmaptype = fgetc(fptr);
   fprintf(stderr,"Colourmap type:    %d\n",header.colourmaptype);
   header.datatypecode = fgetc(fptr);
   fprintf(stderr,"Image type:        %d\n",header.datatypecode);
   fread(&header.colourmaporigin,2,1,fptr);
   fprintf(stderr,"Colour map offset: %d\n",header.colourmaporigin);
   fread(&header.colourmaplength,2,1,fptr);
   fprintf(stderr,"Colour map length: %d\n",header.colourmaplength); 
   header.colourmapdepth = fgetc(fptr);
   fprintf(stderr,"Colour map depth:  %d\n",header.colourmapdepth);
   fread(&header.x_origin,2,1,fptr);
   fprintf(stderr,"X origin:          %d\n",header.x_origin);
   fread(&header.y_origin,2,1,fptr);
   fprintf(stderr,"Y origin:          %d\n",header.y_origin);
   fread(&header.width,2,1,fptr);
   fprintf(stderr,"Width:             %d\n",header.width);
   fread(&header.height,2,1,fptr);
   fprintf(stderr,"Height:            %d\n",header.height);
   header.bitsperpixel = fgetc(fptr);
   fprintf(stderr,"Bits per pixel:    %d\n",header.bitsperpixel);
   header.imagedescriptor = fgetc(fptr);
   fprintf(stderr,"Descriptor:        %d\n",header.imagedescriptor);

   /* Allocate space for the image */
   if ((pixels = malloc(header.width*header.height*sizeof(PIXEL))) == NULL) {
      fprintf(stderr,"malloc of image failed\n");
      exit(-1);
   }
   for (i=0;i<header.width*header.height;i++) {
      pixels[i].r = 0;
      pixels[i].g = 0;
      pixels[i].b = 0;
      pixels[i].a = 0;
   }

   /* What can we handle */
   if (header.datatypecode != 2 && header.datatypecode != 10) {
      printf(stderr,"Can only handle image type 2 and 10\n");
      exit(-1);
   }
   if (header.bitsperpixel != 16 && 
       header.bitsperpixel != 24 && header.bitsperpixel != 32) {
      fprintf(stderr,"Can only handle pixel depths of 16, 24, and 32\n");
      exit(-1);
   }
   if (header.colourmaptype != 0 && header.colourmaptype != 1) {
      fprintf(stderr,"Can only handle colour map types of 0 and 1\n");
      exit(-1);
   }

   /* Skip over unnecessary stuff */
   skipover += header.idlength;
   skipover += header.colourmaptype * header.colourmaplength;
   fprintf(stderr,"Skip over %d bytes\n",skipover);
   fseek(fptr,skipover,SEEK_CUR);

   /* Read the image */
   bytes2read = header.bitsperpixel / 8;
   while (n < header.width * header.height) {
      if (header.datatypecode == 2) {                     /* Uncompressed */
         if (fread(p,1,bytes2read,fptr) != bytes2read) {
            fprintf(stderr,"Unexpected end of file at pixel %d\n",i);
            exit(-1);
         }
         MergeBytes(&(pixels[n]),p,bytes2read);
         n++;
      } else if (header.datatypecode == 10) {             /* Compressed */
         if (fread(p,1,bytes2read+1,fptr) != bytes2read+1) {
            fprintf(stderr,"Unexpected end of file at pixel %d\n",i);
            exit(-1);
         }
         j = p[0] & 0x7f;
         MergeBytes(&(pixels[n]),&(p[1]),bytes2read);
         n++;
         if (p[0] & 0x80) {         /* RLE chunk */
            for (i=0;i<j;i++) {
               MergeBytes(&(pixels[n]),&(p[1]),bytes2read);
               n++;
            }
         } else {                   /* Normal chunk */
            for (i=0;i<j;i++) {
               if (fread(p,1,bytes2read,fptr) != bytes2read) {
                  fprintf(stderr,"Unexpected end of file at pixel %d\n",i);
                  exit(-1);
               }
               MergeBytes(&(pixels[n]),p,bytes2read);
               n++;
            }
         }
      }
   }
   fclose(fptr);

   /* Write the result as a uncompressed TGA */
   if ((fptr = fopen("tgatest.tga","w")) == NULL) {
      fprintf(stderr,"Failed to open outputfile\n");
      exit(-1);
   }
   putc(0,fptr);
   putc(0,fptr);
   putc(2,fptr);                         /* uncompressed RGB */
   putc(0,fptr); putc(0,fptr);
   putc(0,fptr); putc(0,fptr);
   putc(0,fptr);
   putc(0,fptr); putc(0,fptr);           /* X origin */
   putc(0,fptr); putc(0,fptr);           /* y origin */
   putc((header.width & 0x00FF),fptr);
   putc((header.width & 0xFF00) / 256,fptr);
   putc((header.height & 0x00FF),fptr);
   putc((header.height & 0xFF00) / 256,fptr);
   putc(32,fptr);                        /* 24 bit bitmap */
   putc(0,fptr);
   for (i=0;i<header.height*header.width;i++) {
      putc(pixels[i].b,fptr);
      putc(pixels[i].g,fptr);
      putc(pixels[i].r,fptr);
      putc(pixels[i].a,fptr);
   }
   fclose(fptr);
}

void MergeBytes(PIXEL *pixel,unsigned char *p,int bytes)
{
   if (bytes == 4) {
      pixel->r = p[2];
      pixel->g = p[1];
      pixel->b = p[0];
      pixel->a = p[3];
   } else if (bytes == 3) {
      pixel->r = p[2];
      pixel->g = p[1];
      pixel->b = p[0];
      pixel->a = 0;
   } else if (bytes == 2) {
      pixel->r = (p[1] & 0x7c) << 1;
      pixel->g = ((p[1] & 0x03) << 6) | ((p[0] & 0xe0) >> 2);
      pixel->b = (p[0] & 0x1f) << 3;
      pixel->a = (p[1] & 0x80);
   }
}

