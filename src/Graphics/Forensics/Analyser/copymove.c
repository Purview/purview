#include <math.h>
#include <stdlib.h>
#include <memory.h>
#include "copymove.h"

typedef struct {
     int8_t flags; //flags that indicate the status of this block
     float ***dct; //this can either be a partial or full DCT
     int16_t loc_x; //x coordinate of the top left pixel
     int16_t loc_y; //y coordinate
} block;

typedef struct {
     block *from;
     block *to;
     int16_t magnitude;
     uint8_t angle;
} shift;

uint8_t BLOCKSIZE; //DCT block size
float QUALITY; //JPEG quality
uint8_t PDCTSIZE; //Partial DCT size which will be precomputed
int32_t SHIFT_THRESHOLD;

//8x8 JPEG quantization table
float q8[8][8] = {{16.0f, 11.0f, 10.0f, 16.0f,  24.0f,  40.0f,  51.0f,  61.0f},
                  {12.0f, 12.0f, 14.0f, 19.0f,  26.0f,  58.0f,  60.0f,  55.0f},
                  {14.0f, 13.0f, 16.0f, 24.0f,  40.0f,  57.0f,  69.0f,  56.0f},
                  {14.0f, 17.0f, 22.0f, 29.0f,  51.0f,  87.0f,  80.0f,  62.0f},
                  {18.0f, 22.0f, 37.0f, 56.0f,  68.0f, 109.0f, 103.0f,  77.0f},
                  {24.0f, 35.0f, 55.0f, 64.0f,  81.0f, 104.0f, 113.0f,  92.0f},
                  {49.0f, 64.0f, 78.0f, 87.0f, 103.0f, 121.0f, 120.0f, 101.0f},
                  {72.0f, 92.0f, 95.0f, 98.0f, 112.0f, 100.0f, 103.0f,  99.0f}};

float **q16;
float ****dct_pre;
block *blocks;
shift *shifts;
int32_t **shift_counts;

//Pre-stored values for DCT
float DOUBLE;
float SANS;
float SINGLE;

enum {
    PDCT_COMP = 1,
    FDCT_COMP = 2,
    REUSE_DCT = 4
};

//Function prototypes
float coeff(uint8_t x, uint8_t y);
void block_init(block *b, float **dct_mat, int16_t x, int16_t y, int8_t fl);
void block_update(block *b, float **dct_mat, int8_t fl);
void block_free_dct(block *b);
void shift_init(shift *s, block *from, block *to);
int block_compare(block *a, block *b);
int block_compare_from(block *a, block *b, int fx, int fy);
void init(void);
float **partial_dct(float **input, int size, int ox, int oy);
void cleanup(int blocks_size, int32_t max_mag);

//Returns the normalization coefficient for a given index in a DCT block
float coeff(uint8_t x, uint8_t y) {
     if (x == 0 && y == 0) return DOUBLE;
     else if (x == 1 || y == 1) return SINGLE;
     else return SANS;
}

//Initialise a block
void block_init(block *b, float **dct_mat, int16_t x, int16_t y, int8_t fl){
     b->flags = fl;
     b->dct = &dct_mat;
     b->loc_x = x;
     b->loc_y = y;
}

//Update the DCT array stored in a block
void block_update(block *b, float **dct_mat, int8_t fl){
     block_free_dct(b);
     b->dct = &dct_mat;
     b->flags = fl;
}

void block_free_dct(block *b) {
     //If the DCT is reused, we can't free the memory twice
     if (b->flags & REUSE_DCT) return;
     int ds = (b->flags & PDCT_COMP) ? PDCTSIZE : BLOCKSIZE;
     for (int i = 0; i < ds; i++) {
          free(b->dct[i]);
     }
     free(b->dct);
}

//Initialise a shift vector
void shift_init(shift *s, block *from, block *to) {
     s->from = from;
     s->to = to;
     float sx = (float)(from->loc_x - to->loc_x);
     float sy = (float)(from->loc_y - to->loc_y);
     s->magnitude = (int16_t)(sqrtf(sx * sx + sy * sy));
     float asimuth = atan2f(sy, sx);
     //Make sure we get an angle between 0 and 2*pi
     if (asimuth < 0) asimuth += 2 * 3.1415926f;
     s->angle = (uint8_t)(asimuth * (180 / 3.1415926f) + 0.5);
}

//Compares two `blocks`, comparison function for qsort
int block_compare(block *a, block *b) {
     if ((a->flags & PDCT_COMP) || (b->flags & PDCT_COMP)) {
          //If one or both blocks only have partial DCTs
          for (int x = 0; x < PDCTSIZE; x++) {
               for (int y = 0; y < PDCTSIZE; y++) {
                    if (a->dct[x][y] < b->dct[x][y]) return -1;
                    if (a->dct[x][y] > b->dct[x][y]) return 1;
               }
          }
     } else {
          //If both blocks have full DCTs
          for (int x = 0; x < BLOCKSIZE; x++) {
               for (int y = 0; y < BLOCKSIZE; y++) {
                    if (a->dct[x][y] < b->dct[x][y]) return -1;
                    if (a->dct[x][y] > b->dct[x][y]) return 1;
               }
          }
     }

     return 0;
}

//Compares two `blocks`, which MUST have full DCTs computed (this is not checked)
int block_compare_from(block *a, block *b, int fx, int fy) {
     for (int x = fx; x < BLOCKSIZE; x++) {
          for (int y = fy; y < BLOCKSIZE; y++) {
               if (a->dct[x][y] < b->dct[x][y]) return -1;
               if (a->dct[x][y] > b->dct[x][y]) return 1;
          }
     }
     return 0;
}

//Initializes global variables (q-table, constants) and precomputes DCT values
void init(void) {
     //Magic numbers from the JPEG algorithm specification
     DOUBLE = 1.0f / (float)BLOCKSIZE;
     SANS = 2.0f / (float)BLOCKSIZE;
     SINGLE = sqrtf(DOUBLE) * sqrtf(SANS);

     float s = QUALITY < 50 ? 5000.0f / QUALITY : 200.0f - 2.0f * QUALITY;
     for (int x = 0; x < 8; x++) {
          for (int y = 0; y < 8; y++) {
               q8[x][y] = (s * q8[x][y] + 50.0f) / 100.0f;
          }
     }
     q8[0][0] *= (float)BLOCKSIZE / 8.0f;
     //Interpolate q8 to the block size
     q16 = malloc(BLOCKSIZE * sizeof(*q16));
     for (int i = 0; i < BLOCKSIZE; i++)
          q16[i] = malloc(BLOCKSIZE * sizeof(*q16[i]));

     const float sc = 8.0f / (float)BLOCKSIZE; //scaling constant
     int fx, fy, cx, cy;
     float frac_x, frac_y, p1, p2, p3, p4;
     for (int8_t x = 0; x < BLOCKSIZE; x++) {
          for (int8_t y = 0; y < BLOCKSIZE; y++) {
               fx = (int)floorf(sc * (float)x);
               fy = (int)floorf(sc * (float)y);
               cx = fx == 7 ? fx : fx + 1;
               cy = fy == 7 ? fy : fy + 1;
               frac_x = ((float)x * sc) - (float)fx;
               frac_y = ((float)y * sc) - (float)fx;
               p1 = q8[fx][fy];
               p2 = q8[cx][fy];
               p3 = q8[fx][cy];
               p4 = q8[cx][cy];
               q16[x][y] = (1.0f-frac_y) * ((1.0f-frac_x) * p1 + frac_x * p2) +
                    frac_y * ((1.0f - frac_x) * p3 + frac_x * p4);
          }
     }
     //Initialize values used in the DCT
     dct_pre = malloc(BLOCKSIZE * sizeof(*dct_pre));
     float pi = 3.1415926f;
     float fb2 = (float)BLOCKSIZE * 2.0f;
     for (int u = 0; u < BLOCKSIZE; u++) {
          dct_pre[u] = malloc(BLOCKSIZE * sizeof(*dct_pre[u]));
          for (int v = 0; v < BLOCKSIZE; v++) {
               dct_pre[u][v] = malloc(BLOCKSIZE * sizeof(*dct_pre[u][v]));
               for (int x = 0; x < BLOCKSIZE; x++) {
                    dct_pre[u][v][x] = malloc(BLOCKSIZE * sizeof(*dct_pre[u][v][x]));
                    for (int y = 0; y < BLOCKSIZE; y++) {
                         dct_pre[u][v][x][y] =
                              cos(pi * (float)u * (2.0 * (float)x + 1.0) / fb2)*
                              cos(pi * (float)v * (2.0 * (float)y + 1.0) / fb2);
                    }
               }
          }
     }
}

//Main function, runs the analysis and returns a result image
float *runAnalysis(float *input, int32_t width, int32_t height, uint8_t blocksize, float qual, uint8_t pdctsize, int32_t shift_threshold) {
     //Set all the settings
     BLOCKSIZE = blocksize;
     QUALITY = qual;
     PDCTSIZE = pdctsize;
     SHIFT_THRESHOLD = shift_threshold;

     //Initialize q-table, precompute values for DCT
     init();

     //Maximum x/y coordinates for the blocks
     size_t nx = (size_t)width - (size_t)BLOCKSIZE + 1;
     size_t ny = (size_t)height - (size_t)BLOCKSIZE + 1;
     size_t nxy = nx * ny;

     //Convert the input to 2D, make output array
     float *result = calloc(width * height, sizeof(*result));
     float **image = malloc(width * sizeof(*image));
     for (int i = 0; i < width; i++) {
          image[i] = malloc(height * sizeof(*image[i]));
          for (int j = 0; j < height; j++) {
               image[i][j] = input[i + height * j];
          }
     }

     blocks = malloc(nxy * sizeof(*blocks));

     //Compute each block's partial quantized DCT into a vector of blocks
     for (int x = 0; x < nx; x++) {
          for (int y = 0; y < ny; y++) {
               block_init(&blocks[(nx * y) + x], partial_dct(image, 2, x, y), x, y, PDCT_COMP);
          }
     }

     //Sort the blocks
     qsort(blocks, nxy, sizeof(block), block_compare);

     //Make shift vectors, init shift count
     shifts = malloc(nxy * sizeof(*shifts));
     int sc = 0;

     for (int i = 0; i < nxy - 1; i++) {
          block *block1 = &blocks[i];
          block *block2 = &blocks[i + 1];

          if (block_compare(block1, block2) == 0) {
               //See if the blocks have 16x16 DCTs
               int8_t p1 = block1->flags & PDCT_COMP;
               int8_t p2 = block2->flags & PDCT_COMP;

               if (p1 || p2) {
                    if (p1) {
                         //Compute full DCT and set flags accordingly
                         int8_t newflags = (block1->flags ^ PDCT_COMP) | FDCT_COMP;
                         //TODO: Use FFTW to compute the full DCT
                         block_update(block1, partial_dct(image, BLOCKSIZE, block1->loc_x, block1->loc_y), newflags);
                    }
                    if (p2) {
                         //Same with block2
                         int8_t newflags = (block2->flags ^ PDCT_COMP) | FDCT_COMP;
                         block_update(block2, partial_dct(image, BLOCKSIZE, block2->loc_x, block2->loc_y), newflags);
                    }
                    //If the full blocks don't match, continue the for loop. Uses the slightly faster compare function.
                    if (block_compare_from(block1, block2, PDCTSIZE, PDCTSIZE) != 0) continue;
               }

               //The full blocks matched, create a shift, increase shift count
               shift_init(&shifts[sc], block1, block2);
               sc++;

               //Since the DCT arrays match, reuse one between matching blocks, set reuse flag to prevent double free
               block_update(block1, *(block2->dct), block1->flags | REUSE_DCT);
          }
     }

     //Reallocate shift vector array to free some memory
     shifts = realloc(shifts, sc * sizeof(shift));

     //Save the shift counts in a max_magnitude * max_angle integer array
     int32_t max_mag = (int32_t)sqrtf((float)(width * width + height * height));
     shift_counts = malloc(max_mag * sizeof(*shift_counts));
     for (int i = 0; i < max_mag; i++)
          shift_counts[i] = calloc(360, sizeof(*shift_counts[i]));

     //For each shift vector, increment the counter
     shift *current;
     for (int i = 0; i < sc; i++) {
          current = &shifts[i];
          shift_counts[current->magnitude][current->angle]++;
     }

     //Mark areas in the result image
     for (int i = 0; i < sc; i++) {
          current = &shifts[i];
          if (shift_counts[current->magnitude][current->angle] > SHIFT_THRESHOLD) {
               result[current->from->loc_x + height * current->from->loc_y] = 1.0f;
               result[current->to->loc_x + height * current->to->loc_y] = 1.0f;
          }
     }

     //Free allocated memory
     for (int i = 0; i < width; i++)
          free(image[i]);
     free(image);

     cleanup(nxy, max_mag);

     return result;
}

//Computes part of a DCT with the given input
float **partial_dct(float **input, int size, int ox, int oy) {
     //This function computes a quantized partial DCT of size*size. It
     //takes the full image array as input, together with an offset (ox, oy) of
     //the top left pixel of the block. All blocks are quadratic and
     //have an edge length of BLOCKSIZE. It is the caller's
     //responsibility that no block pixel is outside of the image.
     float **result = malloc(size * sizeof(*result));
     float sum;
     //u,v for each value in the result matrix
     for (int u = 0; u < size; u++){
          result[u] = malloc(size * sizeof(*result[u]));
          for (int v = 0; v < size; v++){
               sum = 0.0f;
               //x, y for each value in the input matrix
               for (int x = 0; x < BLOCKSIZE; x++) {
                    for (int y = 0; y < BLOCKSIZE; y++){
                         sum += input[x + ox][y + oy] * dct_pre[u][v][x][y];
                    }
               }
               sum *= coeff(u, v);
               sum /= q16[u][v]; //Quantize the result
               result[u][v] = roundf(sum);
          }
     }
     return result;
}

//Frees allocated memory after the program finishes
void cleanup(int blocks_size, int32_t max_mag){
     for (int i = 0; i < BLOCKSIZE; i++)
          free(q16[i]);
     free(q16);

     for (int u = 0; u < BLOCKSIZE; u++) {
          for (int v = 0; v < BLOCKSIZE; v++) {
               for (int x = 0; x < BLOCKSIZE; x++) {
                    free(dct_pre[u][v][x]);
               }
               free(dct_pre[u][v]);
          }
          free(dct_pre[u]);
     }
     free(dct_pre);

     for (int i = 0; i < blocks_size; i++)
          block_free_dct(&blocks[i]);
     free(blocks);

     free(shifts);

     for (int i = 0; i < max_mag; i++)
          free(shift_counts[i]);
     free(shift_counts);
}

void c_free(void *ptr) {
     free(ptr);
}
