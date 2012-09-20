#include <stdint.h>

float *runAnalysis(float *input, int32_t width, int32_t height, uint8_t blocksize,
                    float qual, uint8_t pdctsize, int32_t shift_threshold);
void c_free(void *ptr);
