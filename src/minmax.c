
#include "minmax.h"

/**
 * Find the maximum value of a double array
 *
 * @param x a pointer array.
 * @param x_size size of the array.
 */
double GetMaxima(double *x, size_t x_size) {
	double max;
	max = x[0];
	for (size_t i = 0; i < x_size; i++) {
		if (x[i] > max) {
			max = x[i];
		}
	}
	return max;
}


/**
 * Find the minimum value of a double array
 *
 * @param x a pointer array.
 * @param x_size size of the array.
 */
double GetMinima(double *x, size_t x_size) {
	double min;
	min = x[0];
	for (size_t i = 0; i < x_size; i++) {
		if (x[i] < min) {
			min = x[i];
		}
	}
	return min;
}


/**
 * @note
 * It returns modified the array pointer provided in the arguments.
 *
 * @param x a pointer array.
 * @param size size of the array.
 * @param res the double array created in the parent call.
 */
double CalcMinMax(double *x, size_t x_size, double *res) {
    double min, max, max_min_diff;
    max = GetMaxima(x, x_size);
    min = GetMinima(x, x_size);
    max_min_diff = max - min;
    for (size_t i = 0; i < x_size; i++) {
        res[i] = (x[i] - min) / max_min_diff;
    }
    return *res;
}


/***** for debugging *****/
// int main(void) {
// 	double data[5] = {
// 		-0.290979594,
// 		1.607911564,
// 		0.654086188,
// 		-0.621329717,
// 		0.002338287
// 	};
//  GetMinima(data, 5);
//  GetMaxima(data, 5);
// 	double out[5];
// 	CalcMinMax(data, 5, out);
// 	return 0;
// }

