
#include "utils.h"

/* get length of an integer */
int nint(int *val){
	int res = 0;
	while(!(*val == 0)) {
		*val /= 10;
		res++;
	}
	return res;
}

/* Find the maximum value in an array */
int maxima(int *x, int *size) {
	int i, max;
	max = *(x+0);
	for(int i = 0; i < *size; i++) {
		if (*(x+i) > max) {
			max = *(x+i);
		}
	}
	return max;
}