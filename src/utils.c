
#include "utils.h"

/* get length of an integer */
int nint(int *x){
	int res = 0;
	while(!(*x == 0)) {
		*x /= 10;
		res++;
	}
	return res;
}

/* count the number of characters */
int nchar(char *x) {
	int counter = 0;
	while (*x != '\0') {
    	x++;
	counter++;
	}
	return counter;
}

/* Find the maximum value in an integer array */
int int_maxima(int *x, int *size) {
	int i, max;
	max = *(x+0);
	for(i = 0; i < *size; i++) {
		if (*(x+i) > max) {
			max = *(x+i);
		}
	}
	return max;
}
