
#include "utils.h"

// get length of an integer
int NumLen(int val){
	int res = 0;
	while(!(val == 0)) {
		val /= 10;
		res++;
	}
	return res;
}

