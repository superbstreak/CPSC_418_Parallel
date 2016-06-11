// =========================================================================================
// Name:  Chia Hsuan Wu
// SID:   42764118
// CSID:  y4d8
// =========================================================================================

#include <math.h>
#include <stdio.h>
#include "time_it.h"
#include <cuda.h>
#include <curand_kernel.h>




/***************************************************************************************
*  print_vec: print the first few elements of a vector
*****************************************************************************************/

// CUDA_CALL and setup_kernel are from 
//   http://docs.nvidia.com/cuda/curand/device-api-overview.html#device-api-example
#define CUDA_CALL(x) do { if((x) != cudaSuccess) { \
  printf("Error at %s:%d\n",__FILE__,__LINE__); \
  return EXIT_FAILURE;}} while(0)
  
void print_vec(float *x, uint n, const char *fmt, const char *who) {
  printf("%s = ", who);
  for(int i = 0; i < n; i++) {
    if(i > 0) printf(", ");
    printf(fmt, x[i]);
  }
  if(n > 10) printf(", ...");
  printf("\n");
}

void print_arr(uint *x, uint n, const char *fmt, const char *who) {
  printf("%s = ", who);
  for(int i = 0; i < n; i++) {
    if(i > 0) printf(", ");
    printf(fmt, x[i]);
  }
  if(n > 10) printf(", ...");
  printf("\n");
}




/***************************************************************************************
*  Question 1: GFlops
*****************************************************************************************/

// http://cuda-programming.blogspot.ca/2013/01/what-is-constant-memory-in-cuda.html
// declare constant memory
__constant__ float firstXvalue[1];

// arguments to the do_norm fuction as seen in perc.cu
struct logistic_arg {
	float *x;
	uint n;
	uint m;
};

// cuda version
__global__ void logisticKernel(float *x, int n, int m, float alpha) {
	int currentId = blockDim.x * blockIdx.x + threadIdx.x;
	if (currentId > 0 && currentId < n) {	
		// since the subsequent element will always be replaced by previous one
		// and the previous one was initally affected by x[0], then it is essentially saying
		// we do it offset*m times	

		float result = firstXvalue[0]; // cached, read-only access of global memory.
		
		int madM = currentId*m;

		// int madM = threadIdx.x*m;
		// at a different block, read from the previous block's last element
		// if (blockIdx.x > 0 && threadIdx.x == 0) {
		// 	result = x[blockDim.x * (blockIdx.x - 1) + (blockDim.x - 1)];
		// } 

		// do m*threadId times
		for (int i = 0; i < madM; ++i) {
			result = alpha*result*(1.0f - result);	// previous value
		}

		// write back to global
		x[currentId] = result;
		
	}
}

float* logistic(float *x, int n, int m) {
	float alpha = 3.6;
	float first[1];
	for (int i = 0; i < 1; ++i) {
		first[0] = x[i];
	}
	float *dev_x, *y;
	int size = n*sizeof(float);
	y = (float *)malloc(size);
	cudaMalloc((void**)(&dev_x), size);
	cudaMemcpy(dev_x, x, size, cudaMemcpyHostToDevice);
	cudaMemcpyToSymbol(firstXvalue,first, sizeof(float));
	logisticKernel<<<ceil(n/256.0), 256>>>(dev_x, n, m, alpha); 
	cudaMemcpy(y, dev_x, size, cudaMemcpyDeviceToHost);
	cudaFree(dev_x);
	return y;
}

void do_logistic(void *arg) {
	struct logistic_arg *logisticArg = (struct logistic_arg*)(arg);
	float* res = logistic(logisticArg->x, logisticArg->n, logisticArg->m);
	cudaDeviceSynchronize();
}

// sequestial version
void logistic_ref(float *xf, int n, int m) {
	float alpha = 3.6;
	for (int i = 1; i < n; i++) {
		for (int j = 0; j < m; j++) {
			xf[i] = alpha*xf[i - 1]*(1.0f - xf[i - 1]);
		}
	}
	print_vec(xf, min(10, n), "%5.3f", "Logistic");
}





/***************************************************************************************
*  Question 2: GBps
*****************************************************************************************/

// arguments to the do_norm fuction as seen in perc.cu
struct norm_arg {
	float *x;
	uint n;
};

// multiply: read 1 write 1
__global__ void normKernal(float *x, uint n) {
	// store block data in here so we dont re read
	__shared__ float cache[256];

	uint dim = blockDim.x;
	uint blockBase = dim * blockIdx.x;
	uint tid = threadIdx.x ;
	uint myId = blockBase + tid;

	if(myId < n) {
		// x[myId] *= x[myId];	// write it directly back

		// store square version
		float val = x[myId];
		cache[tid] = val*val;

		// wait for everyone reach this point
		__syncthreads();	
		
		// if you are the first element of the block do this
		if (tid == 0) {
			float sum = 0.0;
			int adjusted = 256;
			// check if it is in the last block
			if (n - myId < 256) {
				adjusted = n - myId;
			}
			// sum up
			for (int i = 0; i < adjusted; i++) {
				sum += cache[i];
			}
			
			//write back
			x[blockIdx.x] = sum;
		}         
	}
}

// univ caller
float norm (float *x, uint n) {
	float *y;
	float *dev_x, *dev_y;
	uint blksize = 256;
	uint nblk = ceil(n/256.0);
	int size = n*sizeof(float);
	int Ysize = nblk*sizeof(float);
	y = (float *)malloc(size);
	cudaMalloc((void**)(&dev_x), size);
	cudaMalloc((void**)(&dev_y), size);
	cudaMemcpy(dev_x, x, size, cudaMemcpyHostToDevice);
	normKernal<<<nblk,blksize>>>(dev_x, n); 
	cudaMemcpy(y, dev_x, Ysize, cudaMemcpyDeviceToHost);

	// copy back and do the final sum up only nblock items
	float result = 0.0f;
	for (int i = 0; i < nblk; i++) {
		result += y[i];
	}
	result = sqrt(result);
	free(y);
	cudaFree(dev_x);
	cudaFree(dev_y);
	return result;
}

void do_norm(void *arg) {
	struct norm_arg *normArgument = (struct norm_arg*)(arg);
	float res = norm(normArgument->x, normArgument->n);
	cudaDeviceSynchronize();
	// printf("Norm PAR = %f\n", res);
}

// seq version of the norm
void norm_ref (float *x, uint n) {
	float norm = 0.0;
	for (int i = 0; i < n; ++i) {
		norm += x[i]*x[i];
	}
	norm = sqrt(norm);
	printf("Norm = %f\n", norm);
}





/***************************************************************************************
*  Question 3
*****************************************************************************************/

// arguments to the rndm fuction as seen in perc.cu
struct rndm_arg {
  uint n,  // n = nblocks * threads per block * m elements 
       m;  // m elements
  curandState *dev_randState; // an array of n random number generators
  uint *dev_v; // write the final state here.
};

__global__ void setup_kernel(uint n, curandState *state) {
  uint myId = blockDim.x * blockIdx.x + threadIdx.x;

	// int myId = threadIdx.x + blockIdx.x * 256;	// offset count
  /* Each thread gets same seed, a different sequence 
     number, no offset */
  if(myId < n)
    curand_init(1234, myId, 0, &state[myId]);
}

// a modified version of 
// http://docs.nvidia.com/cuda/curand/device-api-overview.html#device-api-example
// with reference to perc.cu provided
__global__ void rndm(uint *v, uint n, curandState *state) {
	uint myId = blockDim.x*blockIdx.x + threadIdx.x;

	if (myId < n) {
		// Copy state to local memory
		curandState localState = state[myId];

		// Generate pseudo-random unsigned ints
		uint x = curand(&localState); 

		// Copy state back to global memory
		// this is becuase the number generate from the same state and the result will be the same
		// Since we want them to be different. We store the state back into global memory for
		// random number generations
		state[myId] = localState; 

		// Store results 
		v[myId] = x;
	}
}


void do_rndm(void *arg) {
	struct rndm_arg *my_arg = (struct rndm_arg*)(arg);
	rndm<<<1,256>>>(my_arg->dev_v, my_arg->n, my_arg->dev_randState);
	cudaDeviceSynchronize();
}





/***************************************************************************************
*  MAIN
* ./hw3 1 n m 			 <- time logistics
* ./hw3 2 n any_number	 <- time norm
* ./hw3 3 m any_number   <- time random
*****************************************************************************************/

int main(int argc, char **argv) {
	uint choice = atoi(argv[1]);
	uint n = atoi(argv[2]);
	uint m = atoi(argv[3]);

	logistic_arg logParg;
	norm_arg parg;
	rndm_arg rdmParg;

	struct time_it_raw *tr = time_it_create(10);
  	struct time_it_stats stats;
  	float *x, *x_ref;
  	float result = 0.0;
  	int ops = 0;

  	int size = n*sizeof(float);
  	x = (float *)malloc(size);
	x_ref = (float *)malloc(size);

	for (int i = 0; i < n; i++) {
		x[i] = 0.123;
		x_ref[i] = 0.123;
	}

	switch (choice) {
		case 1:	// Question 1
			logParg.x = x;
			logParg.n = n;
			logParg.m = m;
			ops = 3*m*(n*(n - 1)/2);
			printf("\n");
			printf("Q1 Logistic\n");
			logistic_ref(x_ref, n, m);
			time_it_run(tr, do_logistic, (void *)(&logParg));
			time_it_get_stats(tr, &stats);
			result = ((ops/stats.mean)/pow(10,9)); // read + write
			printf("Logistic: n = %u, mean Time = %10.3le, std = %10.3le, GFps = %f\n", n, stats.mean, stats.std, result);
		break;
		case 2:
			printf("\n");
			printf("Q2 Norm\n");
			norm_ref(x_ref,n);
			parg.x = x;
			parg.n = n;
			time_it_run(tr, do_norm, (void *)(&parg));
			time_it_get_stats(tr, &stats);
			result = (((2*n*sizeof(float))/stats.mean)/pow(10,9));
			printf("Norm: n = %u, mean Time = %10.3le, std = %10.3le, GBps = %f\n", n, stats.mean, stats.std, result);
		break;
		case 3:
			m = 1;
			printf("\n");
			printf("Q3 Random\n");
			uint *v;
			m = n;
			n = 1 * 256 * n;
			if (n < 1000000) {
				n = 1000000;
				m = 1;
			}
			rdmParg.n = n;
			rdmParg.m = m;

			// allocate an array for the result on the CPU
			int vsz = (rdmParg.n)*sizeof(uint);
			v = (uint *)malloc(vsz);

			// allocate the result array and pseudo-random number generator states on the GPU
			CUDA_CALL(cudaMalloc((void **)(&rdmParg.dev_v), vsz));
			CUDA_CALL(cudaMalloc((void **)(&rdmParg.dev_randState), rdmParg.n*sizeof(curandState)));
			setup_kernel<<<1, 256>>>(rdmParg.n, rdmParg.dev_randState);

			// make the timing measurements.
			time_it_run(tr, do_rndm, (void *)(&rdmParg));

			// fetch the final state from the GPU
			cudaMemcpy(v, rdmParg.dev_v, vsz, cudaMemcpyDeviceToHost);
			time_it_get_stats(tr, &stats);
			result = (n/stats.mean);
			printf("rndm(%u, %u): pnps = %f, mean(T) = %10.3le, stddev(T) = %10.3le\n",
            	result, rdmParg.n, rdmParg.m, stats.mean, stats.std);
			print_arr(v, min(10, n), "%u", "RNDM V: ");
			CUDA_CALL(cudaFree(rdmParg.dev_randState));
			CUDA_CALL(cudaFree(rdmParg.dev_v));	
			free(v);
		break;
	}
	time_it_free(tr);
	free(x);
	free(x_ref);
	exit(0);
}