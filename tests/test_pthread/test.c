#include <stdio.h>
#include <pthread.h>

void *thread_2_body(void *x)
{
	int i;
	for (i = 0; i < *((int*)x); i ++) {
		printf("T2 iteration %d x = %d\n", i, *((int*) x));
	}
	printf("thread_2 finished\n");
	return NULL;
}

int main()
{

	int x = 42;

	printf("initial x: %d\n", x);

	pthread_t thread_2;

	if(pthread_create(&thread_2, NULL, thread_2_body, &x)) {
		fprintf(stderr, "Error creating thread\n");
		return 1;
	}

	printf("thread_2 started\n");
	if(pthread_join(thread_2, NULL)) {
		fprintf(stderr, "Error joining thread\n");
		return 2;
	}

	printf("end\n");
	return 0;
}
