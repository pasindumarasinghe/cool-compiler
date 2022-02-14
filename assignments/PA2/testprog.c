#include <stdio.h>
#include <string.h>

char glob_arr[5];

int main(){

	char arr[] = {'a','b','c'};
	char* arr_ptr;

	arr_ptr = arr;

	printf("%c\n", *arr_ptr++);
	printf("%c\n", *arr_ptr++);
	printf("%c\n", *arr_ptr);

	printf("curr_size = %ld\n", arr_ptr-arr+1);



	return 0;
}