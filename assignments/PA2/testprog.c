#include <stdio.h>
#include <string.h>

char glob_arr[5];

int main(){

	char arr[5];;
	char* arr_ptr;

	arr_ptr = arr;
	
	memset(arr,0,5);	

	*arr_ptr = 'A';
	arr_ptr++;
	*arr_ptr = 'B';
	arr_ptr++;

	strcpy(arr_ptr, "CD");
	arr_ptr += strlen("CD");

	printf("%ld\n", strlen("CD"));

	printf("%s\n",arr);


	printf("curr_size = %ld\n", arr_ptr-arr+1);



	return 0;
}