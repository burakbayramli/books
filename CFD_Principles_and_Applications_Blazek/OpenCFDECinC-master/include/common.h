#pragma once
#ifndef _COMMON_H_
#define _COMMON_H_

#include<stdio.h>
#include<stdlib.h>
#define PAUSE system("pause");

//求出三个数中的最小值
template<typename T>
T min(T a, T b, T c) {
	T temp = a < b ? a : b;
	return temp < c ? temp : c;
}

template<typename T>
T max(T  a, T  b) {
	return a > b ? a : b;
}

template<typename T>
T min(T a, T b) {
	return a < b ? a : b;
}


//使用模板，指针，动态申请二维数组，传参数时应该要把矩阵的地址传过来，即 &B
//template<typename T>
//void  allocMatrix(T*** pointer, int m, int n)
//{
//	(*pointer) = (T **)malloc((m + 1) * sizeof(T *));
//	if ((*pointer) == nullptr) {
//		printf("内存空间不足，无法申请二维数组！\n");
//		exit(1);
//	}
//	for (int i = 0; i <= m; ++i) {
//		(*pointer)[i] = (T*)malloc((n + 1) * sizeof(T));
//		if ((*pointer)[i] == nullptr) {
//			printf("内存空间不足，无法申请二维数组！\n");
//			PAUSE;
//			exit(1);
//		}
//	}
//}

//使用模板，指针动态申请二维数组，传参数时使用了引用机制，直接将矩阵名传过来即可。
//这种方法与上面那一种使用纯指针的方法有所不用
template<typename T>
void  allocMatrix(T**  &pointer, int m, int n)
{
	pointer = (T **)malloc((m + 1) * sizeof(T *));
	if (pointer == nullptr) {
		printf("内存空间不足，无法申请二维数组！\n");
		exit(1);
	}
	for (int i = 0; i <= m; ++i) {
		pointer[i] = (T*)malloc((n + 1) * sizeof(T));
		if (pointer[i] == nullptr) {
			printf("内存空间不足，无法申请二维数组！\n");
			PAUSE;
			exit(1);
		}
	}
}


//使用模板，指针动态释放二维数组，传参数时使用了引用机制，直接将矩阵名传过来即可。
template<typename T>
void deleteMatrix(T ** &p, int m) {
	for (int i = m; i >= 0; --i) {
		free(p[i]);
		p[i] = nullptr;
	}
	free(p);
	p = nullptr;
}

//使用模板，指针动态申请三维数组，传参数时使用了引用机制，直接将矩阵名传过来即可。
template<typename T>
void  allocMatrix(T***  &pointer, int m, int n, int d)
{
	pointer = (T ***)malloc((m + 1) * sizeof(T **));
	if (pointer == nullptr) {
		printf("内存空间不足，无法申请二维数组！\n");
		PAUSE;
		exit(1);
	}
	for (int i = 0; i <= m; ++i) {
		pointer[i] = (T**)malloc((n + 1) * sizeof(T*));
		if (pointer[i] == nullptr) {
			printf("内存空间不足，无法申请二维数组！\n");
			PAUSE;
			exit(1);
		}
	}

	for (int i = 0; i <= m; ++i) {
		for (int j = 0; j <= n; ++j) {
			pointer[i][j] = (T*) malloc((d + 1) * sizeof(T));
			if (pointer[i][j] == nullptr) {
				printf("内存空间不足，无法申请二维数组！\n");
				PAUSE;
				exit(1);
			}
		}
	}
}

//使用模板，指针动态释放二维数组，传参数时使用了引用机制，直接将矩阵名传过来即可。
template<typename T>
void deleteMatrix(T *** &p, int m, int n) {
	for (int i = 0; i <= m; ++i) {
		for (int j = 0; j <= n; ++j) {
			free(p[i][j]);
			p[i][j] = nullptr;
		}
	}
	for (int i = m; i >= 0; --i) {
		free(p[i]);
		p[i] = nullptr;
	}
	free(p);
	p = nullptr;
}






#endif // !_COMMON_H_


