// RUN: %check_clang_tidy %s readability-one-name-per-declaration %t

int cantTouchA, cantTouchB;

void simple() 
{
    int dontTouchC;
    
    long empty;
    long long1 = 11, *long2 = &empty, * long3 = &empty;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}long long1 = 11;
    // CHECK-FIXES: {{^    }}long *long2 = &empty;
    // CHECK-FIXES: {{^    }}long * long3 = &empty;
    
    long ** lint1, lint2 = 0, * lint3, **linn;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}long ** lint1;
    // CHECK-FIXES: {{^    }}long lint2 = 0;
    // CHECK-FIXES: {{^    }}long * lint3;
    // CHECK-FIXES: {{^    }}long **linn;
    
    	long int* lint4, *lint5,  lint6;
    	// CHECK-MESSAGES: :[[@LINE-1]]:6: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    	// CHECK-FIXES: {{^    	}}long int* lint4;
    	// CHECK-FIXES: {{^    	}}long int *lint5;
    	// CHECK-FIXES: {{^    	}}long int lint6;
    
    unsigned int uint1 = 0, uint2 = 44u, uint3, uint4=4;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}unsigned int uint1 = 0;
    // CHECK-FIXES: {{^    }}unsigned int uint2 = 44u;
    // CHECK-FIXES: {{^    }}unsigned int uint3;
    // CHECK-FIXES: {{^    }}unsigned int uint4=4;
    
    double darray1[] = {}, darray2[] = {1,	2}, dv1 = 3, dv2;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}double darray1[] = {};
    // CHECK-FIXES: {{^    }}double darray2[] = {1,	2};
    // CHECK-FIXES: {{^    }}double dv1 = 3;
    // CHECK-FIXES: {{^    }}double dv2;
    
    int notransform[] =   {
                              1,
                              2
                          };
    
    const int cx = 1, cy = 2;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}const int cx = 1;
    // CHECK-FIXES: {{^    }}const int cy = 2;
    
    volatile int vx, vy;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}volatile int vx;
    // CHECK-FIXES: {{^    }}volatile int vy;
    
    signed char sc1 = 'h', sc2;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}signed char sc1 = 'h';
    // CHECK-FIXES: {{^    }}signed char sc2;
    
    long long ll1, ll2, ***ft;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}long long ll1;
    // CHECK-FIXES: {{^    }}long long ll2;
    // CHECK-FIXES: {{^    }}long long ***ft;
    
    const char *cstr1 = "str1", *cstr2="str2";
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}const char *cstr1 = "str1";
    // CHECK-FIXES: {{^    }}const char *cstr2="str2";
    
    const char *literal1 = "clang"		"test" \
                           "one",
               *literal2 = "empty", literal3[] = "three";
    // CHECK-MESSAGES: :[[@LINE-3]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}const char *literal1 = "clang"		"test" \
    // CHECK-FIXES: {{^                           }}"one";
    // CHECK-FIXES: {{^    }}const char *literal2 = "empty";
    // CHECK-FIXES: {{^    }}const char literal3[] = "three";
    
    int intarray[] =
          {
           			1,
                    2,
                    3,
                    4
          }, bb = 5;
    // CHECK-MESSAGES: :[[@LINE-7]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}int intarray[] =
    // CHECK-FIXES: {{^    }}      {
    // CHECK-FIXES: {{^    }}       			1,
    // CHECK-FIXES: {{^    }}                2,
    // CHECK-FIXES: {{^    }}                3,
    // CHECK-FIXES: {{^    }}                4
    // CHECK-FIXES: {{^    }}      };
    // CHECK-FIXES: {{^    }}int bb = 5;
    
    const int cint3 = 4, cintarray[] = { 1, 2, 3, 4 };
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}const int cint3 = 4;
    // CHECK-FIXES: {{^    }}const int cintarray[] = { 1, 2, 3, 4 };
    
}
