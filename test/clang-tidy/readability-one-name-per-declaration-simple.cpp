// RUN: %check_clang_tidy %s readability-one-name-per-declaration %t

int cantTouchA, cantTouchB;

void simple() 
{
    int dontTouchC;
    
    long empty;
    long long1 = 11, *long2 = &empty, * long3 = &empty;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}long long1 = 11;
    // CHECK-FIXES: {{^    }}long *long2 = &empty;
    // CHECK-FIXES: {{^    }}long * long3 = &empty;
    
    long ** lint1, lint2 = 0, * lint3, **linn;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}long ** lint1;
    // CHECK-FIXES: {{^    }}long lint2 = 0;
    // CHECK-FIXES: {{^    }}long * lint3;
    // CHECK-FIXES: {{^    }}long **linn;
    
    	long int* lint4, *lint5,  lint6;
    	// CHECK-MESSAGES: :[[@LINE-1]]:6: warning: multiple declarations should be split
    	// CHECK-FIXES: {{^    	}}long int* lint4;
    	// CHECK-FIXES: {{^    	}}long int *lint5;
    	// CHECK-FIXES: {{^    	}}long int lint6;
    
    /* *& */ int /* *& */ ** /* *& */ pp,*xx;
    // CHECK-MESSAGES: :[[@LINE-1]]:14: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}/* *& */ int /* *& */ ** /* *& */ pp;
    // CHECK-FIXES: {{^    }}int *xx;
    
    unsigned int uint1 = 0,uint2 = 44u, uint3, uint4=4;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}unsigned int uint1 = 0;
    // CHECK-FIXES: {{^    }}unsigned int uint2 = 44u;
    // CHECK-FIXES: {{^    }}unsigned int uint3;
    // CHECK-FIXES: {{^    }}unsigned int uint4=4;
    
    const int * const cpc = &dontTouchC, simple = 0;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}const int * const cpc = &dontTouchC;
    // CHECK-FIXES: {{^    }}const int simple = 0;
    
    double darray1[] = {}, darray2[] = {1,	2},dv1 = 3,dv2;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}double darray1[] = {};
    // CHECK-FIXES: {{^    }}double darray2[] = {1,	2};
    // CHECK-FIXES: {{^    }}double dv1 = 3;
    // CHECK-FIXES: {{^    }}double dv2;
    
    int notransform[] =   {
                              1,
                              2
                          };
    
    const int cx = 1, cy = 2;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}const int cx = 1;
    // CHECK-FIXES: {{^    }}const int cy = 2;
    
    volatile int vx, vy;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}volatile int vx;
    // CHECK-FIXES: {{^    }}volatile int vy;
    
    signed char sc1 = 'h', sc2;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}signed char sc1 = 'h';
    // CHECK-FIXES: {{^    }}signed char sc2;
    
    long long ll1, ll2, ***ft;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}long long ll1;
    // CHECK-FIXES: {{^    }}long long ll2;
    // CHECK-FIXES: {{^    }}long long ***ft;
    
    const char *cstr1 = "str1", *cstr2="str2";
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}const char *cstr1 = "str1";
    // CHECK-FIXES: {{^    }}const char *cstr2="str2";
    
    const char *literal1 = "clang"		"test" \
                           "one",
               *literal2 = "empty", literal3[] = "three";
    // CHECK-MESSAGES: :[[@LINE-3]]:5: warning: multiple declarations should be split
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
    // CHECK-MESSAGES: :[[@LINE-7]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}int intarray[] =
    // CHECK-FIXES: {{^    }}      {
    // CHECK-FIXES: {{^    }}       			1,
    // CHECK-FIXES: {{^    }}                2,
    // CHECK-FIXES: {{^    }}                3,
    // CHECK-FIXES: {{^    }}                4
    // CHECK-FIXES: {{^    }}      };
    // CHECK-FIXES: {{^    }}int bb = 5;
    
    const int cint3 = 4, cintarray[] = { 1, 2, 3, 4 };
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}const int cint3 = 4;
    // CHECK-FIXES: {{^    }}const int cintarray[] = { 1, 2, 3, 4 };

    union Ss{
        int m1;
        float m2;
    } in, out;
    // CHECK-MESSAGES: :[[@LINE-4]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}Ss in;
    // CHECK-FIXES: {{^    }}Ss out;
    
    enum E {} E1, E2;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}E E1;
    // CHECK-FIXES: {{^    }}E E2;

    struct S {int t;} S1 = {1}, S2;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}S S1 = {1};
    // CHECK-FIXES: {{^    }}S S2;

    class C {}C1, C2;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}C C1;
    // CHECK-FIXES: {{^    }}C C2; 

    struct B {} ignoreMe1;
    enum {} ignoreMe2, ignoreMe3;
    struct {} ignoreMe4, ignoreMe5;

    typedef struct X { int t; } X, Y;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}typedef X X;
    // CHECK-FIXES: {{^    }}typedef X Y;

    
    int refme1;
    int refme2 = 0;
    const int &r1 = refme1, &r2 = refme2;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}const int &r1 = refme1;
    // CHECK-FIXES: {{^    }}const int &r2 = refme2;
    
    typedef int ta, tb;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}typedef int ta;
    // CHECK-FIXES: {{^    }}typedef int tb;
    
    typedef const int tca, tcb;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}typedef const int tca;
    // CHECK-FIXES: {{^    }}typedef const int tcb;
    
    typedef int const tac, tbc;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}typedef int const tac;
    // CHECK-FIXES: {{^    }}typedef int const tbc;
    
    int *(i), (*j), (((k)));
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: multiple declarations should be split
    // CHECK-FIXES: {{^    }}int *(i);
    // CHECK-FIXES: {{^    }}int (*j);
    // CHECK-FIXES: {{^    }}int (((k)));
}

