// RUN: %check_clang_tidy %s readability-one-name-per-declaration %t -- -- \
// RUN:    -std=c++11

void dontTouchParameter(int param1, int param2)
{}

int returner(void) 
{
    int f0 = 0, f1 = 1;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}int f0 = 0;
    // CHECK-FIXES: {{^    }}int f1 = 1;
    
    return 3;
}

struct StructOne 
{
    StructOne(){}
    StructOne(int b){}
    
    int cantTouch1, cantTouch2;
};

void complex() 
{
    typedef int* IntPtr;
    typedef int ArrayType[2];
    typedef int FunType(void);
    
    IntPtr intptr1, intptr2 = nullptr, intptr3;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^....}}IntPtr intptr1;
    // CHECK-FIXES: {{^....}}IntPtr intptr2 = nullptr;
    // CHECK-FIXES: {{^....}}IntPtr intptr3;
    
    ArrayType arraytype1, arraytype2 = {1}, arraytype3;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^....}}ArrayType arraytype1;
    // CHECK-FIXES: {{^....}}ArrayType arraytype2 = {1};
    // CHECK-FIXES: {{^....}}ArrayType arraytype3;
    
    FunType funtype1, funtype2, functype3;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^....}}FunType funtype1;
    // CHECK-FIXES: {{^....}}FunType funtype2;
    // CHECK-FIXES: {{^....}}FunType functype3;
    
    for(int index1 = 0, index2 = 0;;)
    {
        int localFor1 = 1, localFor2 = 2;
        // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
        // CHECK-FIXES: {{^        }}int localFor1 = 1;
        // CHECK-FIXES: {{^        }}int localFor2 = 2;
    }
    
    int v1, v2(3), v3, v4(4 ), v5{2}, v6 = {3};
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}int v1;
    // CHECK-FIXES: {{^    }}int v2(3);
    // CHECK-FIXES: {{^    }}int v3;
    // CHECK-FIXES: {{^    }}int v4(4 );
    // CHECK-FIXES: {{^    }}int v5{2};
    // CHECK-FIXES: {{^    }}int v6 = {3};
    
    StructOne s1, s2(23), s3, s4(3), *sptr = new StructOne(2);
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}StructOne s1;
    // CHECK-FIXES: {{^    }}StructOne s2(23);
    // CHECK-FIXES: {{^    }}StructOne s3;
    // CHECK-FIXES: {{^    }}StructOne s4(3);
    // CHECK-FIXES: {{^    }}StructOne *sptr = new StructOne(2);
    
    int *ptrArray[3], dummy, **ptrArray2[5], twoDim[2][3], *twoDimPtr[2][3];
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}int *ptrArray[3];
    // CHECK-FIXES: {{^    }}int dummy;
    // CHECK-FIXES: {{^    }}int **ptrArray2[5];
    // CHECK-FIXES: {{^    }}int twoDim[2][3];
    // CHECK-FIXES: {{^    }}int *twoDimPtr[2][3];
    
        {
            void f1(int), g1(int, float);
            // CHECK-MESSAGES: :[[@LINE-1]]:13: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
            // CHECK-FIXES: {{^            }}void f1(int);
            // CHECK-FIXES: {{^            }}void g1(int, float);
        }

        {
            void gg(int, float);
            
            void ( *f2)(int), (*g2)(int, float) = gg;
            // CHECK-MESSAGES: :[[@LINE-1]]:13: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
            // CHECK-FIXES: {{^            }}void ( *f2)(int);
            // CHECK-FIXES: {{^            }}void (*g2)(int, float) = gg;
            
        }
    
    struct S { int a; const int b; };
    
    int S::*p = &S::a, S::* const q = &S::a;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}int S::*p = &S::a;
    // CHECK-FIXES: {{^    }}int S::* const q = &S::a;
    
    const int S::*r = &S::b, S::*t;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}const int S::*r = &S::b;
    // CHECK-FIXES: {{^    }}const int S::*t;
    
    typedef const int S::*MemPtr;
    MemPtr aaa =  &S::a, bbb = &S::b;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}MemPtr aaa =  &S::a;
    // CHECK-FIXES: {{^    }}MemPtr bbb = &S::b;
    
    class CS { public: int a; const int b; };
    int const CS :: * pp = &CS::a, CS::* const qq = &CS::a;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}int const CS :: * pp = &CS::a;
    // CHECK-FIXES: {{^    }}int const CS::* const qq = &CS::a;
    
    int intfunction = returner(), intarray[] =
          {
                  1,
                  2,
                  3,
                  4
          }, bb = 4;
    // CHECK-MESSAGES: :[[@LINE-7]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^    }}int intfunction = returner();
    // CHECK-FIXES: {{^    }}int intarray[] =
    // CHECK-FIXES: {{^          }}{
    // CHECK-FIXES: {{^                  }}1,
    // CHECK-FIXES: {{^                  }}2,
    // CHECK-FIXES: {{^                  }}3,
    // CHECK-FIXES: {{^                  }}4
    // CHECK-FIXES: {{^          }}};
    // CHECK-FIXES: {{^    }}int bb = 4;
}
