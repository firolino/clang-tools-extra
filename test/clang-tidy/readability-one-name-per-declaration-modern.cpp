// RUN: %check_clang_tidy %s readability-one-name-per-declaration %t -- -- \
// RUN:   -std=c++14

namespace std {
    
    template <typename T>
    class initializer_list {};
    
    template <typename T>
    class vector 
    {
      public:
        vector() {}
        vector(initializer_list<T> init) {}
    };
    
    class string 
    {
      public:
        string() {}
        string(const char*) {}
    };
    
    namespace string_literals {
    
        string operator""s(const char*, decltype(sizeof(int))) 
        {   
            return string(); 
        }
    }
}

namespace Types {
    
    typedef int MyType;    
    int dontTouch1, dontTouch2;
}

void modern() 
{
    auto autoInt1 = 3, autoInt2 = 4;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^....}}auto autoInt1 = 3;
    // CHECK-FIXES: {{^....}}auto autoInt2 = 4;
    
    decltype(int()) declnottouch= 4;
    
    decltype(int()) declint1 = 5, declint2 = 3;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]    
    // CHECK-FIXES: {{^....}}decltype(int()) declint1 = 5;
    // CHECK-FIXES: {{^....}}decltype(int()) declint2 = 3;
    
    std::vector<int> vectorA = {1,2}, vectorB = {1,2,3}, vectorC({1,1,1});
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^....}}std::vector<int> vectorA = {1,2};
    // CHECK-FIXES: {{^....}}std::vector<int> vectorB = {1,2,3};
    // CHECK-FIXES: {{^....}}std::vector<int> vectorC({1,1,1});
    
    using uType = int;
    uType utype1, utype2;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^....}}uType utype1;
    // CHECK-FIXES: {{^....}}uType utype2;
    
    Types::MyType mytype1, mytype2, mytype3 = 3;
    // CHECK-MESSAGES: :[[@LINE-1]]:5: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
    // CHECK-FIXES: {{^....}}Types::MyType mytype1;
    // CHECK-FIXES: {{^....}}Types::MyType mytype2;
    // CHECK-FIXES: {{^....}}Types::MyType mytype3 = 3;
    
    {
        using namespace std::string_literals;
        
        std::vector<std::string> s{"foo"s, "bar"s}, t{"foo"s}, u, a({"hey", "you"}), bb = {"h", "a" };
        // CHECK-MESSAGES: :[[@LINE-1]]:9: warning: declaration statement can be split up into single line declarations [readability-one-name-per-declaration]
        // CHECK-FIXES: {{^        }}std::vector<std::string> s{"foo"s, "bar"s};
        // CHECK-FIXES: {{^        }}std::vector<std::string> t{"foo"s};
        // CHECK-FIXES: {{^        }}std::vector<std::string> u;
        // CHECK-FIXES: {{^        }}std::vector<std::string> a({"hey", "you"});
        // CHECK-FIXES: {{^        }}std::vector<std::string> bb = {"h", "a" };
    }
}

