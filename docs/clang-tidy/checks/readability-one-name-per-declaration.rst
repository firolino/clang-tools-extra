.. title:: clang-tidy - readability-one-name-per-declaration

readability-one-name-per-declaration
====================================

This check can be used to find declarations, which declare more than one name. 
It helps improving readability and prevents potential bugs caused by inattention
and C/C++ syntax specifics.

In addition, appropriate fix-it hints are provided and all user-intended 
indentation will be preserved. For example:

.. code-block:: c++

  {
    long ** lint1, lint2 = 0, * lint3, **linn;
  
    const int cx = 1, cy = 2;
  
    int const CS :: * pp = &CS::a, CS::* const qq = &CS::a;
  
    decltype(int()) declint1 = 5, declint2 = 3;
    
    typedef int ta, tb;
  }

will be transformed to:

.. code-block:: c++

  {
    long ** lint1;
    long lint2 = 0;
    long * lint3;
    long **linn;
    
    const int cx = 1;
    const int cy = 2;
    
    int const CS :: * pp = &CS::a;
    int const CS::* const qq = &CS::a;
    
    decltype(int()) declint1 = 5;
    decltype(int()) declint2 = 3;
    
    typedef int ta;
    typedef int tb;
  }

Only declarations within a compound statement are matched. Meaning, global declarations
and function parameters are not matched. Moreover, it does not match on the following:

.. code-block:: c++

  {
    class A { } Object1, Object2;
    
    for(int i = 0, j = 0;;);
  }
