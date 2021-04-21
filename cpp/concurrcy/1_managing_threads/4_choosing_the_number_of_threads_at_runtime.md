# 1.4 Choosing the number of threads at runtime

One frature of the C++ Standard Library that helps here is  
`std::thread::hardware_concurrency()`. This function returns an indication of  
the number of threads that can **truly run concurrently** for a given execution  
of a program.

[A native parrllel version of std::accumulate](list2_9.cc)
