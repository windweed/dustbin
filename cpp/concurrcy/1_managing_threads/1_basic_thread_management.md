# 1.1 Basic thread management

Every C++ program has at least one thread, which is started by the C++ runtime:  
the thread running `main()`.

Your program can then launch additional threads that have another function as  
the entry point.

These threads then run concurrently with each other and with the initial thread.

In the same way that the program exits when it returns from `main()`, when the  
specified entry point function returns ,the thread exits.

## 1.1.1 Launching a thread

Threads are started by constructing a `std::thread` object that specifies the  
task to run on that thread.

It doesn't matter what the thread is going to do or where it's launched from,  
but starting a thread using the C++ Standard Library always boils down to  
constructing a `std::thread` object:
```cpp
void do_some_work();
std::thread my_thread(do_some_work);
```
As with much of the C++ Standard Library, `std::thread` works with any callable  
type, so you can pass an instance of a class with a function call operator to  
the `std::thread` constructor instead:
```cpp
class background_task {
public:
    void operator() () const {
        do_something();
        do_something_else();
    }
};
background_task f;
std::thread my_thread(f);
```
In this case, the supplied function object is copied into the storage belonging  
to the newly created thread of execution and invoked from there.

If you pass a temporary rather than a named variable, the syntax can be the same  
as that of a function declaration, in which case the compiler interprets it as  
such, rather than an object definition. For example,
```cpp
std::thread my_thread(background_tack());
```
declares a `my_thread` function that takes a single parameter(of type  
*pointer-to-a-function-taking-no-parameters-and-returning-a-background_tack_object*)  
and returns a `std::thread` object, rather than launching a new thread.

You can avoid this by naming your function object as shown previously, by using  
an extra set of parentheses, or by using the new uniform initialization syntax;  
```cpp
// extra parentheses prevent interpretation as a function declaration,
// allowing `my_thread` to be declared as a variable of type `std::thread`.
std::thread my_thread((background_task()));
// uses the new uniform initialization syntax, thus would declare a variable.
std::thread my_thread {background_task()};
```
One type of callable object that avoids this problem is a *lambda expression*.  
```cpp
std::thread my_thread([]{
    do_something();
    do_something_else();
});
```

Once you've started your thread, you need to explicitly decide whether to wait  
for it to finish(by joining with it) or leave it to run on its own(by detaching  
it).

If you don't decide before the `std::thread` object is destroyed, then your  
program is terminated(the `std::thread` destructor calls `std::terminate()`).  

It's therefore imperative that you ensure that the thread is correctly joined  
or detached, even in the presence of exceptions.

Note that you only have to make this decision before the `std::thread` object is  
destroyed -- the thread itself may well have finished long before you join with  
it or detach it, and if you detach it, then if the thread is still running, it  
will continue to do so, and may continue running long after the `std::thread`  
object is destroyed; it will only stop running when it finally returns from  
the thread function.

If you don't wait for your thread to finish, you need to ensure that the data  
accessed by the thread is valid until the thread has finished with it.  
```cpp
struct func {
    int& i;
    func(int& i_) : i(i_) {}
    void operator() () {
        for (unsigned j = 0; j < 1000000; ++j) {
            do_something(i);
        }
    }
};
void oops() {
    int some_local_state = 0;
    func my_func(some_local_state);
    std::thread my_thread(my_func);
    my_thread.detach(); // don't wait for thread to finish
}  // new thread might still be running
```



