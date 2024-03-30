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
    void operator()() {
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

## 1.1.2 Waiting for a thread to complete

`join()` is a simple and brute-force technique -- either you wait for a thread  
to finish or you don't. If you need more fine-grained control over waiting for  
a thread, such as to check whether a thread is finished, or to wait only a  
certain period of time, then you have to use alternative mechanisms such as  
*condition variables* and *futures*, which we'll look at in chapter 4. The act  
of calling `join()` also cleans up any storage associated with the thread, so  
the `std::thread` object is no longer associated with the now-finished thread;  
it isn't associated with any thread. This means that you can call `join()` only  
once for a given thread; once you've called `join()`, the `std::thread` object  
is no longer *joinable*, and `joinable()` will return *false*.

## 1.1.3 Waiting in exceptional circumstances

As mentioned earlier, you need to ensure that you've called either `join()` or  
`detach()` before a `std::thread` object is destroyed. If you're detaching a  
thread, you can usually call `detach()` immediately after the thread has been  
started, so this isn't a problem. But if you're intending to wait for the  
thread, you need to carefully pick the place in the code where you call `join()`.  
This means that the call to `join()` is liable to be skipped if an exception is  
thrown after the thread has been started but before the call to `join()`.

To avoid your application being terminated when an exception is shown, you  
therefore need to make a decision about what to do in this case. In general, if  
you were intending to call `join()` in a non-exceptional case, you also need to  
call `join()` in the presence of an exception to avoid accidental lifetime  
problems. The next listing shows some simple code that does just that.
```cpp
// Listing 2.2 Waiting for a thread to finish
struct func;
void f() {
    int local_state = 0;
    func my_func(local_state);
    std::thread t(my_func);
    try {
        do_something_in_current_thread();
    } catch(...) {
        t.join();
        throw;
    }
    t.join();
}
```
The use of `try/catch` blocks is verbose, and it's easy to get the scope slightly  
wrong, so this sin't an ideal scenario.  

A better mechanism is using the standard *Resource Acquisition Is Initialization*  
(RAII) idiom and provide a class that does the `join()` in its destructor, as in  
the following listing. See how it simplifies the `f()` function.
```cpp
// Listing 2.3 Using RAII to wait for a thread to complete
class thread_guard {
    std::thread& t;
public:
    explicit thread_guard(std::thread& t_) : t(t_) {}
    ~thread_guard() {
        if (t.joinable()) {
            t.join();
        }
    }
    thread_guard(thread_guard const&) = delete;
    thread_guard& operator=(thread_guard const&) = delete;
};
struct func;
void f() {
    int some_local_state = 0;
    func my_func(some_local_state);
    std::thread t(my_func);
    thread_guard g(t);
    do_something_in_current_thread();
}
```
When the execution of the current thread reaches the end of `f()`, the local  
objects are destroyed in reverse order of construction. Consequently, the  
`thread_guard` object, `g`, is destroyed first, and the thread is joined with,  
in the destructor. This even happens if the function exits because  
`do_something_in_current_thread` throws an exception.

If you don't need to wait for a thread to finish, you can avoid this  
exception-safety issue by *`detaching`* it. This breaks the association of the  
thread with the `std::thread` object and ensures that `std::terminate()` won't  
be called when the `std::thread` object is destroyed, even though the thread is  
still running in the background.

## 1.1.4 Running threads in the background

Calling `detach()` on a `std::thread` object leaves the thread to run in the  
background, with no direct means of communicating with it. It's no longer  
possible to wait for that thread to complete; if a thread becomes detached, it  
isn't possible to obtain a `std::thread` object that references it, so ti can  
no longer be joined. Detached threads truly run in the background; ownership  
and control are passed over to the **C++ Runtime Library**, which ensures that  
the resources associated with the thread are corrently reclaimed when the  
thread exits.

Detached threads are often called *`daemon threads`* after the UNIX concept of  
a *`daemon process`* that runs in the background without any explicit user  
interface. Such threads are typically long-running; they run for almost the  
entire lifetime of the application, performing a background task such as  
monitoring the filesystem, clearing unused entries out of object caches, or  
optimizing data structures. At the other extreme, it may make sense to use a  
detached thread where there's another mechanism for identifying when the thread  
has completed or where the thread is used for a fire-and-forget task.

You can only call `t.detach()` for a `std::thread` object `t` when `t.joinable()`  
returns `true`.

Consider an application such as a word processor that can edit multiple  
documents at once. There are many ways to handle this, both at the UI level  
and iternally. One way to handle this internally is run each document-editing  
window in its own thread; each thread runs the same code but with different data  
relating to the document being edited and the corresponding window properties.  
Opening a new document therefore requires starting a new thread.
```cpp
// Listing 2.4 Detaching a thread to handle other documents
void edit_document(std::string const& filename) {
    open_document_and_display_gui(filename);
    while (!done_editing()) {
        user_command cmd = get_user_input();
        if (cmd.type == open_new_document) {
            std::string const new_name = get_filename_from_user();
            std::thread t(edit_document, new_name); // pass parameter to thread constructor
            t.detach();
        } else {
            process_user_input(cmd);
        }
    }
}
```
