# 1.3 Transferring ownership of a thread

Suppose you want to write a function that creates a thead to run in the  
background, but passes ownership of the new thread back to the calling function  
rather than waiting for it to complete; or the reverse: create a thread and  
pass ownership in to some function that should wait for it to complete. In  
either case, you need to transfer ownership from one place to another.

This is where the move support of `std::thread` comes in. As described in the  
previous section, many resource-owning types in the C++ Standard Library, such  
as `ifstream`, `unique_ptr`, `thread`, are movable but not copyable.  
```cpp
void some_function();
void some_other_function();
thread t1(some_function); // a new thread is started and associated with t1
thread t2 = move(t1);     // transferred over to t2
t1 = thread(some_other_function); // associated with a temporary thread object
// transferred from temporaries to t1. and doesn't require `move`,
// because moving from temporaries is automatic and implicit.

thread t3;
t3 = move(t2); // required `move` because t2 is a named object
t1 = move(t3); // this assignment will terminate the program
```
The final move transfers ownership of the thread running `some_function` back to  
`t1`. But in this case `t1` already had an associated thread (some_other_function),  
so `std::terminate()` is called to terminate the program.

This is done for consistency with the `thread` destructor. As mentioned above,  
you must wait for a thread to complete or detach it before destruction.  
and the same applies to assignment:  
**you can't just drop a thread by assigning a new value to the `thread` object**  
**that manages it**.

The move support in `thread` means that ownership can readily be transferred  
out of a function:
```cpp
// Listing 2.5 Returning a `std::thread` from a function
thread f() {
    void some_function();
    return thread(some_function);
}
thread g() {
    void some_other_function(int);
    thread t(some_other_function, 42);
    return t;
}
```
Likewise, if ownership should be transferred into a function, it can accept an  
instance of `thread` by **value** as one of the parameters:
```cpp
void f(thread t);
void g() {
    void some_function();
    f(thread(some_function));
    thread t(some_function);
    f(move(t));
}
```
One benefit of the move support of `thread` is that you can build on the  
`thread_guard` class from listing 2.3 and have it take ownership of the thread.
```cpp
// listing 2.6 scoped_thread and example usage
class scoped_thread {
    thread t;
public:
    explicit scoped_thread(thread t_) : t(move(t_)) {
        if (!t.joinable()) {
            throw std::logic_error("No thread");
        }
    }
    ~scoped_thread() {
        t.join();
    }
    scoped_thread(scoped_thread const&) = delete;
    scoped_thread& operator=(scoped_thread const&) = delete;
};
struct func;
void f() {
    int some_local_state;
    scoped_thread t {thread(func(some_local_state))};
    do_something_in_current_thread();
}
```
The example is similar to listing 2.3, but the new thread is passed in  
directly to `scoped_thread` rather than having to create a separate named  
variable for it. 

One of the proposals for C++17 was for a `joining_thread` class that would be  
similar to `thread`, except that it would automatically join in the  
destructor much like `scoped_thread` does. This didn't get consensus in the  
committee, so it wasn't accepted into the standard(though it's still on track  
for C++20 as `std::jthread`), but it's relatively easy to write.  
One possible implementation is shown in the next listing.
```cpp
// Listing 2.7 A `joining_thread` class
class joining_thread {
    std::thread t;
public:
    joining_thread() noexcept = default;
    template<typename Callable, typename ... Args>
    explicit joining_thread(Callable&& func, Args&& ... args) :
        t(std::forward<Callable>(func), std::forward<Args>(args)...)
    {}
    explicit joining_thread(std::thread t_) noexcept :
        t(std::move(t_))
    {}
    joining_thread(joining_thread&& other) noexcept:
        t(std::move(other.t))
    {}
    joining_thread& operator=(joining_thread&& other) noexcept {
        if (joinable()) {
            join();
        }
        t = std::move(other.t);
        return *this;
    }
    joining_thread& operator=(std::thread other) noexcept {
        if (joinable()) join();

        t = std::move(other);
        return *this;
    }
    ~joining_thread() noexcept { if (joinable()) join(); }
    void swap(joining_thread& other) noexcept { t.swap(other.t); }
    std::thread::id get_id() const noexcept { return t.get_id(); }
    bool joinable() const noexcept { return t.joinable(); }
    void join() { t.join(); }
    void detach() { t.detach(); }
    std::thread& as_thread() noexcept { return t; }
    const std::thread& as_thread() const noexcept { return t; }
};
```
