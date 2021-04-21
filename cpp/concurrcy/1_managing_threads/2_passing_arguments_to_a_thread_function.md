# 1.2 Passing arguments to a thread function

As shown in listing 2.4, passing arguments to the callable object of function  
is as simple as passing additional arguments to the `std::thread` constructor.

But, remember that by default, the arguments are copied into internal stroage,  
where they can be accessed by the newly created thread of execution, and then  
passed to the callable object or function as rvalues as if they were temporaries.  
This is done even if the corresponding parameter in the function is expecting a  
reference:
```cpp
void f(int i, std::thread const& s);
std::thread t(f, 3, "hello");
```
This creates a new thread of execution associated with `t`, which calls  
`f(3, "hello")`. Note that even though `f` takes a `string` as the second  
parameter, the string literal is passed as a `char const*` and converted to a  
`string` only in the context of the new thread. This is particularly important  
when the argument supplied is a pointer to an automatic variable, as follows:
```cpp
void f(int i, string const& s);
void oops(int some_param) {
    char buffer[1024];
    sprintf(buffer, "%i", some_param);
    thread t(f, 3, buffer);
    t.detach();
}
```
In this case, it's the pointer to the local variable `buffer` that's passed  
through to the new thread and there's a significant chance that the `oops`  
function will exit before the buffer has been converted to a `string` on the  
new thread, thus leading to undefined behavior. The solution is to cast to  
`string` before passing the buffer to the `thread` constructor:
```cpp
thread t(f, 3, string(buffer));
```
In this case, the problem is that you were relying on the implicit conversion  
of the pointer to the buffer into the `string` object expected as a function  
parameter, but this conversion happens too late because the `thread` constructor  
copies the supplied values as is, without converting to the expected argument type.

It's not possible to get the reverse scenario: the object is copied, and you  
wanted a not-const reference, because this won't compile. You might try this if  
the thread is updating a data structure that's passed in by reference; e.g.:
```cpp
void update_data_for_widget(widget_id w, widget_data& data);
void oops_again(widget_id w) {
    widget_data data;
    thread t(update_data_for_widget, w, data);
    display_status();
    t.join();
    process_widget_data(data);
}
```
Although `update_data_for_widget` expects the second parameter to be passed by  
reference, the `thread` constructor doesn't know that; it's oblivious to the  
types of the arguments expected by the function and blindly copies the suppied  
values. But the internal code passes copied arguments as rvalues in order to  
work with move-only types, and will thus try to call `update_data_for_widget`  
with an rvalue. This will fail to compile because you can't pass an rvalue to  
a function that expects a not-const reference. For those of you familiar with  
`std::bind` the solution will be readily apparent: you need to wrap the  
arguments that need to be references in `std::ref`. In this case, if you change  
the thread invocation to  
```cpp
thread t(update_data_for_widget, w, std::ref(data));
```
then `update_data_for_widget` will be correctly passed a reference to `data`  
rather than a temporary copy of `data`, and the code will compile successfully.

If you;re familiar with `std::bind`, the parameter-passing semantics will be  
unsurprising, because both the operation of the `thread` constructor and the  
operation of `std::bind` are defined in terms of the same mechanism. This means  
that, for example, you can pass a member function pointer as the function,  
provided you supply a suitable object pointer as the first argument:
```cpp
class X { public: void do_lengthy_work(); };
X my_x;
thread t(&X::do_lengthy_work, &my_x);
```
This code will invoke `my_x.do_lengthy_work()` on the new thread, because the  
address of `my_x` is supplied as the object pointer. You can also supply  
arguments to such a member function call: the third argument to the `thread`  
constructor will be the first argument to the member function, and so forth.

Another scenario for supplying arguments is where the arguments can't be copied  
but can only be *moved*: the data held within one object is transferred over  
to another, leaving the original object empty. This moving of values allows  
objects of this type to be accepted as function parameters or returned from  
functions. Where the source object is temporary, the move is automatic, but  
where the source is a named value, the transfer must be requested directly by  
invoking `std::move()`.
```cpp
// the use of `move` to transfer ownership of a dynamic object into a thread
void process_big_object(std::unique_ptr<big_object>);
unique_ptr<big_object> p(new big_object);
p->prepare_data(42);
thread t(process_big_object, std::move(p));
```
Several of the classes in the C++ Standard Library exhibit the same ownership  
semantics as `std::unique_ptr`, and `std::thread` is one of them.
