/**
 * Fundamentally, the only reason not to use concurrency is when the benefit
 *     isn't worth the cost.
 * Code using concurrency is harder to understand in many cases, so there's a
 *     direct intellectual cost to writing and maintaining multithreaded code,
 *     and the additional complexity can also lead to more bugs.
 * Unless
 *     the potential performance gain is large enough
 *         or
 *     the separation of concerns is clear enough
 *     to justfiy
 *     the additional development time required to get it right
 *         and
 *     the additional cost associated with maintaining multithreaded code,
 * don't use concurrency.
 * 
 * Also, the performance gain might note be as large as expected; there's an
 *     inherent overhead associated with launching a thread, because the OS has
 *     to allocate the associated kernel resources and stack space and then add
 *     the new thread to the scheduler, all of which takes time.
 * If the task being run on the thread is completed quickly, the time taken by
 *     the task may be dwarfed by the overhead of launching the thread, possibly
 *     making the overall performance of the application worse than if the task
 *     had been executed directly by the spawning thread.
 * The more threads you have running, the more context switching the OS has
 *     to do. Each context switch takes time that could be spent doing useful
 *     work, os at some point, adding an extra thread will reduce the overall
 *     application performance rather than increase it.
 * 
 * If you're after the utmost in performance, it's important to understand the
 *     implementation costs associated with using any high-level facilities,
 *     compared to using the underlying low-level APIs directly. This costs is
 *     the *abstraction penalty*.
 * 
*/


