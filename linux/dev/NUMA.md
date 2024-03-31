# NUMA

`$ yum install numactl`

NUMA(Non-Uniform Memory Access, 非一致性内存访问)  
和  
SMP(Symmetric Multi-Processor, 对称多处理器系统)  
是两种不同的CPU硬件体系架构。

SMP的主要特征是共享，所有的CPU共享使用全部资源，例如内存、总线和I/O，多个CPU对称工作，彼此之间没有主次之分，平等地访问共享的资源，这样势必引入资源的竞争问题，从而导致它的扩展内力非常有限；NUMA架构在中大型系统上一直非常盛行，也是高性能的解决方案，在系统延迟方面表现也都很优秀。

在NUMA架构下，CPU的概念从大到小依次是：Socket、Core、Processor。随着多核技术的发展，我们将多个CPU封装在一起，这个封装一般被称为Socket（插槽），而Socket中的每个核心被称为Core，为了进一步提升CPU的处理能力，Intel又引入了HT（Hyper-Threading，超线程）的技术，一个Core打开HT之后，在OS看来就是两个核，当然这个核是逻辑上的概念，所以也被称为Logical Processor，本文简称为Processor。



