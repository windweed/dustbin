本文描述了如何使用C语言模拟继承和多态，以更好地理解c++中的对应概念。

# 封装

首先来看一下封装。

直接来一段代码

```C
// @file: shape.h

#ifndef SHAPE_INCLUDE
#define SHAPE_INCLUDE

#include <stdint.h>

typedef struct {
    int x;
    int y;
} Shape;

// interface
void shapeInit(Shape * const this, int x, int y);
void shapeMoveBy(Shape * const this, int dx, int dy);
int shapeGetX(Shape const * const this);
int shapeGetY(Shape const * const this);

#endif // SHAPE_INCLUDE
```

这样就完成了封装。下面是`.c`文件

```C
// @file: shape.c
#include "shape.h"

void shapeInit(Shape * const this, int x, int y) {
    this->x = x;
    this->y = y;
}

void shapeMoveBy(Shape * const this, int dx, int dy) {
    this->x += dx;
    this->y += dy;
}

int shapeGetX(Shape const * const this) {
    return this->x;
}

int shapeGetY(Shape const * const this) {
    return this->y;
}

```

# 继承

继承就是基于现有的一个类去定义一个新类。\
在C中，实现单继承，只要把基类放到子类的第一个数据成员的位置即可。

我们现在创建一个`Rectangle`类，我们只要继承Shape类已经存在的属性和操作，再添加自己的属性和操作即可。

```C
#ifndef RECTANGLE_INCLUDE
#define RECTANGLE_INCLUDE

// @file: rectangle.h

#include "shape.h"

typedef struct {
    Shape super;
    int width;
    int height;
} Rectangle;

void rectangleInit(Rectangle * const this, int x, int y, int width, int height);

#endif // RECTANGLE_INCLUDE
```

```C
#include "rectangle.h"

void rectangleInit(Rectangle * const this, int x, int y, int width, int height) {
    // first call superclass's constructor
    shapeInit(&this->super, x, y);

    this->width = width;
    this->height = height;
}
```

由于Shape位于Rectangle的第一个元素的位置，因此，可以传一个指向Rectangle的指针到一个\
期望传入Shape指针的函数中。

# 多态

现在，我们要增加一个圆形，并且扩展Shape的功能，增加`area`函数和`draw`函数。\
但是，Shape相当于抽象类，不知道如何去计算自己的面积。

下面，我们修改一个Shape类的定义，给它加上一个虚表：

```C
// @file: shape.h

#ifndef SHAPE_INCLUDE
#define SHAPE_INCLUDE

#include <stdint.h>

struct ShapeVtbl;

typedef struct {
    struct ShapeVtbl const *vptr;
    int x;
    int y;
} Shape;

struct ShapeVtbl {
    int (*area)(Shape const * const this);
    void (*draw)(Shape const * const this);
};

// interface
void shapeInit(Shape * const this, int x, int y);
void shapeMoveBy(Shape * const this, int dx, int dy);
int shapeGetX(Shape const * const this);
int shapeGetY(Shape const * const this);

static inline int shapeArea(Shape const * const this) {
    return (*this->vptr->area)(this);
}

static inline void shapeDraw(Shape const * const this) {
    (*this->vptr->draw)(this);
}

#endif // SHAPE_INCLUDE
```

如上所示，虚表是这个类所有虚函数的函数指针的集合。\
虚指针是一个指向虚表的指针，这个虚指针必须存在于每个对象实例中，会被所有子类继承。

一般的做法是，在构造函数中设置vptr。\
在每个对象实例中，vptr必须被初始化指向其vtbl。\
最好的初始化位置就是在类的构造函数中。

下面展示一下，在Shape的构造函数里初始化vptr。

```C
// @file: shape.c
#include "shape.h"
#include <assert.h>

// Shape的虚函数
static int shapeArea_(Shape const * const this);
static void shapeDraw_(Shape const * const this);

void shapeInit(Shape * const this, int x, int y) {
    static struct ShapeVtbl const vtbl = {
        .area = &shapeArea_,
        .draw = &shapeDraw_,
    };
    this->vptr = &vtbl;

    this->x = x;
    this->y = y;
}

void shapeMoveBy(Shape * const this, int dx, int dy) {
    this->x += dx;
    this->y += dy;
}

int shapeGetX(Shape const * const this) {
    return this->x;
}

int shapeGetY(Shape const * const this) {
    return this->y;
}

// Shape的虚函数
static int shapeArea_(Shape const * const this) {
    assert(0); // 禁止调用
    return 0; // 避免告警
}

static void shapeDraw_(Shape const * const this) {
    assert(0);
}
```

这个新的Shape被继承后，这个vptr需要被子类的虚表重新赋值。

下面修改 Rectangle 的构造函数

```C
#include "rectangle.h"
#include <stdio.h>

// rectangle的虚函数
static int rectangleArea_(Shape const * const this);
static void rectangleDraw_(Shape const * const this);

void rectangleInit(Rectangle * const this, int x, int y, int width, int height) {
    static struct ShapeVtbl const vtbl = {
        .area = &rectangleArea_,
        .draw = &rectangleDraw_,
    };
    shapeInit(&this->super, x, y);

    this->super.vptr = &vtbl;

    this->width = width;
    this->height = height;
}

// rectangle的虚函数
static int rectangleArea_(Shape const * const this) {
    Rectangle const * const rect = (Rectangle const *)this;
    return rect->width * rect->height;
}

static void rectangleDraw_(Shape const * const this) {
    Rectangle const * const rect = (Rectangle const *)this;
    printf("Rectange drawed at (%d,%d),width=%d,height=%d.\n",
        shapeGetX(this), shapeGetY(this), rect->width, rect->height);
}
```

Circle 的代码就略去了
