#ifndef INT_HEAP_H
#define INT_HEAP_H

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <string.h>

static inline bool gt(int a, int b) { return a > b; }
static inline bool lt(int a, int b) { return a < b; }

enum heap_type
{
    MAX = 1,
    MIN = 2,
};

struct int_heap_s
{
    int *d;
    size_t size;
    bool (*need_swap)(int, int);  // lt for MAX, gt for MIN
};

typedef struct int_heap_s int_heap_t;

int_heap_t * int_heap_new(size_t num_of_eles, enum heap_type);
bool         int_heap_empty(int_heap_t *);
size_t       int_heap_size(int_heap_t *);
void         int_heap_push(int_heap_t *, int);
void         int_heap_pop(int_heap_t *);
int          int_heap_top(int_heap_t *);
void         int_heap_percolate_down(int_heap_t *, size_t idx);
void         int_heap_build(int_heap_t *, int *input, size_t input_size);


int_heap_t *
int_heap_new(size_t num_of_eles, enum heap_type t)
{
    int_heap_t *h = (int_heap_t*) malloc(sizeof(int_heap_t));
    if (!h) {
        return NULL;
    }
    h->d = (int*) malloc(sizeof(int) * (1 + num_of_eles));
    if (!h) {
        return NULL;
    }
    h->size = 0u;
    h->d[0] = t == MAX ? INT32_MAX : INT32_MIN;
    h->need_swap = t == MAX ? lt : gt;
    return h;
}

void
int_heap_push(int_heap_t *h, int val)
{
    size_t i = ++h->size;  // make a hole
    for(; h->need_swap(h->d[i / 2], val); i /= 2) {
        h->d[i] = h->d[i / 2];  // hole goes up
    }
    h->d[i] = val;
}

/**
 * @param p_idx parent index
*/
void
int_heap_percolate_down(int_heap_t *h, size_t p_idx)
{
    int hole_val = h->d[p_idx];
    int c_idx = 1;  // children index
    for(; p_idx * 2 <= h->size; p_idx = c_idx) {  // p_idx goes down
        c_idx = p_idx * 2;
        if (c_idx < h->size && h->need_swap(h->d[c_idx], h->d[c_idx + 1])) {
            c_idx++;
        }
        if (!h->need_swap(hole_val, h->d[c_idx])) {
            break;
        }
        h->d[p_idx] = h->d[c_idx];
    }
    h->d[p_idx] = hole_val;
}

void
int_heap_pop(int_heap_t *h)
{
    h->d[1] = h->d[h->size];
    h->size--;
    int_heap_percolate_down(h, 1);
}

bool
int_heap_empty(int_heap_t *h)
{
    return h->size == 0;
}

int
int_heap_top(int_heap_t *h)
{
    return h->d[1];
}

size_t
int_heap_size(int_heap_t *h)
{
    return h->size;
}

void
int_heap_build(int_heap_t *h, int *input, size_t input_size)
{
    memcpy(&h->d[1], input, input_size * sizeof(int));
    h->size = input_size;
    for (int i = input_size / 2; i > 0; i--) {
        int_heap_percolate_down(h, i);  // all no-leaf nodes
    }
}


#endif  // INT_HEAP_H
