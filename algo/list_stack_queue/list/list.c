#include "list.h"
#include <stdlib.h>

struct Node
{
    ElementType Element;
    Position    Next;
};

// return true if L is empty
int
IsEmpty(List L)
{
    return L->Next == NULL;
}

int
IsLast(Position P, List L)
{
    return P->Next == NULL;
}

Position
Find(ElementType X, List L)
{
    Position P;
    P = L->Next;

    while (P != NULL && P->Element != X) {
        P = P->Next;
    }
    return P;
}

/**
 * @brief Delete first occurence of X from a list
 * @note Assume use of a header node
*/
void
Delete(ElementType X, List L)
{
    Position P, TmpCell;
    P = FindPrevious(X, L);

    if (!IsLast(P, L)) { // Assumption of header use
        TmpCell = P->Next;
        P->Next = TmpCell->Next;
        free(TmpCell);
    }
}
