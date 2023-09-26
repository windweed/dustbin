# 1 Sequence Erlang

## 1.1 Eshell

type `erl` to start the erlang-shell(eshell):

```shell
$ erl
Erlang/OTP 24 [erts-12.2.1] [source] [64-bit] [smp:16:16] [ds:16:16:10] [async-threads:1] [jit]

Eshell V12.2.1  (abort with ^G)
1>
```

exit eshell:

```
q(). %% shorthand of `init:stop().`
```

or

```
%% Ctrl + G, h, q
```

## 1.2 Numbers

```eshell
1> 1 + 2.
3
2> 2 - 3.
-1
3> 2 * 3.
6
4> 2 / 3.
0.6666666666666666
5> 5 div 2.
2
6> 5 rem 2.
1
7> 36#cafe + 2#100_1 + 8#35. %% base#value, base[2~36], underline allowed.
573424
8> $a - $\t.  %% $a for 97, $\t for 9
88
9> $\x12.
18
```

## 1.3 Variables

Variables begin with an uppercase letter by definition.  
Variables cannot be changed once it's bounded.

```eshell
11> SomeThing.
* 1:1: variable 'SomeThing' is unbound
12> SomeThing = 1.
1
13> SomeThing = 2.
** exception error: no match of right hand side value 2
14> A = B = SomeThing.
1
15> A.
1
16> B.
1
```

## 1.4 Atoms

Atoms are literals.  
> An atom should be enclosed in single quotes (') if it does not begin with
a lowercase letter or if it contains any characters other than alphanumeric
characters, an underscore (_), or an at sign (@).

```eshell
17> atom1.
atom1
18> atom2_underlineABC.
atom2_underlineABC
19> hello@world.
hello@world
20> 'Hello'.
'Hello'
21> 'Hello-$aaB Cand D'.
'Hello-$aaB Cand D'
22> atom1 = 'atom1'.
atom1
```

> An atom is referred to in an *atom table*, which consumes memory (4 bytes per atom in a 32-bit system and 8 bytes per atom in a 64-bit system).  
The atom table is not garbage collected

### 1.4.1 Reserved words

*begin, case, try, catch, cond, end, fun, if, let, of, when*  
*after, query, receive*  
*and, andalso, not, or, orelse, xor*  
*bnot, bor, bsl, bsr, bxor*  
*div, rem*  
*band*  

## 1.5 Boolean Algebra and Comparison Operators

```eshell
23> true xor false.
true
24> not (true and false).
true
25> 5 == 5.
true
26> 5 == 5.0.
true
27> 5 =:= 5.0.
false
28> a /= b.
true
29> a =/= b.
true
30> 1 < 2.
true
31> 1 > 2.
false
32> 1 >= 1.
true
33> 1 =< 1.
true
```

## 1.6 Tuples

A *tuple* is a way to group together a set number of terms.  

```eshell
36> Point = {1, 2}.
{1,2}
37> {X, Y} = Point.
{1,2}
38> X.
1
```

## 1.7 Lists

```eshell
39> A = [1, 2, 3, {numbers,[4,5,6]}, 5.34, atom].
[1,2,3,{numbers,[4,5,6]},5.34,atom]
40> [233].
"é"
41> [1,2,3] ++ [6,7].  %% append to tail
[1,2,3,6,7]
42> [1,2,1,4,5] -- [1]. %% remove form head.
[2,1,4,5]
45> [3,2,1] --[1,2] -- [2].  %% ++ and -- are right-associative
[3,2]
```

## 1.7.1 hd() and tl() and [H|T]

The first element of a list is named the *head*, and the reset of the list  
is named the *tail*.  
We can use two built-in functions to get them:

```eshell
51> A = [1,2,3,4,5].
[1,2,3,4,5]
52> hd(A).
1
53> tl(A).
[2,3,4,5]
```

Accessing or adding the head is fast and efficient.  
As it's used so frequently, Erlang provides an easier way to separate the head  
from the tail of a list, `[H|T]`:

```eshell
54> A = [1,2,3,4,5].
[1,2,3,4,5]
55> A1 = [3 | A].
[3,1,2,3,4,5]
56> [H|T] = A.
[1,2,3,4,5]
57> H.
1
58> T.
[2,3,4,5]
```

T may be a empty list. The `|` is called the *cons* operator.  
In fact, any list can be built with only cons operators and values:

```eshell
59> [1 | []].
[1]
60> [2 | [1 | []]].
[2,1]
61> [a, b | [c, d]].
[a,b,c,d]
```

### 1.7.2 List Comprehensions

```eshell
62> [2 * N || N <- [1,2,3,4]].
[2,4,6,8]
```

You can also add constraints to a list comprehension by using operations that  
return boolean values.

```eshell
63> [X || X <- lists:seq(1,10), X rem 2 =:= 0].
[2,4,6,8,10]
```

So, the recipe for list comprehensions in Erlang is as follows:

- - -
> NewList = [Expression || Pattern1 <- List1, ... PatternN <-ListN, Condition1, ... ConditionN]
- - -

The `Pattern <- List` part is called a *generator expression*.

```eshell
65> RestaurantMenu = [{steak, 5.99}, {beer, 3.99}, {poutine, 3.50}, {kitten, 20.99}, {water, 0.00}].
[{steak,5.99},
 {beer,3.99},
 {poutine,3.5},
 {kitten,20.99},
 {water,0.0}]
66> [{Item, Price * 1.07} || {Item, Price} <- RestaurantMenu, Price >= 3, Price =< 10].
[{steak,6.409300000000001},{beer,4.2693},{poutine,3.745}]

67> [X + Y || X <- [1,2], Y <- [3,4]].
[4,5,5,6]

68> Weather = [{toronto, rain}, {montreal, storms}, {london, fog}, {pairs, sun}, {boston, fog}, {vancouver, snow}].
[{toronto,rain},
 {montreal,storms},
 {london,fog},
 {pairs,sun},
 {boston,fog},
 {vancouver,snow}]
69> FoggyCities = [City || {City, fog} <- Weather].
[london,boston]
```

## 1.8 Binary Data

### 1.8.1 Bit Syntax

Erlang bit syntax encolses binary data between `<<` and `>>`, and splits it in  
readable segments;  
each segment is separated by a comma.  
A segment is a sequence of bits of a binary(not necessarily on a byte boundary,  
although this is the default behavior).

Suppose you want to store an orange pixel of true color(24bits, `#F09A29`):

```eshell
1> Color = 16#F09A29.
15768105
2> Pixel = <<Color:24>>.
<<240,154,41>> %% 0xF0, 0x9A, 0x29
```

This says, "Put the binay values of `#F09A29` on 24 bits of space in the Variable `Pixel`.  
That value can then be written to a file or a socket later.  

Pattern match with binaries:

```eshell
3> Pixels = <<213, 45, 132, 54, 76, 32, 76, 0, 0, 234, 32, 15>>.
 
<<213,45,132,54,76,32,76,0,0,
  234,32,15>>
4> <<Pix2, Pix2, Pix3, Pix4>> = Pixels.
** exception error: no match of right hand side value 
                    <<213,45,
                      132,54,
                      76,32,76,
                      0,0,234,
                      32,15>>
5> <<Pix1:24, Pix2:24, Pix3:24, Pix4:24>> = Pixels.
<<213,45,132,54,76,32,76,0,0,
  234,32,15>>
6> Pix1.
13970820 %% 0xD52D84: 0xD5->213, 0x2D->45, 0x84->132

7> <<R:8, G:8, B:8>> = Pix1.
** exception error: no match of right hand side value 13970820
8> <<R:8, G:8, B:8>> = <<Pix1:24>>.
<<213,45,132>>
9> R.
213
%% syntactic sugar, like [H|T] in list.
10> <<R:8, Rest/binary>> = Pixels.
<<213,45,132,54,76,32,76,0,0,
  234,32,15>>
```

The line 10 works because Erlang allows more than one way to describe a binary segment:

* *Value*
* *Value:Size*
* *Value/TypeSpecifierList*
* *Value:Size/TypeSpecifierList*

Here, *Size* is always in bits when no *TypeSpecifierList* is defined.  
*TypeSpecifierList* represents one or more of the following, seperated by  
a hyphen(-):

* Type  
    Represents the kind of binary data used.  
    *integer*, *float*, *binary(bytes)*, *bitstring(bits)*, *utf8*, *utf16*, *utf32*.  
    When no type is specified, Erlang assumes an *integer* type.
* Signedness  
    Only matters when the **Type** is *integer*, .  
    *signed*, *unsigned*.
    The default is *unsigned*.
* Endianness  
    Only matters when the **Type** is *integer*, *utf16*, *utf32*, *float*.  
    *big*, *little*, *native*.  
    Default is *big*(network protocol encodings).
* Unit**
    This is written as *unit:Integer*.  
    The unit is the size of each segment. Ranges 1 to 256.  
    Default: 1 bit for *integer*, *float*, *bitstring*; 8 bits for *binary*.  
    The multiplication of size by unit is equal to the number of bits the  
       segment will take, and must be evenly divisible by 8.  
    The unit size is usually used to ensure byte alignment.
    `<<25:4/uint:8>>`, `<<25:2/uint:16>>`, `<<25:1/uint:32>>` -> `<<0,0,0,25>>`.
    Erlang will generally accept `<<25:Size/uint:Unit>>` and multiply `Size`  
      by `Unit` to figure out how much space it should take to represent the  
      value. Again, the result of this should be divisible by 8.

Some examples:

```eshell
20> <<X1/unsigned>> = <<-44>>.
<<"Ô">>
21> X1.
212
22> <<X2/signed>> = <<-44>>.
<<"Ô">>
23> X2.
-44
```

Another example, Pack a 16-bits RGB data into `Mem`:
```eshell
1> Red = 2.
2
2> Green = 61.
61
3> Blue = 20.
20
4> Mem = << Red:5, Green:6, Blue:5>>.
<<23,180>>
```

Why it's `<<23,180>>` ?

`2 => (5bits) 00010`;  
`61 => (6bits) 111101`;  
`20 => (5bits) 10100`;  

Combine them: `0001_0111_1011_0100`. => `0x17, 0xB4` => `23, 180`

Unpack:

```eshell
6> Mem = << 2:5, 61:6, 20:5>>.
<<23,180>>
7> <<R:5, G:6, B:5>> = Mem.
<<23,180>>
8> R.
2
```
