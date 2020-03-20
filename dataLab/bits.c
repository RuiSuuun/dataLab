/* 
 * CS:APP Data Lab 
 * 
 * <Please put your name and userid here>
 * Rui Sun, ruisun@andrew.cmu.edu
 * bits.c - Source file with your solutions to the Lab.
 *          This is the file you will hand in to your instructor.
 *
 * WARNING: Do not include the <stdio.h> header; it confuses the dlc
 * compiler. You can still use printf for debugging without including
 * <stdio.h>, although you might get a compiler warning. In general,
 * it's not good practice to ignore compiler warnings, but in this
 * case it's OK.  
 */

#if 0
/*
 * Instructions to Students:
 *
 * STEP 1: Read the following instructions carefully.
 */

You will provide your solution to the Data Lab by
editing the collection of functions in this source file.

INTEGER CODING RULES:
 
  Replace the "return" statement in each function with one
  or more lines of C code that implements the function. Your code 
  must conform to the following style:
 
  long Funct(long arg1, long arg2, ...) {
      /* brief description of how your implementation works */
      long var1 = Expr1;
      ...
      long varM = ExprM;

      varJ = ExprJ;
      ...
      varN = ExprN;
      return ExprR;
  }

  Each "Expr" is an expression using ONLY the following:
  1. (Long) integer constants 0 through 255 (0xFFL), inclusive. You are
      not allowed to use big constants such as 0xffffffffL.
  3. Function arguments and local variables (no global variables).
  4. Local variables of type int and long
  5. Unary integer operations ! ~
     - Their arguments can have types int or long
     - Note that ! always returns int, even if the argument is long
  6. Binary integer operations & ^ | + << >>
     - Their arguments can have types int or long
  7. Casting from int to long and from long to int
    
  Some of the problems restrict the set of allowed operators even further.
  Each "Expr" may consist of multiple operators. You are not restricted to
  one operator per line.

  You are expressly forbidden to:
  1. Use any control constructs such as if, do, while, for, switch, etc.
  2. Define or use any macros.
  3. Define any additional functions in this file.
  4. Call any functions.
  5. Use any other operations, such as &&, ||, -, or ?:
  6. Use any form of casting other than between int and long.
  7. Use any data type other than int or long.  This implies that you
     cannot use arrays, structs, or unions.
 
  You may assume that your machine:
  1. Uses 2s complement representations for int and long.
  2. Data type int is 32 bits, long is 64.
  3. Performs right shifts arithmetically.
  4. Has unpredictable behavior when shifting if the shift amount
     is less than 0 or greater than 31 (int) or 63 (long)

EXAMPLES OF ACCEPTABLE CODING STYLE:
  /*
   * pow2plus1 - returns 2^x + 1, where 0 <= x <= 63
   */
  long pow2plus1(long x) {
     /* exploit ability of shifts to compute powers of 2 */
     /* Note that the 'L' indicates a long constant */
     return (1L << x) + 1L;
  }

  /*
   * pow2plus4 - returns 2^x + 4, where 0 <= x <= 63
   */
  long pow2plus4(long x) {
     /* exploit ability of shifts to compute powers of 2 */
     long result = (1L << x);
     result += 4L;
     return result;
  }

FLOATING POINT CODING RULES

For the problems that require you to implement floating-point operations,
the coding rules are less strict.  You are allowed to use looping and
conditional control.  You are allowed to use both ints and unsigneds.
You can use arbitrary integer and unsigned constants. You can use any arithmetic,
logical, or comparison operations on int or unsigned data.

You are expressly forbidden to:
  1. Define or use any macros.
  2. Define any additional functions in this file.
  3. Call any functions.
  4. Use any form of casting.
  5. Use any data type other than int or unsigned.  This means that you
     cannot use arrays, structs, or unions.
  6. Use any floating point data types, operations, or constants.


NOTES:
  1. Use the dlc (data lab checker) compiler (described in the handout) to 
     check the legality of your solutions.
  2. Each function has a maximum number of operations (integer, logical,
     or comparison) that you are allowed to use for your implementation
     of the function.  The max operator count is checked by dlc.
     Note that assignment ('=') is not counted; you may use as many of
     these as you want without penalty.
  3. Use the btest test harness to check your functions for correctness.
  4. Use the BDD checker to formally verify your functions
  5. The maximum number of ops for each function is given in the
     header comment for each function. If there are any inconsistencies 
     between the maximum ops in the writeup and in this file, consider
     this file the authoritative source.

/*
 * STEP 2: Modify the following functions according the coding rules.
 * 
 *   IMPORTANT. TO AVOID GRADING SURPRISES:
 *   1. Use the dlc compiler to check that your solutions conform
 *      to the coding rules.
 *   2. Use the BDD checker to formally verify that your solutions produce 
 *      the correct answers.
 */


#endif
/* Copyright (C) 1991-2012 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* We do support the IEC 559 math functionality, real and complex.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
//1
/*
 * bitMatch - Create mask indicating which bits in x match those in y
 *            using only ~ and &
 *   Example: bitMatch(0x7L, 0xEL) = 0xFFFFFFFFFFFFFFF6L
 *   Legal ops: ~ &

 *   Max ops: 14
 *   Rating: 1
 */
long bitMatch(long x, long y) {
  return ~(~x&y)&~(x&~y);
}
//2
/* 
 * copyLSB - set all bits of result to least significant bit of x
 *   Examples:
 *     copyLSB(5L) = 0xFFFFFFFFFFFFFFFFL,
 *     copyLSB(6L) = 0x0000000000000000L
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
long copyLSB(long x) {
  return x << 63 >> 63;
}
/*
 * distinctNegation - returns 1 if x != -x.
 *     and 0 otherwise 
 *   Legal ops: ! ~ & ^ | +
 *   Max ops: 5
 *   Rating: 2
 */
long distinctNegation(long x) {
  return !!(x^(~x + 1));
}
/* 
 * leastBitPos - return a mask that marks the position of the
 *               least significant 1 bit. If x == 0, return 0
 *   Example: leastBitPos(96L) = 0x20L
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2 
 */
long leastBitPos(long x) {
  return (x ^ (x + ~0)) & x;
}
/* 
 * dividePower2 - Compute x/(2^n), for 0 <= n <= 62
 *  Round toward zero
 *   Examples: dividePower2(15L,1L) = 7L, dividePower2(-33L,4L) = -2L
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 2
 */
long dividePower2(long x, long n) {
    return (x + (x >> 63 & ((1L << n) + ~0))) >> n;
}
//3
/* 
 * conditional - same as x ? y : z 
 *   Example: conditional(2,4L,5L) = 4L
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
long conditional(long x, long y, long z) {
  x = (long) (!x) << 63 >> 63;
  return (~x & y) | (x & z);
}
/* 
 * isLessOrEqual - if x <= y  then return 1, else return 0 
 *   Example: isLessOrEqual(4L,5L) = 1L.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
long isLessOrEqual(long x, long y) {
  long sign1 = (x ^ y) >> 63;
  return (long) !!((~(sign1 | ((y + ~x + 1) >> 63))) | (sign1 & ~(y >> 63)));
}
//4
/*
 * trueThreeFourths - multiplies by 3/4 rounding toward 0,
 *   avoiding errors due to overflow
 *   Examples:
 *    trueThreeFourths(11L) = 8
 *    trueThreeFourths(-9L) = -6
 *    trueThreeFourths(4611686018427387904L) = 3458764513820540928L (no overflow)
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
long trueThreeFourths(long x) {
  long quar = x >> 2;
  long rema = x & 0x3;
  return ((quar << 1) + quar) + (((rema << 1) + rema + ((x >> 63) & 0x3)) >> 2);
}
/*
 * bitCount - returns count of number of 1's in word
 *   Examples: bitCount(5L) = 2, bitCount(7L) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 50
 *   Rating: 4
 */
long bitCount(long x) {
  long mask1;
  long mask2;
  long mask4 = 0x0f;
  long mask8 = 0xff;
  long mask16;
  long mask32;
  mask4 = (mask4 << 8) | mask4;
  mask4 = (mask4 << 16) | mask4;
  mask4 = (mask4 << 32) | mask4;
  mask2 = mask4 ^ (mask4 << 2);
  mask1 = mask2 ^ (mask2 << 1);
  mask16 = (mask8 << 8) | mask8;
  mask8 = (mask8 << 16) | mask8;
  mask8 = (mask8 << 32) | mask8;
  mask32 = (mask16 << 16) | mask16;
  mask16 = (mask16 << 32) | mask16;

  x = (x&mask1) + ((x>>1)&mask1);
  x = (x&mask2) + ((x>>2)&mask2);
  x = (x&mask4) + ((x>>4)&mask4);
  x = (x&mask8) + ((x>>8)&mask8);
  x = (x&mask16) + ((x>>16)&mask16);
  x = (x&mask32) + ((x>>32)&mask32);
  return x;
}
/*
 * isPalindrome - Return 1 if bit pattern in x is equal to its mirror image
 *   Example: isPalindrome(0x6F0F0123c480F0F6L) = 1L
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 70
 *   Rating: 4
 */
long isPalindrome(long x) {
  long mirror = x;
  long mask1;
  long mask2;
  long mask4 = 0x0f;
  long mask8 = 0xff;
  long mask16;
  long mask32;
  mask4 = (mask4 << 8) | mask4;
  mask4 = (mask4 << 16) | mask4;
  mask4 = (mask4 << 32) | mask4;
  mask2 = mask4 ^ (mask4 << 2);
  mask1 = mask2 ^ (mask2 << 1);
  mask16 = (mask8 << 8) | mask8;
  mask8 = (mask8 << 16) | mask8;
  mask8 = (mask8 << 32) | mask8;
  mask32 = (mask16 << 16) | mask16;
  mask16 = (mask16 << 32) | mask16;
  x = ((x & mask1) << 1) + ((x >> 1) & mask1);
  x = ((x & mask2) << 2) + ((x >> 2) & mask2);
  x = ((x & mask4) << 4) + ((x >> 4) & mask4);
  x = ((x & mask8) << 8) + ((x >> 8) & mask8);
  x = ((x & mask16) << 16) + ((x >> 16) & mask16);
  x = ((x & mask32) << 32) + ((x >> 32) & mask32);
  return (long) !(mirror ^ x);
}
/*
 * integerLog2 - return floor(log base 2 of x), where x > 0
 *   Example: integerLog2(16L) = 4L, integerLog2(31L) = 4L
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 60
 *   Rating: 4
 */
long integerLog2(long x) {
  long n = 0;
  n += (((long) !!(x&((~0L)<<(n+32)))) << 5);
  n += (((long) !!(x&((~0L)<<(n+16)))) << 4);
  n += (((long) !!(x&((~0L)<<(n+8)))) << 3);
  n += (((long) !!(x&((~0L)<<(n+4)))) << 2);
  n += (((long) !!(x&((~0L)<<(n+2)))) << 1);
  n += ((long) !!(x&((~0L)<<(n+1))));
  return n;
}
//float
/* 
 * floatIsEqual - Compute f == g for floating point arguments f and g.
 *   Both the arguments are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representations of
 *   single-precision floating point values.
 *   If either argument is NaN, return 0.
 *   +0 and -0 are considered equal.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 25
 *   Rating: 2
 */
int floatIsEqual(unsigned uf, unsigned ug) {
  if (!((uf & 0x7fffffff) || (ug & 0x7fffffff))) return 1;
  if ((uf & 0x7fffffff) > 0x7f800000) return 0;
  if ((ug & 0x7fffffff) > 0x7f800000) return 0;
      return uf == ug;
}
/* 
 * floatScale4 - Return bit-level equivalent of expression 4*f for
 *   floating point argument f.
 *   Both the argument and result are passed as unsigned int's, but
 *   they are to be interpreted as the bit-level representation of
 *   single-precision floating point values.
 *   When argument is NaN, return argument
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned floatScale4(unsigned uf) {
  int sign = uf & 0x80000000;
  int expo = uf & 0x7f800000;
  int i = 2;
  if ((uf & 0x7fffffff) > 0x7f800000) return uf;
  while (i) {
  if (!expo) {
    uf = sign | (uf << 1);
  } else if (expo != 0x7f800000) {
    uf += 0x800000;
  }
  i--;
  expo = uf & 0x7f800000;
  }
  if (expo == 0x7f800000) {
    uf = sign | 0x7f800000;
  }
    return uf;
}
/* 
 * floatUnsigned2Float - Return bit-level equivalent of expression (float) u
 *   Result is returned as unsigned int, but
 *   it is to be interpreted as the bit-level representation of a
 *   single-precision floating point values.
 *   Legal ops: Any integer/unsigned operations incl. ||, &&. also if, while
 *   Max ops: 30
 *   Rating: 4
 */
unsigned floatUnsigned2Float(unsigned u) {
  int len = 31;
  int move, G, R, S;
  if(!u) return 0;
  while(!(u&(1<<len))) len--;
  if(len<=23) u<<=(23-len);
  else{
	  	move = len - 23;
	  	G = u & (1 << move);
	  	R = u & (1 << (move - 1));
	  	if (move == 1) {
	  		S = 0;
	  	} else {
	  		S = u << (33 - move);
	  	}
	  	u >>= move;
	  	u += R && (G || S);
	  	if (!(0x01000000 ^ u)) len++;
//    u+=(1<<(n_-24));
//    if(u<<(55-n_)) ;else u&=(0xffffffff<<(n_-22));
//    if(u&(1<<n_))  ;else n_++;
//    u >>= (n_-23);
  }
  return (u & 0x007fffff)|((len + 127) << 23);
}
