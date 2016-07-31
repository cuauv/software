# CUAUV Simulator Trigger Bytecode Documentation

# Stack

The stack stores bytes. The endianness is undefined.

# Instruction Format

All instructions consist of a uint8 instruction ID, with the exception of instructions that push constants to the stack.

# Instruction Types

ID | Name | Short Description
--- | --- | ---
0 | DUP1 |
1 | DUP4 |
2 | DUP8 |
3 | POP1 |
4 | POP4 |
5 | POP8 |
6 | PUSH1 | 
7 | PUSH4 | Pushes the following four bytes to the stack in order.
8 | PUSH8 |
9 | AND |
10 | OR |
11-15 | * | Reserved for future use.
16 | SBOOL | Loads a boolean with the ID on the top of stack from shared memory and pushes it.
17 | SUINT8 |
18 | SUINT32 |
19 | SUINT64 |
20 | SINT32 |
21 | SINT64 |
22 | SFLOAT32 |
23 | SFLOAT64 |
24 | U32MUL |
25 | U32DIV |
26 | U32ADD |
27 | U32SUB |
28 | U64MUL |
29 | U64DIV |
30 | U64ADD |
31 | U64SUB |
32 | I32MUL |
33 | I32DIV |
34 | I32ADD |
35 | I32SUB |
36 | I64MUL |
37 | I64DIV |
38 | I64ADD |
39 | I64SUB |
40 | F32MUL |
41 | F32DIV |
42 | F32ADD |
43 | F32SUB |
44 | F64MUL |
45 | F64DIV |
46 | F64ADD |
47 | F64SUB |
48 | U32LT | 
49 | U32LTE | 
50 | U32GT | 
51 | U32GTE | 
52 | U32EQ |
53 | U32NEQ |
54 | U64LT | 
55 | U64LTE | 
56 | U64GT | 
57 | U64GTE | 
58 | U64EQ |
59 | U64NEQ |
60 | I32LT | 
61 | I32LTE | 
62 | I32GT | 
63 | I32GTE | 
64 | I32EQ |
65 | I32NEQ |
66 | I64LT | 
67 | I64LTE | 
68 | I64GT | 
69 | I64GTE | 
70 | I64EQ |
71 | I64NEQ |
72 | F32LT | 
73 | F32LTE | 
74 | F32GT | 
75 | F32GTE | 
76 | F32EQ |
77 | F32NEQ |
78 | F32WITHIN | (a : float32, b : float32, c : float32) |a - b| < c
79 | F64LT | 
80 | F64LTE | 
81 | F64GT | 
82 | F64GTE | 
83 | F64EQ |
84 | F64NEQ |
85 | F64WITHIN | (a : float64, b : float64, c : float64) |a - b| < c
86 | 2DWITHIN | (ax, ay, bx, by, r2) (ax-bx)^2 + (ay-by)^2 < r2
87 | 3DWITHIN | (ax, ay, az, bx, by, bz, r2) (ax-bx)^2 + (ay-by)^2 + (az-bz)^2 < r2
