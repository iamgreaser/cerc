-define(is_reg8(X), (
	X == al orelse
	X == cl orelse
	X == dl orelse
	X == bl orelse
	X == ah orelse
	X == ch orelse
	X == dh orelse
	X == bh)).
-define(is_reg16(X), (
	X == ax orelse
	X == cx orelse
	X == dx orelse
	X == bx orelse
	X == sp orelse
	X == bp orelse
	X == si orelse
	X == di)).
-define(is_reg16_grp(X), (
	X == bx_si orelse
	X == bx_di orelse
	X == bp_si orelse
	X == bp_di orelse
	X == si orelse
	X == di orelse
	X == bp orelse
	X == bx)).

-define(is_mem(X), (X == memw orelse X == memb)).

-define(is_cmp(X), (
	X == eq orelse
	X == ne orelse
	X == lt orelse
	X == le orelse
	X == gt orelse
	X == ge)).

-define(is_cjmp(X), (
	X == jz orelse
	X == jnz orelse
	X == jl orelse
	X == jle orelse
	X == jg orelse
	X == jge)).

-define(is_grp1(X), (
	X == add  orelse
	X == zor  orelse
	X == adc  orelse
	X == sbb  orelse
	X == zand orelse
	X == sub  orelse
	X == zxor orelse
	X == cmp)).

-define(is_grp2(X), (
	X == rol orelse
	X == ror orelse
	X == rcl orelse
	X == rcr orelse
	X == shl orelse
	X == shr orelse
	X == sar)).

-define(is_grp3(X), (
	X == znot orelse
	X == neg  orelse
	X == mul  orelse
	X == imul orelse
	X == zdiv orelse
	X == idiv)).

-define(is_grp5(X), (
	X == inc   orelse
	X == dec   orelse
	X == zcall orelse
	X == jmp   orelse
	X == push)).

