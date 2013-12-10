% LL(*) C-like parser + interpreter
% 2013, Ben "GreaseMonkey" Russell -- Public Domain

-module(cerc_cmp_x86).
-export([build_code/2]).
-include("cerc.hrl").

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

%%%
reg8(al) -> 0;
reg8(cl) -> 1;
reg8(dl) -> 2;
reg8(bl) -> 3;
reg8(ah) -> 4;
reg8(ch) -> 5;
reg8(dh) -> 6;
reg8(bh) -> 7.

%%%
reg16(ax) -> 0;
reg16(cx) -> 1;
reg16(dx) -> 2;
reg16(bx) -> 3;
reg16(sp) -> 4;
reg16(bp) -> 5;
reg16(si) -> 6;
reg16(di) -> 7.

%%%
em8(State, V) -> cerc_cmp:em8(State, V).

em16(State, V) -> cerc_cmp:em16le(State, V).

%%%
emit(S0, mov, [R1, I2]) when ?is_reg16(R1) and is_number(I2) ->
	S1 = em8(S0, reg16(R1) + 16#B8),
	em16(S1, I2 band 16#FFFF);
emit(S0, mov, [R1, I2]) when ?is_reg8(R1) and is_number(I2) ->
	S1 = em8(S0, reg8(R1) + 16#B0),
	em8(S1, I2 band 16#FF);

emit(S0, int, [3]) ->
	em8(S0, 16#CC);
emit(S0, int, [I1]) when is_number(I1) ->
	S1 = em8(S0, 16#CD),
	em8(S1, I1 band 16#FF).

%%%
build_code(_Code, S0) ->
	_S1 = emit(S0, int, [16#20]).

