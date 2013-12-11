% LL(*) C-like parser + interpreter
% 2013, Ben "GreaseMonkey" Russell -- Public Domain

-module(cerc_asm_x86).
-export([emit/3]).
-include("cerc.hrl").
-include("cerc_cmp_x86.hrl").

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
reg16_grp(bx_si) -> 0;
reg16_grp(bx_di) -> 1;
reg16_grp(bp_si) -> 2;
reg16_grp(bp_di) -> 3;
reg16_grp(si) -> 4;
reg16_grp(di) -> 5;
reg16_grp(bp) -> 6;
reg16_grp(bx) -> 7.

%%%
cjmp(jz)  -> 4;
cjmp(jnz) -> 5;
cjmp(jl)  -> 12;
cjmp(jle) -> 13;
cjmp(jg)  -> 14;
cjmp(jge) -> 15.

%%%
grp1(add) ->  0;
grp1(zor) ->  1;
grp1(adc) ->  2;
grp1(sbb) ->  3;
grp1(zand) -> 4;
grp1(sub) ->  5;
grp1(zxor) -> 6;
grp1(cmp) ->  7.

%%%
grp2(rol) -> 0;
grp2(ror) -> 1;
grp2(rcl) -> 2;
grp2(rcr) -> 3;
grp2(shl) -> 4;
grp2(shr) -> 5;
grp2(sar) -> 7.

%%%
grp3(znot) -> 2;
grp3(neg)  -> 3;
grp3(mul)  -> 4;
grp3(imul) -> 5;
grp3(zdiv) -> 6;
grp3(idiv) -> 7.

%%%
grp5(inc)   -> 0;
grp5(dec)   -> 1;
grp5(zcall) -> 2;
grp5(jmp)   -> 4;
grp5(push)  -> 6.

%%%
em8(State, V) -> cerc_cmp:em8(State, V).
em16(State, V) -> cerc_cmp:em16le(State, V).
heap_set_fix(S, S0) -> cerc_cmp:heap_set_fix(S, S0).

%%%
% 16-bit versions
emit_modrm(S0, R1, {memw, bp}) when ?is_reg16(R1) ->
	S1 = em8(S0, 16#46 + (reg16(R1) bsl 3)),
	em8(S1, 16#00);
emit_modrm(S0, R1, {memw, I2}) when ?is_reg16(R1) andalso is_number(I2) ->
	S1 = em8(S0, 16#06 + (reg16(R1) bsl 3)),
	em16(S1, I2);
emit_modrm(S0, R1, {memw, R2, I2}) when ?is_reg16(R1) andalso ?is_reg16_grp(R2) andalso is_number(I2) ->
	S1 = em8(S0, 16#80 + (reg16(R1) bsl 3) + reg16_grp(R2)),
	em16(S1, I2);
emit_modrm(S0, R1, {memw, R2}) when ?is_reg16(R1) andalso ?is_reg16_grp(R2) ->
	em8(S0, 16#00 + (reg16(R1) bsl 3) + reg16_grp(R2));
emit_modrm(S0, R1, R2) when ?is_reg16(R1) andalso ?is_reg16(R2) ->
	em8(S0, 16#C0 + (reg16(R1) bsl 3) + reg16(R2));
emit_modrm(S0, I1, {memw, R2}) when is_number(I1) andalso ?is_reg16_grp(R2) ->
	em8(S0, 16#00 + (I1 bsl 3) + reg16_grp(R2));
emit_modrm(S0, I1, R2) when is_number(I1) andalso ?is_reg16(R2) ->
	em8(S0, 16#C0 + (I1 bsl 3) + reg16(R2));

% 8-bit versions
emit_modrm(S0, R1, {memb, bp}) when ?is_reg8(R1) ->
	S1 = em8(S0, 16#46 + (reg8(R1) bsl 3)),
	em8(S1, 16#00);
emit_modrm(S0, R1, {memb, I2}) when ?is_reg8(R1) andalso is_number(I2) ->
	S1 = em8(S0, 16#06 + (reg8(R1) bsl 3)),
	em16(S1, I2);
emit_modrm(S0, R1, {memb, R2, I2}) when ?is_reg8(R1) andalso ?is_reg16_grp(R2) andalso is_number(I2) ->
	S1 = em8(S0, 16#80 + (reg8(R1) bsl 3) + reg16_grp(R2)),
	em16(S1, I2);
emit_modrm(S0, R1, {memb, R2}) when ?is_reg8(R1) andalso ?is_reg16_grp(R2) ->
	em8(S0, 16#00 + (reg8(R1) bsl 3) + reg16_grp(R2));
emit_modrm(S0, R1, R2) when ?is_reg8(R1) andalso ?is_reg8(R2) ->
	em8(S0, 16#C0 + (reg8(R1) bsl 3) + reg8(R2));
emit_modrm(S0, I1, {memb, R2}) when is_number(I1) andalso ?is_reg16_grp(R2) ->
	em8(S0, 16#00 + (I1 bsl 3) + reg16_grp(R2));
emit_modrm(S0, I1, R2) when is_number(I1) andalso ?is_reg8(R2) ->
	em8(S0, 16#C0 + (I1 bsl 3) + reg8(R2));

% named versions
emit_modrm(S0, R1, {memw, {nam, N2}}) when ?is_reg16(R1) ->
	S1 = em8(S0, 16#06 + (reg16(R1) bsl 3)),
	em16(heap_set_fix(N2, S1), 0);
emit_modrm(S0, R1, {memw, R2, {nam, N2}}) when ?is_reg16(R1) andalso ?is_reg16_grp(R2) ->
	S1 = em8(S0, 16#80 + (reg16(R1) bsl 3) + reg16_grp(R2)),
	em16(heap_set_fix(N2, S1), 0);
emit_modrm(S0, I1, {memw, {nam, N2}}) when is_number(I1) ->
	S1 = em8(S0, 16#06 + (I1 bsl 3)),
	em16(heap_set_fix(N2, S1), 0);
emit_modrm(S0, I1, {memw, R2, {nam, N2}}) when is_number(I1) andalso ?is_reg16_grp(R2) ->
	S1 = em8(S0, 16#80 + (I1 bsl 3) + reg16_grp(R2)),
	em16(heap_set_fix(N2, S1), 0).

%%%
emit(S0, mov, [R1, I2]) when ?is_reg16(R1) andalso is_number(I2) ->
	S1 = em8(S0, reg16(R1) + 16#B8),
	em16(S1, I2 band 16#FFFF);
emit(S0, mov, [R1, I2]) when ?is_reg8(R1) andalso is_number(I2) ->
	S1 = em8(S0, reg8(R1) + 16#B0),
	em8(S1, I2 band 16#FF);
emit(S0, mov, [ax, {memw, I2}]) when is_number(I2) ->
	S1 = em8(S0, 16#A1),
	em16(S1, I2 band 16#FFFF);
emit(S0, mov, [al, {memb, I2}]) when is_number(I2) ->
	S1 = em8(S0, 16#A0),
	em8(S1, I2 band 16#FF);
emit(S0, mov, [{memw, I1}, ax]) when is_number(I1) ->
	S1 = em8(S0, 16#A3),
	em16(S1, I1 band 16#FFFF);
emit(S0, mov, [{memb, I1}, al]) when is_number(I1) ->
	S1 = em8(S0, 16#A2),
	em8(S1, I1 band 16#FF);

% modr/m versions
emit(S0, mov, [R1, X2]) when ?is_reg16(R1) ->
	S1 = em8(S0, 16#8B),
	emit_modrm(S1, R1, X2);
emit(S0, mov, [R1, X2]) when ?is_reg8(R1) ->
	S1 = em8(S0, 16#8A),
	emit_modrm(S1, R1, X2);
emit(S0, mov, [X1, R2]) when ?is_reg16(R2) ->
	S1 = em8(S0, 16#89),
	emit_modrm(S1, R2, X1);
emit(S0, mov, [X1, R2]) when ?is_reg8(R2) ->
	S1 = em8(S0, 16#88),
	emit_modrm(S1, R2, X1);

% named versions
emit(S0, mov, [R1, {nam, N2}]) when ?is_reg16(R1) ->
	S1 = em8(S0, reg16(R1) + 16#B8),
	em16(heap_set_fix(N2, S1), 0);
emit(S0, mov, [ax, {memw, {nam, N2}}]) ->
	S1 = em8(S0, 16#A1),
	em16(heap_set_fix(N2, S1), 0);
emit(S0, mov, [{memw, {nam, N1}}, ax]) ->
	S1 = em8(S0, 16#A3),
	em16(heap_set_fix(N1, S1), 0);

% push/pop (non-grp1)
emit(S0, push, [R1]) when ?is_reg16(R1) ->
	em8(S0, 16#50 + reg16(R1));
emit(S0, pop, [R1]) when ?is_reg16(R1) ->
	em8(S0, 16#58 + reg16(R1));

% grp1
emit(S0, Mode, [ax, I2]) when ?is_grp1(Mode) andalso is_number(I2) ->
	S1 = em8(S0, 16#05 + (grp1(Mode) bsl 3)),
	em16(S1, I2);
emit(S0, Mode, [al, I2]) when ?is_grp1(Mode) andalso is_number(I2) ->
	S1 = em8(S0, 16#04 + (grp1(Mode) bsl 3)),
	em8(S1, I2);
emit(S0, Mode, [R1, I2]) when ?is_grp1(Mode) andalso ?is_reg8(R1) andalso is_number(I2) ->
	S1 = em8(S0, 16#80),
	S2 = emit_modrm(S1, grp1(Mode), R1),
	em8(S2, I2);
emit(S0, Mode, [X1, I2]) when ?is_grp1(Mode) andalso is_number(I2) ->
	% TODO: enforce 8/16-bit X1
	S1 = em8(S0, 16#81),
	S2 = emit_modrm(S1, grp1(Mode), X1),
	em16(S2, I2);
emit(S0, Mode, [X1, R2]) when ?is_grp1(Mode) andalso ?is_reg16(R2) ->
	S1 = em8(S0, 16#01 + (grp1(Mode) bsl 3)),
	emit_modrm(S1, R2, X1);
emit(S0, Mode, [X1, R2]) when ?is_grp1(Mode) andalso ?is_reg8(R2) ->
	S1 = em8(S0, 16#00 + (grp1(Mode) bsl 3)),
	emit_modrm(S1, R2, X1);
emit(S0, Mode, [R1, X2]) when ?is_grp1(Mode) andalso ?is_reg16(R1) ->
	S1 = em8(S0, 16#03 + (grp1(Mode) bsl 3)),
	emit_modrm(S1, R1, X2);
emit(S0, Mode, [R1, X2]) when ?is_grp1(Mode) andalso ?is_reg8(R1) ->
	S1 = em8(S0, 16#02 + (grp1(Mode) bsl 3)),
	emit_modrm(S1, R1, X2);

% grp2
% TODO: determine "bitness" (assuming 16-bit for now)
emit(S0, Mode, [R1, 1]) when ?is_grp2(Mode) and ?is_reg8(R1) ->
	S1 = em8(S0, 16#D0),
	emit_modrm(S1, grp2(Mode), R1);
emit(S0, Mode, [R1, cl]) when ?is_grp2(Mode) and ?is_reg8(R1) ->
	S1 = em8(S0, 16#D2),
	emit_modrm(S1, grp2(Mode), R1);
emit(S0, Mode, [X1, 1]) when ?is_grp2(Mode) ->
	S1 = em8(S0, 16#D1),
	emit_modrm(S1, grp2(Mode), X1);
emit(S0, Mode, [X1, cl]) when ?is_grp2(Mode) ->
	S1 = em8(S0, 16#D3),
	emit_modrm(S1, grp2(Mode), X1);

% grp3 (aside from TEST)
emit(S0, Mode, [R1]) when ?is_grp3(Mode) and ?is_reg8(R1) ->
	S1 = em8(S0, 16#F6),
	emit_modrm(S1, grp3(Mode), R1);
emit(S0, Mode, [X1]) when ?is_grp3(Mode) ->
	S1 = em8(S0, 16#F7),
	emit_modrm(S1, grp3(Mode), X1);

%
emit(S0, jmp, [I1]) when is_number(I1) ->
	S1 = em8(S0, 16#E9),
	em16(S1, 16#FFFF band (I1 - (S1#cstate.pc + 2)));
emit(S0, Mode, [I1]) when ?is_cjmp(Mode) andalso is_number(I1) ->
	S1 = em8(S0, 16#70 + cjmp(Mode)),
	em8(S1, 16#FF band (I1 - (S1#cstate.pc + 1)));

%
emit(S0, inc, [R1]) when ?is_reg16(R1) ->
	em8(S0, 16#40 + reg16(R1));
emit(S0, dec, [R1]) when ?is_reg16(R1) ->
	em8(S0, 16#48 + reg16(R1));
emit(S0, inc, [R1]) when ?is_reg8(R1) ->
	S1 = em8(S0, 16#FE + reg16(R1)),
	emit_modrm(S1, 0, reg8(R1));
emit(S0, dec, [R1]) when ?is_reg8(R1) ->
	S1 = em8(S0, 16#FE),
	emit_modrm(S1, 1, reg8(R1));

%
emit(S0, Mode, [X1]) when ?is_grp5(Mode) ->
	S1 = em8(S0, 16#FF),
	emit_modrm(S1, grp5(Mode), X1);

%
emit(S0, daa, []) -> em8(S0, 16#27);
emit(S0, sahf, []) -> em8(S0, 16#9E);
emit(S0, lahf, []) -> em8(S0, 16#9F);

%
emit(S0, int, [3]) ->
	em8(S0, 16#CC);
emit(S0, int, [I1]) when is_number(I1) ->
	S1 = em8(S0, 16#CD),
	em8(S1, I1 band 16#FF).


