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
em8(State, V) -> cerc_cmp:em8(State, V).

em16(State, V) -> cerc_cmp:em16le(State, V).

%%%
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

emit_modrm(S0, R1, {memw, bp}) when ?is_reg8(R1) ->
	S1 = em8(S0, 16#46 + (reg8(R1) bsl 3)),
	em8(S1, 16#00);
emit_modrm(S0, R1, {memw, I2}) when ?is_reg8(R1) andalso is_number(I2) ->
	S1 = em8(S0, 16#06 + (reg8(R1) bsl 3)),
	em16(S1, I2);
emit_modrm(S0, R1, {memw, R2, I2}) when ?is_reg8(R1) andalso ?is_reg16_grp(R2) andalso is_number(I2) ->
	S1 = em8(S0, 16#80 + (reg8(R1) bsl 3) + reg16_grp(R2)),
	em16(S1, I2);
emit_modrm(S0, R1, {memw, R2}) when ?is_reg8(R1) andalso ?is_reg16_grp(R2) ->
	em8(S0, 16#00 + (reg8(R1) bsl 3) + reg16_grp(R2));
emit_modrm(S0, R1, R2) when ?is_reg8(R1) andalso ?is_reg8(R2) ->
	em8(S0, 16#C0 + (reg8(R1) bsl 3) + reg8(R2)).

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

emit(S0, sahf, []) -> em8(S0, 16#9E);
emit(S0, lahf, []) -> em8(S0, 16#9F);

emit(S0, int, [3]) ->
	em8(S0, 16#CC);
emit(S0, int, [I1]) when is_number(I1) ->
	S1 = em8(S0, 16#CD),
	em8(S1, I1 band 16#FF).

%%%
emit_chain(S0, []) -> S0;
emit_chain(S0, [{Op, Args} | T]) ->
	S1 = emit(S0, Op, Args),
	emit_chain(S1, T).

%%%
heap_set_fix(S, S0) ->
	S0#cstate {
		heapfix = [{imm16le, S0#cstate.pc, S} | S0#cstate.heapfix]}.

%%%
build_op(I={nam, S}, S0) ->
	emit(S0, mov, [ax, {memw, I}]);
build_op({binop, Mode, N1={nam, S}, E2}, S0) when ?is_ass(Mode) ->
	S1 = build_op(E2, S0),
	case Mode of
		ass -> emit(S1, mov, [{memw, N1}, ax]);
		assadd -> emit(S1, add, [{memw, N1}, ax]);
		asssub -> emit(S1, sub, [{memw, N1}, ax]);
			% the rest are kinda tricky.
		assmul -> emit_chain(S1, [
			{zxor, [dx, dx]},
			{imul, [{memw, N1}]},
			{mov, [{memw, N1}, ax]}]);
		assquo -> emit_chain(S1, [
			{zxor, [dx, dx]},
			{idiv, [{memw, N1}]},
			{mov, [{memw, N1}, ax]}]);
		assmod -> emit_chain(S1, [
			{zxor, [dx, dx]},
			{idiv, [{memw, N1}]},
			{mov, [ax, dx]},
			{mov, [{memw, N1}, ax]}]);
		assshl -> emit_chain(S1, [
			{mov, [cx, ax]},
			{shl, [{memw, N1}, cl]},
			{mov, [ax, {memw, N1}]}]);
		assshr -> emit_chain(S1, [
			{mov, [cx, ax]},
			{shl, [{memw, N1}, cl]},
			{mov, [ax, {memw, N1}]}])
	end;
build_op({int, N}, S0) ->
	emit(S0, mov, [ax, N]);
build_op({unop, Mode, E}, S0) ->
	S1 = build_op(E, S0),
	case Mode of
		neg -> emit(S1, neg, [ax]);
		grp -> S1
	end.

%%%
build(term, S0) ->
	S0;
build({op_while, EC, EB, T}, S0) ->
	% TODO!
	build(T, S0);
build({op_if, EC, ET, EF, T}, S0) ->
	% TODO!
	build(T, S0);
build({op_for, EI, EC, EL, EB, T}, S0) ->
	% TODO: loop body
	S1 = build_op(EI, S0),
	build(T, S1);
build({op_block, E, T}, S0) ->
	S1 = build(E, S0),
	build(T, S1);
build({op, E, T}, S0) ->
	S1 = build_op(E, S0),
	build(T, S1).

%%%
build_result(S0) ->
	% TODO!
	emit_chain(S0, [
		]).

%%%
build_code(Code, S0) ->
	S1 = build(Code, S0),
	S2 = build_result(S1),
	emit(S2, int, [16#20]).

