% LL(*) C-like parser + interpreter
% 2013, Ben "GreaseMonkey" Russell -- Public Domain

-module(cerc_cmp_x86).
-export([build_code/2]).
-include("cerc.hrl").
-include("cerc_cmp_x86.hrl").

emit(S0, Op, Args) ->
	cerc_asm_x86:emit(S0, Op, Args).

%%%
emit_chain(S0, []) -> S0;
emit_chain(S0, [{Op, Args} | T]) ->
	S1 = emit(S0, Op, Args),
	emit_chain(S1, T).

%%%
add_fix(Fix, S0) ->
	cerc_cmp:add_fix(Fix, S0).

%%%
build_op(I={nam, S}, S0) ->
	emit(S0, mov, [ax, {memw, I}]);
build_op({binop, ass, N1={nam, S}, E2}, S0) ->
	S1 = build_op(E2, S0),
	emit(S1, mov, [{memw, N1}, ax]);
build_op({binop, Mode, N1={nam, S}, E2}, S0) when ?is_ass(Mode) ->
	BinMode = case Mode of
		assadd -> add;
		asssub -> sub;
		assmul -> mul;
		assquo -> quo;
		assmod -> mod;
		assshl -> shl;
		assshr -> shr
	end,
	S1 = build_op({binop, BinMode, {nam, S}, E2}, S0),
	emit(S1, mov, [{memw, N1}, ax]);
build_op(dummy, S0) -> S0;
build_op({int, N}, S0) ->
	emit(S0, mov, [ax, N]);
build_op({binop, Mode, E1, E2}, S0) ->
	S1 = build_op(E1, S0),
	S2 = emit(S1, push, [ax]),
	S3 = build_op(E2, S2),
	S4 = emit_chain(S3, [
		{mov, [cx, ax]},
		{pop, [ax]}]),
	case Mode of
		add -> emit(S4, add, [ax, cx]);
		sub -> emit(S4, sub, [ax, cx]);
		mul -> emit_chain(S4, [
			{zxor, [dx, dx]},
			{imul, [cx]}]);
		quo -> emit_chain(S4, [
			{zxor, [dx, dx]},
			{idiv, [cx]}]);
		mod -> emit_chain(S4, [
			{zxor, [dx, dx]},
			{idiv, [cx]},
			{mov, [ax, dx]}]);
		shl -> emit(S4, shl, [ax, cl]);
		shr -> emit(S4, shr, [ax, cl])
	end;

build_op({unop, Mode, I={nam, S}}, S0) ->
	case Mode of
		preinc -> emit_chain(S0, [
			{inc, [{memw, I}]},
			{mov, [ax, {memw, I}]}]);
		postinc -> emit_chain(S0, [
			{mov, [ax, {memw, I}]},
			{inc, [{memw, I}]}]);
		predec -> emit_chain(S0, [
			{dec, [{memw, I}]},
			{mov, [ax, {memw, I}]}]);
		postdec -> emit_chain(S0, [
			{mov, [ax, {memw, I}]},
			{dec, [{memw, I}]}]);
		_ -> S1 = emit(S0, mov, [ax, {memw, I}]),
			build_op({unop, Mode, dummy}, S1)
	end;
build_op({unop, Mode, E}, S0) ->
	S1 = build_op(E, S0),
	case Mode of
		neg -> emit(S1, neg, [ax]);
		grp -> S1
	end.

%%%
build_op_cond_after(Mode, {jmp, JState, NewPC}, S0) when ?is_cmp(Mode) ->
	S1 = emit(S0, cmp, [ax, dx]),
	FromPC = S1#cstate.pc + 2,
	Delta = NewPC - FromPC,
	case Delta =< 16#7F andalso Delta >= -16#80 of
		false ->
			S2 = build_op_cond_after(Mode, {jmp, not JState, FromPC + 3}, S1),
			emit(S2, jmp, [FromPC + 3]);
		true ->
			Mode2 = case {Mode, JState} of
				{_, true} -> Mode;
				{eq, false} -> ne;
				{ne, false} -> eq;
				{lt, false} -> ge;
				{le, false} -> gt;
				{gt, false} -> le;
				{ge, false} -> lt
			end,
			SubMode = case Mode2 of
				eq -> jz;
				ne -> jnz;
				lt -> jl;
				le -> jle;
				gt -> jg;
				ge -> jge
			end,
			emit(S1, SubMode, [NewPC])
	end.

%%%
build_op_cond({binop, Mode, E1, E2}, After, S0) when ?is_cmp(Mode) ->
	S1 = build_op(E1, S0),
	S2 = emit(S1, push, [ax]),
	S3 = build_op(E2, S2),
	S4 = emit_chain(S3, [
		{mov, [dx, ax]},
		{pop, [ax]}]),
	build_op_cond_after(Mode, After, S4).
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
	S1 = build_op(EI, S0),
	S2 = emit(S1, jmp, [0]),
	FixPC = S2#cstate.pc,
	S3 = build(EB, S2),
	S4 = build_op(EL, S3),
	NewPC = S4#cstate.pc,
	S5 = add_fix({imm16le, FixPC-2, NewPC-FixPC}, S4),
	S6 = build_op_cond(EC, {jmp, true, FixPC}, S5),
	build(T, S6);
build({op_block, E, T}, S0) ->
	S1 = build(E, S0),
	build(T, S1);
build({op, E, T}, S0) ->
	S1 = build_op(E, S0),
	build(T, S1).

%%%
build_result(S0) ->
	% TODO!
	S1 = emit_chain(S0, [
		{mov, [bx, ax]},

		{mov, [al, bh]},
		{mov, [cl, 4]},
		{shr, [al, cl]},
		{sub, [ah, ah]}, {daa, []}, {add, [al, 16#F0]}, {adc, [al, 16#40]},
		{mov, [ah, 16#02]}, {mov, [dl, al]}, {int, [16#21]},

		{mov, [al, bh]},
		{zand, [al, 16#0F]},
		{sub, [ah, ah]}, {daa, []}, {add, [al, 16#F0]}, {adc, [al, 16#40]},
		{mov, [ah, 16#02]}, {mov, [dl, al]}, {int, [16#21]},

		{mov, [al, bl]},
		{mov, [cl, 4]},
		{shr, [al, cl]},
		{sub, [ah, ah]}, {daa, []}, {add, [al, 16#F0]}, {adc, [al, 16#40]},
		{mov, [ah, 16#02]}, {mov, [dl, al]}, {int, [16#21]},

		{mov, [al, bl]},
		{zand, [al, 16#0F]},
		{sub, [ah, ah]}, {daa, []}, {add, [al, 16#F0]}, {adc, [al, 16#40]},
		{mov, [ah, 16#02]}, {mov, [dl, al]}, {int, [16#21]},

		{mov, [dl, 16#0D]},
		{int, [16#21]},
		{mov, [dl, 16#0A]},
		{int, [16#21]}]).

%%%
build_code(Code, S0) ->
	S1 = build(Code, S0),
	S2 = build_result(S1),
	emit(S2, int, [16#20]).

