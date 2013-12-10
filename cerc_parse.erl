% LL(*) C-like parser + interpreter
% 2013, Ben "GreaseMonkey" Russell -- Public Domain

-module(cerc_parse).
-export([parse_code/1]).
-include("cerc.hrl").

%%%
skip_ws([X|T]) when (X == 32 orelse X == 13 orelse X == 10 orelse X == 9) ->
	skip_ws(T);
skip_ws(L) -> L.

%%%
parse_name([X|T], Acc) when (X >= $0 andalso X =< $7)
		orelse (X >= $A andalso X =< $Z)
		orelse (X >= $a andalso X =< $z)
		orelse X == $_ ->
	parse_name(T, [X|Acc]);
parse_name(L, Acc) ->
	{lists:reverse(Acc), L}.

parse_name(L) -> parse_name(L, "").

%%%
parse_number_oct([X|T], Acc) when (X >= $0 andalso X =< $7) ->
	parse_number_oct(T, Acc*8 + (X-$0));
parse_number_oct(L, Acc) ->
	{Acc, L}.

%%%
parse_number_dec([X|T], Acc) when (X >= $0 andalso X =< $9) ->
	parse_number_dec(T, Acc*10 + (X-$0));
parse_number_dec(L, Acc) ->
	{Acc, L}.

%%%
parse_number_hex([X|T], Acc) when (X >= $0 andalso X =< $9) ->
	parse_number_hex(T, Acc*16 + (X-$0));
parse_number_hex([X|T], Acc) when (X >= $A andalso X =< $F) ->
	parse_number_hex(T, Acc*16 + (X-$A) + 10);
parse_number_hex([X|T], Acc) when (X >= $a andalso X =< $f) ->
	parse_number_hex(T, Acc*16 + (X-$a) + 10);
parse_number_hex(L, Acc) ->
	{Acc, L}.

%%%
parse_number([$0,X|T]) when (X == $x orelse X == $X) ->
	parse_number_hex(T, 0);
parse_number(L=[$0|_]) ->
	parse_number_oct(L, 0);
parse_number(L=[X|_]) when (X >= $1 andalso X =< $9) ->
	parse_number_dec(L, 0).

%%%
parse_expr_sing(L=[X|_]) when (X >= $a andalso X =< $z)
		orelse (X >= $A andalso X =< $Z)
		orelse X == $_ ->
	{S, L1} = parse_name(L),
	{{nam, S}, skip_ws(L1)};
parse_expr_sing(L=[X|_]) when (X >= $0 andalso X =< $9) ->
	{N, L1} = parse_number(L),
	{{int, N}, skip_ws(L1)};
parse_expr_sing([$+,$+|T]) ->
	{E, L1} = parse_expr_sing(skip_ws(T)),
	{{unop, preinc, E}, skip_ws(L1)};
parse_expr_sing([$-,$-|T]) ->
	{E, L1} = parse_expr_sing(skip_ws(T)),
	{{unop, predec, E}, skip_ws(L1)};
parse_expr_sing([$-|T]) ->
	{E, L1} = parse_expr_sing(skip_ws(T)),
	{{unop, neg, E}, skip_ws(L1)};
parse_expr_sing([$(|T]) ->
	{E, L1} = parse_expr(skip_ws(T)),
	[$)|L2] = skip_ws(L1),
	{{unop, grp, E}, skip_ws(L2)}.

%%%
parse_expr_binop(E1, [$+,$+|T]) ->
	parse_expr_binop({unop, postinc, E1}, T);
parse_expr_binop(E1, [$-,$-|T]) ->
	parse_expr_binop({unop, postdec, E1}, T);
parse_expr_binop(E1, L) ->
	{L1, Mode} = case L of
		[$+,$=|T] -> {T, assadd};
		[$-,$=|T] -> {T, asssub};
		[$*,$=|T] -> {T, assmul};
		[$/,$=|T] -> {T, assquo};
		[$%,$=|T] -> {T, assmod};
		[$+|T] -> {T, add};
		[$-|T] -> {T, sub};
		[$*|T] -> {T, mul};
		[$/|T] -> {T, quo};
		[$%|T] -> {T, mod};
		[$=,$=|T] -> {T, eq};
		[$!,$=|T] -> {T, ne};
		[$<,$=|T] -> {T, le};
		[$>,$=|T] -> {T, ge};
		[$<|T] -> {T, lt};
		[$>|T] -> {T, gt};
		[$=|T] -> {T, ass};
		_ -> {L, none}
	end,
	case Mode of
		none -> {E1, L1};	
		_ -> 
			{Ey, L2} = parse_expr(skip_ws(L1)),
			{{binop_x, Mode, E1, Ey}, skip_ws(L2)}
	end.

%%%
parse_expr(L) ->
	{E, L1} = parse_expr_sing(skip_ws(L)),
	L2 = skip_ws(L1),
	parse_expr_binop(E, L2).

%%%
parse_stat(L = [$}|_]) -> {term, L};
parse_stat(L = "") -> {term, L};
parse_stat(L = [X|_]) when (X >= $A andalso X =< $Z)
		orelse (X >= $a andalso X =< $z)
		orelse X == $_ ->
	{S, LS} = parse_name(L),
	case S of
		"while" ->
			L1 = [$(|_] = skip_ws(LS),
			{E1, L2} = parse_expr(L1),
			L3 = skip_ws(L2),
			{ET, L4} = case L3 of
				[${|Tx1] ->
					parse_block(skip_ws(Tx1));
				_ ->
					{Ex1, Lx1} = parse_expr(skip_ws(L2)),
					[$;|Lx2] = skip_ws(Lx1),
					{{op, Ex1, term}, skip_ws(Lx2)}
			end,
			{T, L5} = parse_stat(skip_ws(L4)),
			{{op_while, E1, ET, T}, L5};
		"for" ->
			[$(|L1] = skip_ws(LS),
			{EI, L2} = parse_expr(skip_ws(L1)),
			[$;|L3] = skip_ws(L2),
			{EC, L4} = parse_expr(skip_ws(L3)),
			[$;|L5] = skip_ws(L4),
			{EL, L6} = parse_expr(skip_ws(L5)),
			[$)|L7] = skip_ws(L6),
			L8 = skip_ws(L7),
			{EB, L9} = case L8 of
				[${|Tx1] ->
					parse_block(skip_ws(Tx1));
				_ ->
					{Ex1, Lx1} = parse_expr(skip_ws(L8)),
					[$;|Lx2] = skip_ws(Lx1),
					{{op, Ex1, term}, skip_ws(Lx2)}
			end,
			{T, L10} = parse_stat(skip_ws(L9)),
			{{op_for, EI, EC, EL, EB, T}, L10};
		"if" ->
			L1 = [$(|_] = skip_ws(LS),
			{E1, L2} = parse_expr(L1),
			L3 = skip_ws(L2),
			{ET, L4} = case L3 of
				[${|Tx1] ->
					parse_block(skip_ws(Tx1));
				_ ->
					{Ex1, Lx1} = parse_expr(skip_ws(L2)),
					[$;|Lx2] = skip_ws(Lx1),
					{{op, Ex1, term}, skip_ws(Lx2)}
			end,
			{T, L5} = parse_stat(skip_ws(L4)),
			case T of
				{op_else, EF, T2} ->
					{{op_if, E1, ET, EF, T2}, L5};
				_ ->
					{{op_if, E1, ET, term, T}, L5}
			end;
		"else" ->
			L1 = skip_ws(LS),
			{EF, L2} = case L1 of
				[${|Tx1] ->
					parse_block(skip_ws(Tx1));
				_ ->
					{Ex1, Lx1} = parse_expr(L1),
					[$;|Lx2] = skip_ws(Lx1),
					{{op, Ex1, term}, skip_ws(Lx2)}
			end,
			{T, L3} = parse_stat(skip_ws(L2)),
			{{op_else, EF, T}, L3};
		_ ->
			{E, L1} = parse_expr(skip_ws(L)),
			[$;|L2] = skip_ws(L1),
			{T, L3} = parse_stat(skip_ws(L2)),
			{{op, E, T}, L3}
	end;
parse_stat([${|L]) ->
	{E, L1} = parse_block(skip_ws(L)),
	{T, L2} = parse_stat(skip_ws(L1)),
	{{op_block, E, T}, L2};
parse_stat(L) ->
	{E, L1} = parse_expr(skip_ws(L)),
	[$;|L2] = skip_ws(L1),
	{T, L3} = parse_stat(skip_ws(L2)),
	{{op, E, T}, L3}.

%%%
preced(mul) -> 3;
preced(quo) -> 3;
preced(mod) -> 3;
preced(add) -> 4;
preced(sub) -> 4;
preced(lt) -> 6;
preced(le) -> 6;
preced(gt) -> 6;
preced(ge) -> 6;
preced(eq) -> 7;
preced(ne) -> 7;
preced(assadd) -> 13;
preced(asssub) -> 13;
preced(assmul) -> 13;
preced(assquo) -> 13;
preced(assmod) -> 13;
preced(ass) -> 13.

%%%
rotate_tree(term) -> term;
rotate_tree({op, E, T}) ->
	{op, rotate_tree(E),
		rotate_tree(T)};
rotate_tree({op_block, E, T}) ->
	{op_block, rotate_tree(E),
		rotate_tree(T)};
rotate_tree({op_if, E1, ET, EF, T}) ->
	{op_if, rotate_tree(E1),
		rotate_tree(ET),
		rotate_tree(EF),
		rotate_tree(T)};
rotate_tree({op_for, EI, EC, EL, EB, T}) ->
	{op_for, rotate_tree(EI),
		rotate_tree(EC),
		rotate_tree(EL),
		rotate_tree(EB),
		rotate_tree(T)};
rotate_tree({op_while, E1, E2, T}) ->
	{op_while, rotate_tree(E1),
		rotate_tree(E2),
		rotate_tree(T)};
rotate_tree({unop, Mode, E}) ->
	{unop, Mode, rotate_tree(E)};
rotate_tree({binop_x, Mode, E1, E2}) ->
	E2N = rotate_tree(E2),
	case E2N of
		{BinOp, Mode2, E21, E22} when BinOp == binop orelse BinOp == binop_x ->
			P = preced(Mode),
			P2 = preced(Mode2),
			case P =< P2 of
				false -> rotate_tree({binop, Mode, E1, E2N});
				true -> rotate_tree({binop_x,
					Mode2,
						{binop_x, Mode, E1, E21},
						E22})
			end;
		_ -> rotate_tree({binop, Mode, E1, E2N})
	end;
rotate_tree({binop, Mode, E1, E2}) ->
	{binop, Mode, rotate_tree(E1), rotate_tree(E2)};
rotate_tree(E = {nam, _}) -> E;
rotate_tree(E = {int, _}) -> E.

%%%
parse_block(Str) ->
	{Code, T} = parse_stat(Str),
	[$}|T2] = skip_ws(T),
	{Code, skip_ws(T2)}.

%%%
parse_code(Str) ->
	{Code, T} = parse_stat(Str),
	"" = skip_ws(T),
	rotate_tree(Code).


