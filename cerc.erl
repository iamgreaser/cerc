% LL(*) C-like parser + interpreter
% 2013, Ben "GreaseMonkey" Russell -- Public Domain

-module(cerc).
-export([main/1,main/0]).
-record(state, {
	vars}).

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

%%%
binop_to_str(add)  -> "+";
binop_to_str(sub)  -> "-";
binop_to_str(mul)  -> "*";
binop_to_str(quo)  -> "/";
binop_to_str(mod)  -> "%";
binop_to_str(zand) -> "&";
binop_to_str(zxor) -> "^";
binop_to_str(zor)  -> "|";
binop_to_str(land) -> "&&";
binop_to_str(lor)  -> "||";
binop_to_str(shl)  -> "<<";
binop_to_str(shr)  -> ">>";
binop_to_str(eq)   -> "==";
binop_to_str(ne)   -> "!=";
binop_to_str(lt)   -> "<";
binop_to_str(le)   -> "<=";
binop_to_str(gt)   -> ">";
binop_to_str(ge)   -> ">=";

binop_to_str(assadd)  -> "+=";
binop_to_str(asssub)  -> "-=";
binop_to_str(assmul)  -> "*=";
binop_to_str(assquo)  -> "/=";
binop_to_str(assmod)  -> "%=";
binop_to_str(asszand) -> "&=";
binop_to_str(asszxor) -> "^=";
binop_to_str(asszor)  -> "|=";
binop_to_str(assshl)  -> "<<=";
binop_to_str(assshr)  -> ">>=";
binop_to_str(ass)  -> "=".

%%%
unop_to_str(preinc)  -> "++";
unop_to_str(predec)  -> "--";
unop_to_str(neg)  -> "-";
unop_to_str(znot) -> "~";
unop_to_str(lnot) -> "!";
unop_to_str(grp)  -> "".

%%%
print_code_tok({binop, Mode, E1, E2}) ->
	io:format("("),
	print_code_tok(E1),
	io:format(" ~s ", [binop_to_str(Mode)]),
	print_code_tok(E2),
	io:format(")");
print_code_tok({unop, postinc, E}) ->
	print_code_tok(E),
	io:format("++");
print_code_tok({unop, postdec, E}) ->
	print_code_tok(E),
	io:format("--");
print_code_tok({unop, Mode, E}) ->
	io:format("~s", [unop_to_str(Mode)]),
	print_code_tok(E);
print_code_tok(term) ->
	ok;
print_code_tok({op_block, E, T}) ->
	io:format("{~n"),
	print_code_tok(E),
	io:format("}~n"),
	print_code_tok(T);
print_code_tok({op, E, T}) ->
	print_code_tok(E),
	io:format(";~n"),
	print_code_tok(T);
print_code_tok({op_for, EI, EC, EL, EB, T}) ->
	io:format("for ("),
	print_code_tok(EI),
	io:format("; "),
	print_code_tok(EC),
	io:format("; "),
	print_code_tok(EL),
	io:format(") {~n"),
	print_code_tok(EB),
	io:format("}~n"),
	print_code_tok(T);
print_code_tok({op_while, E1, E2, T}) ->
	io:format("while "),
	print_code_tok(E1),
	io:format(" {~n"),
	print_code_tok(E2),
	io:format("}~n"),
	print_code_tok(T);
print_code_tok({op_if, E1, ET, EF, T}) ->
	io:format("if "),
	print_code_tok(E1),
	io:format(" {~n"),
	print_code_tok(ET),
	case EF of
		term -> ok;
		_ ->
			io:format("} else {~n"),
			print_code_tok(EF)
	end,
	io:format("}~n"),
	print_code_tok(T);
print_code_tok({nam, S}) -> io:format("~s", [S]);
print_code_tok({int, N}) -> io:format("~p", [N]).

%%%
print_code(Code) ->
	print_code_tok(Code),
	io:format("~n").

%%%
bool_to_int(true) -> 1;
bool_to_int(false) -> 0.

%%%
reduce_code({op, E, T}) ->
	{op, reduce_code(E), reduce_code(T)};
reduce_code({op_if, E1, ET, EF, T}) ->
	{op_if, reduce_code(E1), reduce_code(ET), reduce_code(EF), reduce_code(T)};
reduce_code({op_while, E1, E2, T}) ->
	{op_while, reduce_code(E1), reduce_code(E2), reduce_code(T)};
reduce_code({binop, Mode, E1, E2}) ->
	E1N = reduce_code(E1),
	E2N = reduce_code(E2),
	case {E1N, E2N} of
		{{int, N1}, {int, N2}} ->
			{int, case Mode of
				add -> N1 + N2;
				sub -> N1 - N2;
				mul -> N1 * N2;
				quo -> N1 div N2;
				mod -> N1 rem N2;
				eq -> bool_to_int(N1 == N2);
				ne -> bool_to_int(N1 /= N2);
				lt -> bool_to_int(N1 <  N2);
				le -> bool_to_int(N1 =< N2);
				gt -> bool_to_int(N1 >  N2);
				ge -> bool_to_int(N1 >= N2)
			end};
		_ -> {binop, Mode, E1N, E2N}
	end;
reduce_code({unop, Mode, E}) ->
	EN = reduce_code(E),
	case EN of
		{int, N} ->
			{int, case Mode of
				neg -> -N;
				grp -> N
			end};
		_ -> {unop, Mode, EN}
	end;
reduce_code(Code) -> Code.

%%%
run_binop_val(Mode, V1, V2, _State) ->
	case Mode of
		add -> V1 + V2;
		sub -> V1 - V2;
		mul -> V1 * V2;
		quo -> V1 / V2;
		mod -> V1 rem V2;
		eq -> bool_to_int(V1 == V2);
		ne -> bool_to_int(V1 /= V2);
		lt -> bool_to_int(V1 <  V2);
		le -> bool_to_int(V1 =< V2);
		gt -> bool_to_int(V1 >  V2);
		ge -> bool_to_int(V1 >= V2)
	end.

run_binop_ass(Mode, S, V1, V2, State) ->
	VR = case Mode of
		assadd -> V1 + V2;
		asssub -> V1 - V2;
		assmul -> V1 * V2;
		assquo -> V1 / V2;
		assmod -> V1 rem V2
	end,
	{VR, State#state{vars=dict:store(S, VR, State#state.vars)}}.

%%%
run_op({binop, ass, {nam, S}, E2}, State) ->
	{V2, S2} = run_op(E2, State),
	{V2, S2#state{vars=dict:store(S, V2, S2#state.vars)}};
	
run_op({binop, Mode, E1 = {nam, S}, E2}, State) ->
	{V1, S1} = run_op(E1, State),
	{V2, S2} = run_op(E2, S1),
	case Mode of
		assadd -> run_binop_ass(Mode, S, V1, V2, S2);
		asssub -> run_binop_ass(Mode, S, V1, V2, S2);
		assmul -> run_binop_ass(Mode, S, V1, V2, S2);
		assquo -> run_binop_ass(Mode, S, V1, V2, S2);
		assmod -> run_binop_ass(Mode, S, V1, V2, S2);
		_ -> {run_binop_val(Mode, V1, V2, S2), S2}
	end;

run_op({binop, Mode, E1, E2}, State) ->
	{V1, S1} = run_op(E1, State),
	{V2, S2} = run_op(E2, S1),
	{run_binop_val(Mode, V1, V2, S2), S2};

run_op({unop, Mode, {nam, S}}, State) ->
	case Mode of
		preinc ->
			Vx = dict:fetch(S, State#state.vars) + 1,
			S2 = State#state{vars = dict:store(S, Vx, State#state.vars)},
			{Vx, S2};
		predec ->
			Vx = dict:fetch(S, State#state.vars) - 1,
			S2 = State#state{vars = dict:store(S, Vx, State#state.vars)},
			{Vx, S2};
		postinc ->
			Vx = dict:fetch(S, State#state.vars),
			S2 = State#state{vars = dict:store(S, Vx + 1, State#state.vars)},
			{Vx, S2};
		postdec ->
			Vx = dict:fetch(S, State#state.vars),
			S2 = State#state{vars = dict:store(S, Vx - 1, State#state.vars)},
			{Vx, S2};
		_ -> run_op({unop, Mode, {int, dict:fetch(S, State#state.vars)}}, State)
	end;
run_op({unop, Mode, E}, State) ->
	{V1, S1} = run_op(E, State),
	VR = case Mode of
		neg -> -V1;
		grp -> V1
	end,
	{VR, S1};
run_op({nam, S}, State) -> 
	{dict:fetch(S, State#state.vars), State};
run_op({int, N}, State) -> {N, State}.

%%%
run_op_while(E1, E2, T, State) ->
	{V1, S1} = run_op(E1, State),
	case {V1, T} of
		{0, term} -> 0;
		{0, _} -> run_code(T, S1);
		_ ->
			{_Vx1, Sx1} = run_code(E2, S1),
			run_op_while(E1, E2, T, Sx1)
	end.

%%%
run_op_for(VR1, EC, EL, EB, T, State) ->
	{V1, S1} = run_op(EC, State),
	case {V1, T} of
		{0, term} -> VR1;
		{0, _} -> run_code(T, S1);
		_ ->
			{Vx1, Sx1} = run_code(EB, S1),
			{_Vx2, Sx2} = run_op(EL, Sx1),
			run_op_for(Vx1, EC, EL, EB, T, Sx2)
	end.

%%%
run_code(term, State) -> {0, State};
run_code({op_while, E1, E2, T}, State) ->
	run_op_while(E1, E2, T, State);
run_code({op_for, EI, EC, EL, EB, T}, State) ->
	{V1, S1} = run_op(EI, State),
	run_op_for(V1, EC, EL, EB, T, S1);
run_code({op_if, E1, ET, EF, T}, State) ->
	{V1, S1} = run_op(E1, State),
	{V2, S2} = case V1 of
		0 -> run_code(EF, S1);
		_ -> run_code(ET, S1)
	end,
	case T of
		term -> {V2, S2};
		_ -> run_code(T, S2)
	end;
run_code({op_block, E, T}, State) ->
	{V1, S1} = run_code(E, State),
	case T of
		term -> {V1, S1};
		_ -> run_code(T, S1)
	end;

run_code({op, E, term}, State) ->
	run_op(E, State);
run_code({op, E, T}, State) ->
	{_V1, S1} = run_op(E, State),
	run_code(T, S1).

run_code(Code) ->
	run_code(Code, #state{vars=dict:new()}).

%%%
main() -> main([]).

main([FName]) ->
	{ok, CBin} = file:read_file(FName),
	CList = binary:bin_to_list(CBin),
	Code = parse_code(CList),
	print_code(Code),
	Code2 = reduce_code(Code),
	print_code(Code2),
	{Result, _RState} = run_code(Code2),
	io:format("~p~n", [Result]).

