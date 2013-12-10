% LL(*) C-like parser + interpreter
% 2013, Ben "GreaseMonkey" Russell -- Public Domain

-module(cerc_run).
-export([reduce_code/1,run_code/1]).
-include("cerc.hrl").

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
	{VR, State#rstate{vars=dict:store(S, VR, State#rstate.vars)}}.

%%%
run_op({binop, ass, {nam, S}, E2}, State) ->
	{V2, S2} = run_op(E2, State),
	{V2, S2#rstate{vars=dict:store(S, V2, S2#rstate.vars)}};
	
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
			Vx = dict:fetch(S, State#rstate.vars) + 1,
			S2 = State#rstate{vars = dict:store(S, Vx, State#rstate.vars)},
			{Vx, S2};
		predec ->
			Vx = dict:fetch(S, State#rstate.vars) - 1,
			S2 = State#rstate{vars = dict:store(S, Vx, State#rstate.vars)},
			{Vx, S2};
		postinc ->
			Vx = dict:fetch(S, State#rstate.vars),
			S2 = State#rstate{vars = dict:store(S, Vx + 1, State#rstate.vars)},
			{Vx, S2};
		postdec ->
			Vx = dict:fetch(S, State#rstate.vars),
			S2 = State#rstate{vars = dict:store(S, Vx - 1, State#rstate.vars)},
			{Vx, S2};
		_ -> run_op({unop, Mode, {int, dict:fetch(S, State#rstate.vars)}}, State)
	end;
run_op({unop, Mode, E}, State) ->
	{V1, S1} = run_op(E, State),
	VR = case Mode of
		neg -> -V1;
		grp -> V1
	end,
	{VR, S1};
run_op({nam, S}, State) -> 
	{dict:fetch(S, State#rstate.vars), State};
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
	run_code(Code, #rstate{vars=dict:new()}).

