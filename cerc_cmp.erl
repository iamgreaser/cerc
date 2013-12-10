% LL(*) C-like parser + interpreter
% 2013, Ben "GreaseMonkey" Russell -- Public Domain

-module(cerc_cmp).
-export([build_code/2, em8/2, em16le/2, heap_var/2]).
-include("cerc.hrl").

%%%
heap_var_noalloc(S, S0) ->
	{dict:fetch(S, S0#cstate.heapname), S0}.

heap_var(S, S0) ->
	case dict:is_key(S, S0#cstate.heapname) of
		true -> heap_var_noalloc(S, S0);
		false ->
			SHeap = dict:store(S,
				S0#cstate.heappos,
				S0#cstate.heapname),
			S1 = S0#cstate{
				heappos = S0#cstate.heappos + 2,
				heapname = SHeap},
			{S0#cstate.heappos, S1}
	end.

%%%
em8(S0, Byte) when Byte >= 16#00 andalso Byte =< 16#FF ->
	S0#cstate{mem = array:set(S0#cstate.pc, Byte, S0#cstate.mem),
		pc = S0#cstate.pc + 1}.
em16le(S0, Word) ->
	S1 = em8(S0, Word band 16#FF),
	em8(S1, Word bsr 8).

%%%
fix_code(S0, [{imm16le, P, Name} | T], RealPC) ->
	S1 = S0#cstate{pc = P},
	{Var, S2} = heap_var(Name, S1),
	S3 = em16le(S2, Var + S2#cstate.heapstart),
	fix_code(S3, T, RealPC);
fix_code(S0, [], RealPC) ->
	S0#cstate{pc = RealPC}.

fix_code(S0) ->
	fix_code(S0, S0#cstate.heapfix, S0#cstate.pc).

spew_code(PC, PC, _Mem, Acc) ->
	Acc;
spew_code(PC, PCEnd, Mem, Acc) when PC < PCEnd ->
	spew_code(PC + 1, PCEnd, Mem,
		<<Acc/binary, (array:get(PC, Mem))>>).
	

%%%
build_cstate() ->
	#cstate{mem=array:new(),
		heapname=dict:new()}.

build_cstate(i8086) ->
	State = build_cstate(),
	State#cstate{pcstart=16#0100,
		pc=16#0100,
		cpu=i8086}.

%%%
build_code(i8086, Code, State) ->
	cerc_cmp_x86:build_code(Code, State).

build_code(Arch, Code) ->
	S0 = build_code(Arch, Code, build_cstate(Arch)),
	S1 = S0#cstate{heapstart = S0#cstate.pc},
	S2 = fix_code(S1),
	spew_code(S2#cstate.pcstart, S2#cstate.pc, S2#cstate.mem, <<>>).

