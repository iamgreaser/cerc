% LL(*) C-like parser + interpreter
% 2013, Ben "GreaseMonkey" Russell -- Public Domain

-module(cerc).
-export([main/1,main/0]).
-include("cerc.hrl").

%%%
main() -> main([]).

main([FName]) ->
	{ok, CBin} = file:read_file(FName),
	CList = binary:bin_to_list(CBin),
	Code = cerc_parse:parse_code(CList),
	cerc_print:print_code(Code),
	Code2 = cerc_run:reduce_code(Code),
	cerc_print:print_code(Code2),
	{Result, _RState} = cerc_run:run_code(Code2),
	io:format("~p~n", [Result]).

