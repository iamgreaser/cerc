% LL(*) C-like parser + interpreter
% 2013, Ben "GreaseMonkey" Russell -- Public Domain

-module(cerc_print).
-export([print_code/1]).
-include("cerc.hrl").

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


