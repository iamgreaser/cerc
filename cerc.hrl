-record(rstate, {
	vars}).
-record(cstate, {
	mem,
	pcstart, pc,
	heapstart=0, heappos=0, heapname, heapfix=[],
	cpu}).

-define(is_ass(X), (
	X == ass orelse
	X == assadd orelse
	X == asssub orelse
	X == assmul orelse
	X == assquo orelse
	X == assmod orelse
	X == assshl orelse
	X == assshr)).

