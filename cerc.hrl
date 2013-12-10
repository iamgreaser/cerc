-record(rstate, {
	vars}).
-record(cstate, {
	mem,
	pcstart, pc,
	heapstart=0, heappos=0, heapname, heapfix=[],
	cpu}).

