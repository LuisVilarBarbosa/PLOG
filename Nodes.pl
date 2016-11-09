:-use_module(library(lists)).

:- dynamic player/1.
:- dynamic state/2.

/*
Game legend:
- -> road
| -> road
X -> conduits
O -> space -> sp
A -> player 1 node -> n1
B -> player 1 unit -> u1
S -> player 2 node -> n2
T -> player 2 unit -> u2
 */

/* Players */
player(p1).
player(p2).

%matriznos(H), find_node(H, X, Y, n1).
matriznos([
	[sp, sp, u1, u1],
	[sp, sp, n1, n2],
	[sp, n2, sp, sp]
	]).

/* Board */
board([
	[' ', ' ', u1, u1, n1, u1, u1, ' ', ' '],
	[' ',  sp, sp, u1, u1, u1, sp,  sp, ' '],
	[ sp,  sp, sp, sp, u1, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, sp, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, sp, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, sp, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, u2, sp, sp,  sp,  sp],
	[' ',  sp, sp, u2, u2, u2, sp,  sp, ' '],
	[' ', ' ', u2, u2, n2, u2, u2, ' ', ' ']
	]).

intermediate_board([
	[' ', ' ', sp, sp, sp, u1, sp, ' ', ' '],
	[' ',  sp, u1, u1, n1, u1, sp,  sp, ' '],
	[ sp,  sp, sp, sp, u1, sp, sp,  sp,  sp],
	[ sp,  u1, sp, sp, sp, sp, sp,  u1,  sp],
	[ sp,  sp, sp, sp, u1, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, sp, sp, sp,  sp,  sp],
	[ sp,  sp, sp, u2, u2, sp, sp,  sp,  sp],
	[' ',  sp, sp, u2, n2, u2, sp,  sp, ' '],
	[' ', ' ', u2, u2, sp, u2, u2, ' ', ' ']
	]).

final_board([
	[' ', ' ', sp, sp, sp, sp, sp, ' ', ' '],
	[' ',  u1, sp, sp, sp, u1, sp,  sp, ' '],
	[ sp,  sp, u1, sp, u1, n1, sp,  sp,  sp],
	[ sp,  sp, sp, u1, n2, u1, sp,  sp,  sp],
	[ sp,  sp, sp, sp, u1, sp, sp,  sp,  sp],
	[ sp,  sp, u2, u1, sp, sp, sp,  u2,  sp],
	[ sp,  sp, u2, sp, sp, sp, sp,  sp,  sp],
	[' ',  sp, u2, u2, sp, sp, u2,  sp, ' '],
	[' ', ' ', sp, u2, sp, u2, sp, ' ', ' ']
	]).

/* State */
state(Player, board(Board)).

/* Display */
display_board(Board) :-
	board(Board), 
	display_board_rows(Board, Board),
	nl,
	nl .
	
display_board_rows([Row | []], _Board) :-
	display_board_middle_bottom_row(Row),
	nl,
	display_board_row_pieces(Row).
display_board_rows([Row | Other_rows], Board) :-	/* Display board up half */
	length(Board, All_board_length),
	length(Other_rows, Left_board_length),
	Left_board_length1 is Left_board_length + 1,
	Half_board_length is All_board_length >> 1,		/* integer division by 2 */
	Left_board_length1 > Half_board_length,
	display_board_row_pieces(Row), 
	nl, 
	display_board_middle_up_row(Row),
	nl,
	display_board_rows(Other_rows, Board).
display_board_rows([Row | Other_rows], Board) :-	/* Display board middle */
	length(Board, All_board_length),
	length(Other_rows, Left_board_length),
	Left_board_length1 is Left_board_length + 1,
	Half_board_length is All_board_length >> 1,		/* integer division by 2 */
	Left_board_length1 == Half_board_length,
	display_board_row_pieces(Row), 
	nl,
	display_board_rows(Other_rows, Board).
display_board_rows([Row | Other_rows], Board) :-	/* Display board bottom half */
	length(Board, All_board_length),
	length(Other_rows, Left_board_length),
	Left_board_length1 is Left_board_length + 1,
	Half_board_length is All_board_length >> 1,		/* integer division by 2 */
	Left_board_length1 < Half_board_length,
	display_board_middle_bottom_row(Row),
	nl, 
	display_board_row_pieces(Row), 
	nl,
	display_board_rows(Other_rows, Board).

translate(sp, 'O').
translate(n1, 'A').
translate(u1, 'B').
translate(n2, 'S').
translate(u2, 'T').
translate(X, X).
	
display_board_row_pieces([Piece | []]) :-
	translate(Piece, TPiece),
	write(TPiece).
display_board_row_pieces([' ' | Other_pieces]) :-
	write('  '),
	display_board_row_pieces(Other_pieces).
display_board_row_pieces([Piece | [' ' | _]]) :-
	translate(Piece, TPiece),
	write(TPiece).
display_board_row_pieces([Piece | Other_pieces]) :-
	translate(Piece, TPiece),
	write(TPiece),
	write('-'),
	display_board_row_pieces(Other_pieces).

display_board_middle_up_row([_Piece | []]) :-
	write('|').
display_board_middle_up_row([Piece | [' ' | _]]) :-
	Piece \== ' ',
	write('|X|\\').
display_board_middle_up_row([' ' | [Other_piece | Other_pieces]]) :-
	Other_piece \== ' ',
	write(' /'),
	display_board_middle_up_row(Other_pieces).
display_board_middle_up_row([' ' | Other_pieces]) :-
	write('  '),
	display_board_middle_up_row(Other_pieces).
display_board_middle_up_row([_Piece | Other_pieces]) :-
	write('|X'),
	display_board_middle_up_row(Other_pieces).

display_board_middle_bottom_row([_Piece | []]) :-
	write('|').
display_board_middle_bottom_row([Piece | [' ' | _]]) :-
	Piece \== ' ',
	write('|X|/').
display_board_middle_bottom_row([' ' | [Other_piece | Other_pieces]]) :-
	Other_piece \== ' ',
	write(' \\'),
	display_board_middle_bottom_row(Other_pieces).
display_board_middle_bottom_row([' ' | Other_pieces]) :-
	write('  '),
	display_board_middle_bottom_row(Other_pieces).
display_board_middle_bottom_row([_Piece | Other_pieces]) :-
	write('|X'),
	display_board_middle_bottom_row(Other_pieces).

/* Game logic */
game(Type) :-
	write('Player 1 (younger): '),
	read(Player1),
	write('Player 2 (older): '),
	read(Player2),
	retract(player(p1)),
	assert(player(Player1)),
	retract(player(p2)),
	assert(player(Player2)),
	retract(state(_, Board)),
	assert(state(Player1, Board)),	/* the youngest player begins the game */
	repeat,
		play(Type),
		verify_game_over,
	state(Player, _),
	next_player(Player, Winner),	/* 'play' changed the current player to the next, it is necessary to recover the prior player */
	show_results(Winner).

play(cc) :-
	retract(state(Player, Actual_board)),
%	best_move(Player, Actual_board, Best_board),
	display_board(Best_board),
	next_player(Player, Next),
	assert(state(Next, Best_board)),
	! .

% play(hh) :-

% play(ch) :-

% rule(Name, Player, Piece_orig_x, Piece_orig_y, Board, New_board) :-
/* Applicability pre-conditions verifications */
%	Caso peça: fazer verificaçao se pode mover para cima, esquerda direita baixo. E fazer o sinal na horizontal, vertical e diagonal. verificar se o espaço esta livre 
	check_signal(Board, Player, Piece_x, Piece_y):-
		get_piece(Board, Node1_x, Node1_y, n1), %obter as coordenadas dos nos
		get_piece(Board, Node2_x, Node2_y, n2),
		%nó1
		((check_signal_vertical(Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1);
		check_signal_horizontal(Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1);
		check_signal_diagonal(Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1)), %calcular a diagonal
		check_enemies_interruptingsignal(Board, Player, Piece_x, Piece_y, Node1_x, Node1_y, Signal_direction1));
		%nó2
		((check_signal_vertical(Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2);
		check_signal_horizontal(Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2);
		check_signal_diagonal(Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2)),
		check_enemies_interruptingsignal(Board, Player, Piece_x, Piece_y, Node2_x, Node2_y, Signal_direction2)).
		
	check_signal_horizontal(Xp, Yp, Xn, Yn, Signal_direction):-
		%se calhar dá para nao utilizar a Distance e fazer diretamente
		Distance is Yn - Yp,
		Xn = Xp,
		((Distance > 1, Signal_direction is right);
		(Distance < 1, Signal_direction is left)).
		
	check_signal_vertical(Xp, Yp, Xn, Yn, Signal_direction):-
		Distance is Xn - Xp,
		Yn = Yp,
		((Distance > 1, Signal_direction is up);
		(Distance < 1, Signal_direction is left)).
		
	check_signal_diagonal(X1, Y1, X2, Y2, Signal_direction):-
		Xdiference is X2 - X1,
		Ydiference is Y2 - Y1,
		(Xdiference > 1, Ydiference > 1, Signal_direction = diagonal_upright);
		(Xdiference < 1, Ydiference > 1, Signal_direction = diagonal_upleft);
		(Xdiference > 1, Ydiference < 1, Signal_direction = diagonal_downright);
		(Xdiference < 1, Ydiference < 1, Signal_direction = diagonal_downleft),
		Absxdiference is abs(Xdiference),
		Absydiference is abs(Ydiference),
		Absxdiference = Absydiference .
		
	check_enemies_interruptingsignal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction):-
		%como se verifica se é o p1 ou o p2?
		((Signal_direction = up; Signal_direction = down), 
		check_enemies_interruptingsignal_vertical(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction));
		
		((Signal_direction = left; Signal_direction = right), 
		check_enemies_interruptingsignal_horizontal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction));
		
		((Signal_direction = diagonal_downleft; Signal_direction = diagonal_downright; Signal_direction = diagonal_upleft; Signal_direction = diagonal_upright),
		check_enemies_interruptingsignal_diagonal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction)).
		
	check_enemies_interruptingsignal_vertical(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction):-
		Piece_x =\= Node_x,
		get_piece(Board, Node_x, Node_y, Enemy),
		%if Player p1, Enemy = u2...
		!, %nao tenho a certeza de como se usa isto
		(Signal_direction = up, Node_x2 is Node_x - 1);
		(Signal_direction = down, Node_x2 is Node_x + 1),
		check_enemies_interruptingsignal_vertical(Board, Player, Piece_x, Piece_y, Node_x2, Node_y, Signal_direction).
		
	check_enemies_interruptingsignal_horizontal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction):-
		Piece_y =\= Node_y,
		get_piece(Board, Node_x, Node_y, Enemy),
		%if Player p1, Enemy = u2...
		!, %nao tenho a certeza de como se usa isto
		Node_y2 is Node_y - 1,
		(Signal_direction = left, Node_y2 is Node_y - 1);
		(Signal_direction = right, Node_y2 is Node_y + 1),
		check_enemies_interruptingsignal_horizontal(Board, Player, Piece_x, Piece_y, Node_x, Node_y2, Signal_direction).
		
	check_enemies_interruptingsignal_diagonal(Board, Player, Piece_x, Piece_y, Node_x, Node_y, Signal_direction):-
		Piece_x =\= Node_x,
		Piece_y =\= Node_y,
		get_piece(Board, Node_x, Node_y, Enemy),
		%if Player p1, Enemy = u2...
		!, %nao tenho a certeza de como se usa isto
		((Signal_direction = diagonal_downleft, Node_y2 is Node_y + 1, Node_x2 is Node_x - 1);
		(Signal_direction = diagonal_downright, Node_y2 is Node_y + 1, Node_x2 is Node_x + 1);
		(Signal_direction = diagonal_upleft, Node_y2 is Node_y - 1, Node_x2 is Node_x - 1);
		(Signal_direction = diagonal_upright, Node_y2 is Node_y - 1, Node_x2 is Node_x + 1)),
		check_enemies_interruptingsignal_diagonal(Board, Player, Piece_x, Piece_y, Node_x2, Node_y2, Signal_direction).
		
	%falta a parte de pintar um sp e colocar a peça na nova posiçao
	rule(move_up, Player, Piece_orig_x, Piece_orig_y, Board, New_board):-
		check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
		get_piece(Board, Piece_orig_x, Piece_orig_y, Piece),
		Piece_orig_y2 is Piece_orig_y + 1,
		get_piece(Board, Piece_orig_x, Piece_orig_y2, sp),
		set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board)
		set_piece(Board, Piece_orig_x, Piece_orig_y2, Piece, New_board).
				
	rule(move_down, Player, Piece_orig_x, Piece_orig_y, Board, New_board):-
		check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
		get_piece(Board, Piece_orig_x, Piece_orig_y, Piece),
		Piece_orig_y2 is Piece_orig_y - 1,
		get_piece(Board, Piece_orig_x, Piece_orig_y2, sp),
		set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board)
		set_piece(Board, Piece_orig_x, Piece_orig_y2, Piece, New_board).
		
	rule(move_left, Player, Piece_orig_x, Piece_orig_y, Board, New_board):-
		check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
		get_piece(Board, Piece_orig_x, Piece_orig_y, Piece),
		Piece_orig_x2 is Piece_orig_x - 1,
		get_piece(Board, Piece_orig_x2, Piece_orig_y, sp),
		set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board)
		set_piece(Board, Piece_orig_x2, Piece_orig_y, Piece, New_board).
		
	rule(move_right, Player, Piece_orig_x, Piece_orig_y, Board, New_board):-
		check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
		get_piece(Board, Piece_orig_x, Piece_orig_y, Piece),
		Piece_orig_x2 is Piece_orig_x + 1,
		get_piece(Board, Piece_orig_x2, Piece_orig_y, sp),
		set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board)
		set_piece(Board, Piece_orig_x2, Piece_orig_y, Piece, New_board).
		
	rule(jump_up_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board):-
		check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
		get_piece(Board, Piece_orig_x, Piece_orig_y, Piece),
		((Piece = u2, Next_piece is u1); (Piece = u1, Next_piece is u2)),
		Piece_orig_y2 is Piece_orig_y + 1,
		get_piece(Board, Piece_orig_x, Piece_orig_y2, New_piece),
		Piece_orig_y3 is Piece_orig_y + 2,
		get_piece(Board, Piece_orig_x, Piece_orig_y3, sp),
		set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board)
		set_piece(Board, Piece_orig_x, Piece_orig_y3, Piece, New_board).
		
	rule(jump_down_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board):-
		check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
		get_piece(Board, Piece_orig_x, Piece_orig_y, Piece),
		((Piece = u2, Next_piece is u1); (Piece = u1, Next_piece is u2)),
		Piece_orig_y2 is Piece_orig_y - 1,
		get_piece(Board, Piece_orig_x, Piece_orig_y2, New_piece),
		Piece_orig_y3 is Piece_orig_y - 2,
		get_piece(Board, Piece_orig_x, Piece_orig_y3, sp),
		set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board)
		set_piece(Board, Piece_orig_x, Piece_orig_y3, Piece, New_board).
		
	rule(jump_left_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board):-
		check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
		get_piece(Board, Piece_orig_x, Piece_orig_y, Piece),
		((Piece = u2, Next_piece is u1); (Piece = u1, Next_piece is u2)),
		Piece_orig_x2 is Piece_orig_x - 1,
		get_piece(Board, Piece_orig_x2, Piece_orig_y, New_piece),
		Piece_orig_x3 is Piece_orig_x - 2,
		get_piece(Board, Piece_orig_x3, Piece_orig_y, sp),
		set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board)
		set_piece(Board, Piece_orig_x3, Piece_orig_y, Piece, New_board).
		
	rule(jump_right_enemy_unit, Player, Piece_orig_x, Piece_orig_y, Board, New_board):-
		check_signal(Board, Player, Piece_orig_x, Piece_orig_y),
		get_piece(Board, Piece_orig_x, Piece_orig_y, Piece),
		((Piece = u2, Next_piece is u1); (Piece = u1, Next_piece is u2)),
		Piece_orig_x2 is Piece_orig_x + 1,
		get_piece(Board, Piece_orig_x2, Piece_orig_y, New_piece),
		Piece_orig_x3 is Piece_orig_x + 2,
		get_piece(Board, Piece_orig_x3, Piece_orig_y, sp),
		set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board)
		set_piece(Board, Piece_orig_x3, Piece_orig_y, Piece, New_board).
	

	
	/* action / movement */
%	set_piece(Board, Piece_orig_x, Piece_orig_y, sp, New_board2),
%	set_piece(New_board2, Piece_new_x, Piece_new_y, Piece, New_board).

get_piece(Board, X, Y, Piece):-
	nth1(Y, Board, Line),
	nth1(X, Line, Piece).

best_move(Player, Board, Best) :-
	findall(Aux_board, rule(_, Player, _, _, Board, Aux_board), Possible_boards),
	select_best(Possible_boards, Best).

quality(Board, Player, Value).

select_best(Possible_boards, Best) :-
	findall(Aux_board_value, quality(Aux_board, Player, Value), List_boards_values).

verify_game_over :-
	state(_, Board),
	find_node(Board, X, Y, Node),
	verify_blocked(Board, Node, X, Y).

find_node(Board, X, Y, Node) :-
	find_node_aux(Board, 1, X, Y, Node).

find_node_aux([Line | Other_lines], Pos_y, X, Y, Node) :-
	((nth1(X, Line, n1), Node = n1); (nth1(X, Line, n2), Node = n2)),
	Y = Pos_y.
find_node_aux([Line | Other_lines], Pos_y, X, Y, Node) :-
	Next_pos_y is Pos_y + 1,
	find_node_aux(Other_lines, Next_pos_y, X, Y, Node).

verify_blocked(Board, Node, X, Y) :-
	((Node = n1, Enemy_unit = u2); (Node = n2, Enemy_unit = u1)),
	verify_blocked_left(Board, Enemy_unit, X, Y),
	verify_blocked_right(Board, Enemy_unit, X, Y),
	verify_blocked_up(Board, Enemy_unit, X, Y),
	verify_blocked_down(Board, Enemy_unit, X, Y).

/* if 'X' or 'Y' out of borders 'yes' will be returned */
verify_blocked_left(Board, Enemy_unit, X, Y) :- X2 is X - 1, (X2 < 1; (nth1(Y, Board, Line), nth1(X2, Line, Enemy_unit))).
verify_blocked_right(Board, Enemy_unit, X, Y) :- X2 is X + 1, (nth1(Y, Board, Line), length(Line, Length_x), (X2 > Length_x; nth1(X2, Line, Enemy_unit))).
verify_blocked_up(Board, Enemy_unit, X, Y) :- Y2 is Y - 1, (Y2 < 1; (nth1(Y2, Board, Line), nth1(X, Line, Enemy_unit))).
verify_blocked_down(Board, Enemy_unit, X, Y) :- Y2 is Y + 1, length(Board, Length_y), (Y2 > Length_y; (nth1(Y2, Board, Line), nth1(X, Line, Enemy_unit))).

next_player(Player, Next) :- player(Player), player(Next), Player \= Next.

show_results(Winner) :- format('~NWinner: ~s~N', Winner).

/* if 'Y' is invalid, the same list will be returned */
set_piece([Line | Other_lines], X, Y, New_piece, [Line | Other_new_lines]) :-
	Y =\= 1,
	Next_Y is Y - 1,
	set_piece(Other_lines, X, Next_Y, New_piece, Other_new_lines).
set_piece([Line | Other_lines], X, 1, New_piece, [New_line | Other_new_lines]) :-
	set_cell(X, New_piece, Line, New_line),
	Next_Y is 0,
	set_piece(Other_lines, X, Next_Y, New_piece, Other_new_lines).
set_piece([], _, _, _, []).

/* if 'X' is invalid, the same list will be returned */
set_cell(X, New_piece, [Piece | Rest_line], [Piece | Rest_new_line]) :-
	X =\= 1,
	Next_X is X - 1,
	set_cell(Next_X, New_piece, Rest_line, Rest_new_line).
set_cell(1, New_piece, [Piece | Rest_line], [New_piece | Rest_new_line]) :-
	Next_X is 0,
	set_cell(Next_X, New_piece, Rest_line, Rest_new_line).
set_cell(_, _, [], []).
