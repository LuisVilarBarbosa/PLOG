:-use_module(library(lists)).

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
 
/* Instrucoes
display_board(board). - desenha o tabuleiro atual
*/

/* Players */
player(p1).
player(p2).

/* Changes player */
next_player(p1, p2).
next_player(p2, p1).

/* Board */
board([
	[' ', ' ', u1, u2, n1, u3, u4, ' ', ' '],
	[' ',  sp, sp, u5, u6, u7, sp,  sp, ' '],
	[ sp,  sp, sp, sp, u8, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, sp, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, sp, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, sp, sp, sp,  sp,  sp],
	[ sp,  sp, sp, sp, x8, sp, sp,  sp,  sp],
	[' ',  sp, sp, x5, x6, x7, sp,  sp, ' '],
	[' ', ' ', x1, x2, n2, x3, x4, ' ', ' ']
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

coordboard([
	[(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(1,7),(1,8),(1,9)],
	[(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(2,7),(2,8),(2,9)],
	[(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9)],
	[(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(4,7),(4,8),(4,9)],
	[(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(5,7),(5,8),(5,9)],
	[(6,1),(6,2),(6,3),(6,4),(6,5),(6,6),(6,7),(6,8),(6,9)],
	[(7,1),(7,2),(7,3),(7,4),(7,5),(7,6),(7,7),(7,8),(7,9)],
	[(8,1),(8,2),(8,3),(8,4),(8,5),(8,6),(8,7),(8,8),(8,9)],
	[(9,1),(9,2),(9,3),(9,4),(9,5),(9,6),(9,7),(9,8),(9,9)]
	]).
	
/* Display */
display_board(board) :-
	board(Board), 
	display_board_rows(Board, Board).
	
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
	
/*
Translates
*/
translate(sp, 'O').
translate(n1, 'A').
translate(u1, 'B').
translate(u2, 'B').
translate(u3, 'B').
translate(u4, 'B').
translate(u5, 'B').
translate(u6, 'B').
translate(u7, 'B').
translate(u8, 'B').
translate(n2, 'S').
translate(x1, 'T').
translate(x2, 'T').
translate(x3, 'T').
translate(x4, 'T').
translate(x5, 'T').
translate(x6, 'T').
translate(x7, 'T').
translate(x8, 'T').
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
		
/* Utilitarios */
validPosition(X, Y):- 	
	X >= 1, X =< 9,
	Y >= 1, Y =< 9.
	
%talvez precisamos de um getPiece().
getPiece(X, Y):-
	board(X, Y).
getPiece(X, Y, Result):-
	Result is board(X, Y).
	
isFree(X,Y):-
	getPiece(X,Y,Piece),
	Piece \== ' ',
	Piece == sp.

/* Movimentos */
vertical(X1, X2):-
	X1 is X2.
vertical(X1, _, X2, _):-
	vertical(X1, X2),
	isFree(X1, X2).

horizontal(Y1, Y2):-
	Y1 is Y2.
horizontal(_, Y1, _, Y2):-
	horizontal(Y1, Y2),
	isFree(Y1, Y2).
	

move_Up(Jogador, BoardIn, BoardOut):-
	move_piece(up, Jogador, BoardIn, BoardOut).
move_Right(Jogador, BoardIn, BoardOut):-
	move_piece(right, Jogador, BoardIn, BoardOut).
move_piece(Direction, Jogador, BoardIn, BoardOut):-
	Direction = right,
	Jogador = p1; Jogador = p2,
	
member(X, [Y|T]) :- 
	X = Y; member(X, T).
	
move_piece(X, Y, XTarget, YTarget):-
	validPosition(X, Y),
	validPosition(XTarget, YTarget),
	Piece is getPiece(X,Y),
	horizontal(X, Y, XTarget, YTarget),
	vertical(X, Y, XTarget, YTarget).
	
	/*
indexOfPiece([Row|_], Piece, IndexX, IndexY):-
	member(Piece, Row),
	!.
indexOfPiece([_|Other_rows], Piece, IndexX, IndexY):- 
	indexOf(Row, Piece, IndexX),
	indexOfPiece(Other_rows, Piece, IndexX, IndexY1),
	!,
	IndexY is IndexY1+1.
	*/
indexOfPiece(Board, Piece, IndexX):- 
	member(Row, Board), 
	indexOf(Row, Piece, IndexX).
	
indexOf([Piece|_], Piece, _):- !.
indexOf([_|Other_rows], Piece, Index):-
	indexOf(Other_rows, Piece, Index1),
	!,
	Index is Index1+1.
	
%depois podemos mudar para mudar diretamente a peça do jogador atual
move_piece(Player, Direction):-
	board(p1).
move_nod(p1, Direction):-
	move_piece(n1, Direction).

/* 
recomendaçoes do prof: 
	colocar numeros nas linhas e colunas
	fazer translate do board
	
read(X).
write(X).
get_char(X).	'X'
get_code(X).	96
put_char(X).
put_code(X).

print_board([]). para acabar de imprimir o tabuleiro
print_board([Line|Rest]):-
	nl,
	print_line(Line),
	print_board(Rest).

print_line([]).
print_line([Elem|Rest]):-
	translate(Elem,TElem), %este traduz o Elem e guarda em TElem
	write(TElem),
	print_line(Rest).

*/

/* Links */
%http://stackoverflow.com/questions/4380624/how-compute-index-of-element-in-a-list