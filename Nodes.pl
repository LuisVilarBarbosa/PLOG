:-use_module(library(lists)).

/* Game pieces */
road('-').
conduits('X').
node('N').
unit('U').
space('O').

/* Board */
board([
	[' ', ' ', 'O', 'O', 'O', 'O', 'O', ' ', ' '],
	[' ', 'O', 'O', 'O', 'O', 'O', 'O', 'O', ' '],
	['O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O'],
	['O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O'],
	['O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O'],
	['O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O'],
	['O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O'],
	[' ', 'O', 'O', 'O', 'O', 'O', 'O', 'O', ' '],
	[' ', ' ', 'O', 'O', 'O', 'O', 'O', ' ', ' ']
	]).

/* Display */
display_board(board) :-	/* board is not a variable so no question is asked */
	board(Board), 
	display_board_rows(Board, Board).
	
display_board_rows([Row | []], Board) :-
	display_board_middle_bottom_row(Row),
	nl,
	display_board_row_pieces(Row).
display_board_rows([Row | Other_rows], Board) :-	/* Display board up half */
	length(Board, All_board_length),
	length(Other_rows, Left_board_length),
	Left_board_length1 is Left_board_length + 1,
	Half_board_length is All_board_length >> 1,		% integer division by 2
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
	Half_board_length is All_board_length >> 1,		% integer division by 2
	Left_board_length1 == Half_board_length,
	display_board_row_pieces(Row), 
	nl,
	display_board_rows(Other_rows, Board).
display_board_rows([Row | Other_rows], Board) :-	/* Display board bottom half */
	length(Board, All_board_length),
	length(Other_rows, Left_board_length),
	Left_board_length1 is Left_board_length + 1,
	Half_board_length is All_board_length >> 1,		% integer division by 2
	Left_board_length1 < Half_board_length,
	display_board_middle_bottom_row(Row),
	nl, 
	display_board_row_pieces(Row), 
	nl,
	display_board_rows(Other_rows, Board).

display_board_row_pieces([Piece | []]) :-
	write(Piece).
display_board_row_pieces([' ' | Other_pieces]) :-
	write('  '),
	display_board_row_pieces(Other_pieces).
display_board_row_pieces([Piece | [' ' | _]]) :-
	write(Piece).
display_board_row_pieces([Piece | Other_pieces]) :-
	write(Piece),
	write('-'),
	display_board_row_pieces(Other_pieces).

display_board_middle_up_row([Piece | []]):-
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
display_board_middle_up_row([Piece | [' ' | Other_pieces]]) :-	%how it works? 
	write('|X'),
	display_board_middle_up_row(Other_pieces).
display_board_middle_up_row([Piece | Other_pieces]) :-
	write('|X'),
	display_board_middle_up_row(Other_pieces).

display_board_middle_bottom_row([Piece | []]):-
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
display_board_middle_bottom_row([Piece | [' ' | Other_pieces]]) :-	%how it works? 
	write('|X'),
	display_board_middle_bottom_row(Other_pieces).
display_board_middle_bottom_row([Piece | Other_pieces]) :-
	write('|X'),
	display_board_middle_bottom_row(Other_pieces).

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
	translate(Elem,TElem), %este traduz o Elem e guarda em Elem
	write(TElem),
	print_line(Rest).
	
translate(0, ' ').
translate(1, 'X').
translate(2, '0').
*/
