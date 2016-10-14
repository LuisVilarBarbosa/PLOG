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
	display_board_rows(Board).
	
display_board_rows([Row | []]) :-
	display_board_row_pieces(Row).
display_board_rows([Row | Other_rows]) :-
	display_board_row_pieces(Row), 
	nl, 
	display_board_middle_up_row(Row),
	nl,
	display_board_rows(Other_rows).

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
display_board_middle_up_row([' ' | [Other_piece | Other_pieces]]) :-
	Other_piece \== ' ',
	write('/ '),
	display_board_middle_up_row(Other_pieces).
display_board_middle_up_row([' ' | Other_pieces]) :-
	write('  '),
	display_board_middle_up_row(Other_pieces).
display_board_middle_up_row([Piece | [' ' | Other_pieces]]) :-	/* how it works? */
	write('|X'),
	display_board_middle_up_row(Other_pieces).
display_board_middle_up_row([Piece | Other_pieces]) :-
	write('|X'),
	display_board_middle_up_row(Other_pieces).

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
