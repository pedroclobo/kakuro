% Pedro Lobo - ist199115

:- [codigo_comum].

% combinacoes_soma(N, Els, Soma, Combs)
/* Combs eh a lista ordenada cujos elementos sao as combinacoes N a N, dos
 * elementos de Els cuja soma eh Soma. */
combinacoes_soma(N, Els, Soma, Combs) :-
	setof(Comb, (combinacao(N, Els, Comb), sum_list(Comb, Soma)), Combs).


% permutacoes_soma(N, Els, Soma, Perms)
/* Perms eh a lista ordenada cujos elementos sao as permutacoes das combinacoes
 * N a N, dos elementos de Els cuja soma eh Soma. */
permutacoes_soma(N, Els, Soma, Perms) :-

	% Encontra as permutacoes de cada combinacao
	findall(Perm,
	(combinacoes_soma(N, Els, Soma, Combs), member(Comb, Combs), permutation(Comb, Perm)),
	Temp),

	% Ordena as permutacoes
	setof(Perm, member(Perm, Temp), Perms).


% espaco_fila(Fila, Esp, H_V)
/* em que Fila eh uma fila (linha ou coluna) de um puzzle e H_V eh um dos
 * atomos h ou v, conforme se trate de uma fila horizontal ou vertical,
 * respectivamente, significa que Esp eh um espaco de Fila. */
espaco_fila(Fila, Esp, 'h') :-

	% Iterando sobre todas as listas da fila Agrupada,
	% cria o espaco com a lista e variaveis correspondentes
	obtem_indices(Fila, Inds), obtem_intervalos(Inds, Ints), agrupa_fila(Fila, Fila_Ag, Ints),
	member(M, Fila_Ag),
	M = [Nums|Vars],
	nth1(2, Nums, Num),
	Esp = espaco(Num, Vars).

espaco_fila(Fila, Esp, 'v') :-
	obtem_indices(Fila, Inds), obtem_intervalos(Inds, Ints), agrupa_fila(Fila, Fila_Ag, Ints),
	member(M, Fila_Ag),
	M = [Nums|Vars],
	nth1(1, Nums, Num),
	Esp = espaco(Num, Vars).


% obtem_indices(Fila, Inds)
/* Fila eh uma fila e Inds eh os indices ocupados pelas listas validas de Fila */
obtem_indices(Fila, Inds) :-

	% Lista com todas as listas invalidas
	findall(M, (nextto(M, N, Fila), is_list(M), is_list(N)), Inv),

	% Indices das listas validas, acrescentada com o tamanho da lista+1
	findall(Pos, (nth1(Pos, Fila, O), is_list(O), \+member(O, Inv)), Temp),
	length(Fila, L), L_mais_1 is L+1, append(Temp, [L_mais_1], Inds).


% obtem_intervalos(Inds, Ints)
/* Inds eh uma lista de indices e Ints eh a lista formada pelos intervalos
 * dos indices. ex. Inds = [1,2,3], Ints = [[1,2], [2,3]] */
obtem_intervalos(Inds, Ints) :-
	findall(Int, obtem_intervalos_aux(Inds, Int), Ints).

obtem_intervalos_aux(Inds, Int) :-

	% Itera sobre todos os indices, prefazendo pares de indices
	length(Inds, L), L_menos_1 is L-1, between(1, L_menos_1, Ind),
	nth1(Ind, Inds, Ind1), Ind_mais_1 is Ind+1, nth1(Ind_mais_1, Inds, Ind2),
	Int = [Ind1, Ind2].


% agrupa_fila(Fila, Fila_S, Ints)
/* Fila eh uma fila, Ints eh uma lista de intervalos e Fila_S eh a fila com
 * listas agrupadas de listas e variaveis */
agrupa_fila(Fila, Fila_Ag, Ints) :-
	bagof(Fila_S, agrupa_fila_aux(Fila, Fila_S, Ints), Fila_Ag).

agrupa_fila_aux(Fila, Fila_S, Ints) :-

	% Itera sobre todos os pares, definindo um limite inferior e superior de indices
	member(Par, Ints), nth1(1, Par, Inf), nth1(2, Par, Sup),

	% Forma a lista com todas as variaveis entre as duas listas com indices
	% Inf e Sup
	bagof(M, Pos^(nth1(Pos, Fila, M), \+is_list(M), Inf < Pos, Pos < Sup), Vars),
	nth1(Inf, Fila, Lst),

	Fila_S = [Lst|Vars].


% espacos_fila(H_V, Fila, Espacos)
/* em que Fila eh uma fila (linha ou coluna) de uma grelha e H_V eh um dos
 * atomos h ou v, significa que Espacos eh a lista de todos os espacos de
 * Fila, da esquerda para a direita */
espacos_fila(H_V, Fila, Espacos) :-
	bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos).

% numeros_comuns(Lst_Perms, Numeros_comuns)
/* Lst_Perms eh uma lista de permutacoes e Numeros_comuns eh uma lista de pares
 * (pos, numero), em que as listas de Lst_Perms contem numero na posicao pos. */
numeros_comuns(Lst_Perms, Numeros_comuns) :-

	% Lista de pares (pos, numero) de todas as permutacoes
	findall((Pos, Num), (member(L, Lst_Perms), nth1(Pos, L, Num)), Temp),

	% Lista de pares comuns a todas as permutacoes
	% Caso nada seja comum, Numeros_comuns eh a lista vazia
	length(Lst_Perms, N),
	setof(Par, Aux ^
	(member(Par, Temp), intersection(Temp, [Par], Aux), length(Aux, N)),
	Numeros_comuns); Numeros_comuns = [].
