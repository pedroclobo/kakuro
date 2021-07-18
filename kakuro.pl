% Pedro Lobo - ist199115

:- [codigo_comum].

% combinacoes_soma(N, Els, Soma, Combs)
/* Combs eh a lista ordenada cujos elementos sao as combinacoes N a N, dos
 * elementos de Els cuja soma eh Soma.
*/
combinacoes_soma(N, Els, Soma, Combs) :-
	setof(Comb, (combinacao(N, Els, Comb), sum_list(Comb, Soma)), Combs).


% permutacoes_soma(N, Els, Soma, Perms)
/* Perms eh a lista ordenada cujos elementos sao as permutacoes das combinacoes
 * N a N, dos elementos de Els cuja soma eh Soma.
*/
permutacoes_soma(N, Els, Soma, Perms) :-

	% Encontra as permutacoes de cada combinacao
	findall(Perm,
	(combinacoes_soma(N, Els, Soma, Combs), member(Comb, Combs), permutation(Comb, Perm)),
	Temp),

	% Ordena as permutacoes
	setof(Perm, member(Perm, Temp), Perms).


% numeros_comuns(Lst_Perms, Numeros_comuns)
/* Lst_Perms eh uma lista de permutacoes e Numeros_comuns eh uma lista de pares
 * (pos, numero), em que as listas de Lst_Perms contem numero na posicao pos.
*/
numeros_comuns(Lst_Perms, Numeros_comuns) :-

	% Lista de pares (pos, numero) de todas as permutacoes
	findall((Pos, Num), (member(L, Lst_Perms), nth1(Pos, L, Num)), Temp),

	% Lista de pares comuns a todas as permutacoes
	% Caso nada seja comum, Numeros_comuns eh a lista vazia
	length(Lst_Perms, N),
	setof(Par, Aux ^
	(member(Par, Temp), intersection(Temp, [Par], Aux), length(Aux, N)),
	Numeros_comuns); Numeros_comuns = [].
