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
