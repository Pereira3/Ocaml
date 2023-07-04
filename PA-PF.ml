(*Ricardo Pereira nº 48554*)
(*https://en.wikipedia.org/wiki/Motzkin_number / https://ocaml.org/docs/hash-tables*)
(*Leitura do nº inteiro inserido*)
let num = read_int()
(*Validação do valor de num inserido*)
let () = if num < 0 || num > 10000 then invalid_arg "Valor inserido inválido, o valor do mesmo tem de estar presente entre [0, 10000]!"
(*Para valores grandes o programa demora muito tempo, criar uma Hashtable e insere os valores em cada instancia para consultar na recursiva*)
let ht = Hashtbl.create num

(*Função recursiva, caso o resultado da expressão associado a um dado n não esteja na hashtable ele verifica os valores,
	caso n seja 0 ou 1, ele dá print ao resultado visto que não precisa de executar a fórmula, caso n seja outro valor válido
	ele executa a fórmula e adiciona o resultado à hashtable do n associado a esse resultado
	Caso o n já tenha um resultado associado na hashtable, apenas dá return a ele mesmo.*)
let rec eq n =
	if (Hashtbl.mem ht n) = false then
		(match (Z.to_int n) with
		| 0 -> Z.one
		| 1 -> Z.one
		| _ -> ( let res = Z.add ( eq(n-1) eq(n-2) ) in
		Hashtbl.add ht n res; res))
	else Hashtbl.find ht n

(*Conversão do tipo int para Z.int e apresenta o valor final*)
let () = Z.print(eq (Z.of_int num));
print_string("\n")

(*Após a inicialização do programa, o mesmo fica à espera que o utilizador introduza um valor numérico.
	Após a introdução desse valor numérico, guardado posteriormente na variável num, o programa certifica-se de que o valor de num é válido para o intervalo definido.
	De modos ao programa ser mais eficiente e mais rápido é criada um hashtable e de modo recursivo, os valores correspondentes aos resultados da fórmula dentro da função recursiva vão sendo guardados na hashtable.
	Para efeitos de rapidez, quando num é 1 ou 0 o programa dá logo o output direto, visto que para ambos os casos o resultado é 1.
	Caso um valor já tenha sido calculado, o programa apenas recorre à hashtable e procura o resultado, prosseguindo com a fórmula ou prosseguindo assim para a apresentação do resultado.
	
	Caso o utilizador tenha escrito 3
	Inicialmente é corrida a fórmula e fica (7*eq2 + 6*eq1)/5
	De seguinda, como eq1 é 1, [7*((5*eq1 + 3*eq0)/4) +6]/5
	Como eq0 é 1,  [7*((5*1+3*1)/4) +6]/5 
	Resultado: 4

	Conforme o programa vai calculando, vai guardado os resultados das operações efetuadas.
	No exemplo anterior, conforme o programa foi percorrendo o eq de 2, 1 e 0, foi guardando os seus respetivos resultados na hashtable, visto que 1 e 0 não chegam à terceira parte do ciclo if, a hashtable apenas ficou com o valor de eq2 guardado.*)
