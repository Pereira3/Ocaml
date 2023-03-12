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
		| _ -> ( let res =  Z.div (Z.add (Z.mul(Z.add (Z.mul (n)(Z.of_int 2)) (Z.one))( eq(Z.sub(n)(Z.one)) )) (Z.mul(Z.sub (Z.mul (n)(Z.of_int 3)) (Z.of_int 3))( eq(Z.sub(n)(Z.of_int 2)) ))) (Z.add n (Z.of_int 2)) in
		Hashtbl.add ht n res; res))
	else Hashtbl.find ht n

(*Conversão do tipo int para Z.int e apresenta o valor final*)
let () = Z.print(eq (Z.of_int num));
print_string("\n")