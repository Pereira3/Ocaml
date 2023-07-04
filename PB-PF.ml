(*Ricardo Pereira nº 48554*)
(*https://v2.ocaml.org/api/Stdlib.html*)
(*Leitura dos nº inteiros inseridos*)
let num_fatias = read_int()
let num_precos = read_int()

(*Validação dos valores inseridos*)
let () = if num_fatias < 0 || num_precos < 0 || num_fatias > 10000 || num_precos > 10000 || num_fatias < num_precos then invalid_arg "Valores inseridos inválidos!"

(*Guardar valores dos preços, caso num_fatia tenha algum valor saltado, este ficará com o valor num_preço inicializado a 0*)
let array_precos = Array.make (num_fatias+1) 0
(*Array que futuramente terá os valores máximos para cada valor de num_fatias guardado em cada um dos seus index*)
let array_vm = Array.make (num_fatias+1) 0
(*Valor máximo utilizado futuramente para a comparação dos valores sucessivos relativos aos preços por num_fatias*)
let valor_max = ref 0

(*Ciclo para introduzir os valores no array, como o array começa em 0 o index será o valor1, sendo o primeiro valor 0.
O Ciclo é efetuado tantas vezes como num_precos-1, isto porque apenas as fatias com preço importam, e como o ciclo começa em 0
num_preco-1 será a quantidade de vezes suficientes para preencher o array.*)
let () = 
	for i = 0 to num_precos-1 do 
		let valor1, valor2 = 
			Scanf.scanf " %d %d" (fun valor1 valor2 -> (valor1, valor2)) in
			array_precos.(valor1) <- valor2;
	done

(*O Ciclo será efetuado tantas vezes como a quantidade de fatias.
Após a execução do primeiro for, é devolvido o valor da variável relativa ao valor máximo e será inserido no array dos valores máximos.
Após a execução do segundo for, é devolvido o valor da variável relativa ao valor máximo da variável num_fatias, o que será por consequência
o valor máximo para as fatias inseridas como input.*)
let rec lucro_max array_precos num_fatias =
	for i = 1 to num_fatias do 
		for j = 1 to i do
			valor_max := max (!valor_max) (array_precos.(j) + array_vm.(i-j));
		done; array_vm.(i) <- !valor_max;
	done; array_vm.(num_fatias)

(*Chamada da função e apresentação do respetivo Output*)
let () =
	Printf.printf "%d\n" (lucro_max array_precos num_fatias)

(*Supondo que é inserido como input os seguintes valores:
	6
	4
	1 2
	2 4
	4 5
	6 7
	
	Inicialmente o programa verificar os dados inseridos e vai criar a variável valor_max e ambos os arrays (array_precos e array_vm).
	De seguida insere os valores relativos aos preços no index correspondente ao tamanho da fatia:
	| 0 | 1 | 2 | 3 | 4 | 5 | 6 |
	| 0 | 2 | 4 | 0 | 5 | 0 | 7 |

	Em seguida percorre os ciclos for de modos a achar o valor máximo para cada tamanho de fatia.
	inserindo posteriormente esse valor no array_vm com o index esperado (numero de fatias):
		- valor_max = max 0 2+0 2 = 2 -> array_vm.(1) = 2 -> Valor máximo para 1 fatia;
	 	- valor_max = max 2 2+2 = 4 -> array_vm.(2) = 4 -> Soma de 2 fatias de tamanho 1;
	 	- valor_max = max 4 4+0 = 4 -> array_vm.(2) = 4 -> Valor máximo das fatias de tamanho 2 por definição;
		...
		- valor_max = max 10 2+10 = 12 -> array_vm.(6) = 12 -> Soma entre uma fatia de tamanho 1 e o valor máximo obtido a partir de fatias de tamanho 5;
		- valor_max = max 12 4+8 = 12 -> array_vm.(6) = 12 -> Soma entre uma fatia de tamanho 2 e o valor máximo obtido a partir de fatias de tamanho 4;
		- valor_max = max 12 0+6 = 12 -> array_vm.(6) = 12 -> Soma entre uma fatia de tamanho 3 e o valor máximo obtido a partir de fatias de tamanho 3;
		- valor_max = max 12 5+4 = 12 -> array_vm.(6) = 12 -> Soma entre uma fatia de tamanho 4 e o valor máximo obtido a partir de fatias de tamanho 2;
		- valor_max = max 12 0+2 = 12 -> array_vm.(6) = 12 -> Soma entre uma fatia de tamanho 5 e o valor máximo obtido a partir de fatias de tamanho 1;
		- valor_max = max 12 7+0 = 12 -> array_vm.(6) = 12 -> Valor máximo das fatias de tamanho 6 por definição;

	No fim de tudo o array_vm ficará da seguinte maneira (index/preços):
	| 0 | 1 | 2 | 3 | 4 | 5  | 6  |
	| 0 | 2 | 4 | 6 | 8 | 10 | 12 |

	Como o programa terá de devolver o preço máximo do numero de fatias inserido inicialmente, o mesmo será o ultimo valor do array.
	*)
