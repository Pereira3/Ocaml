(*Ler as variáveis, nfatias para efeito de calculos, ntlinhas para a quantidade de linhas da tabela*)
let nfatias = read_int()
let ntlinhas = read_int()
(*Verificar se os valores são válidos*)
let () = if nfatias < 0 || ntlinhas < 0 || nfatias >= 10000 || ntlinhas >= 10000 || nfatias <= ntlinhas then invalid_arg "Valor inserido inválido!"

(*Array de 2 dimensões, n linhas 2 colunas, inicializado com tudo a 0*)
let tabelaPQ = Array.make_matrix ntlinhas 2 0
(*Inserção dos valores na matrix apartir de tuplos*)
let () =
	for i = 0 to (ntlinhas-1) do
		let (v1, v2) = Scanf.scanf " %d %d" (fun a b -> (a, b)) in
    tabelaPQ.(i).(0) <- v1;
		tabelaPQ.(i).(1) <- v2;
  done;

