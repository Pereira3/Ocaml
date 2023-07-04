(*Ricardo Pereira nº 48554*)
open F_parser
(*Criação da variável referente à letra mais pequena*)
let letra = ref "Z"
(*Função que corre a fórmula de modo recursivo para determinar se a mesma apresenta uma letra, caso apresente
 a variável criada anteriormente assume esse valor, caso não apresente será usada a letra Z como default*)
let rec find_letra = function
| Var v -> if v < !letra then (letra := v) else ()
| Not v1 -> (match v1 with 
  | Or(v2, v3) -> find_letra v2; find_letra v3;
  | v4 -> find_letra v4;
)
| And (v1, v2) -> find_letra v1; find_letra v2;
| Or (v1, v2) -> find_letra v1; find_letra v2;
| Implies (v1, v2) -> find_letra v1; find_letra v2;
| Equiv (v1, v2) -> find_letra v1; find_letra v2;
| False -> ()
| True -> ()
(*Função recursiva responsável pela conversão da fórmula para apenas portas NOR*)
let rec conv = function
| Var v -> v
| Not v1 -> (match v1 with 
  | Or(v2, v3) -> (fun x y -> "("^x^" % "^y^")") (conv v2) (conv v3)
  | v4 -> (fun x -> "("^x^" % "^x^")") (conv v4)
)
| And (v1, v2) -> (fun x y -> "(("^x^" % "^x^") % ("^y^" % "^y^"))") (conv v1) (conv v2)
| Or (v1, v2) -> (fun x y -> "(("^x^" % "^y^") % ("^x^" % "^y^"))") (conv v1) (conv v2)
| Implies (v1, v2) -> conv ( Or(Not v1, v2) )
| Equiv (v1, v2) -> conv ( And( Implies (v1,v2), Implies(v2, v1) ) )
| False -> (fun x -> "("^x^" % ("^x^" % "^x^"))") (conv (Var !letra))
| True -> conv (Not (False))

let () =
 match parse "stdin" with
  | Some formula -> List.iter (fun f -> find_letra f; print_endline (conv f)) formula; 
  | None -> ();

(*
Caso o código seja executado e seja inserido como input a fórmula ((C | !(D)) -> E)
  - Inicialmente o F_Parser converte essa fórmula de modo a apresenta Implies ( Or (Var C) (Not (Var D)) ) (Var E)
  - A seguir apartir da função find_letra, como a fórmula apresenta 3 Variáveis (C, D e E), conforme a função percorre a fórmula
  e a variável letra assume o valor C.
  - Após isso percorre a função conv e converte a fórmula:
    = Or ( (Not (Or (Var C) (Not (Var D)))) (Var E) )
    = (  (Not (Or (Var C) (Not (Var D)))  %  (Var E)  )  %  (  (Not (Or (Var C) (Not (Var D)))  %  (Var E)  )
    = (  (Var C) % (Not (Var D))  %  (Var E)  )  %  (  (Var C) % (Not (Var D))  %  (Var E)  )
    = (  (Var C) % ( (Var D) % (Var D) )  %  (Var E)  )  %  (  (Var C) % ( (Var D) % (Var D) )  %  (Var E)  )
    = (((C % (D % D)) % E) % ((C % (D % D)) % E))

Caso o código seja executado e seja inserido como input a fórmula TRUE, como não é passado nenhuma variável o código assume Z como
letra default. 
  - Executa a função conv e converte diretamente para: ((Z % (Z % Z)) % (Z % (Z % Z)))
*)
