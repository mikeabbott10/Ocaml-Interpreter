(*open Interpreter*)

let env0 = emptyenv Unbound;;

(* TEST: scatenamento eccezione su costruttore Empty*)
eval (Empty(TBool)) env0;;

(* TEST: costruzione di un set di 7 elementi interi*)
let af = 
Insert(Eint 0,
Insert(Eint 1,
Insert(Eint 2,
Insert(Eint 3,
Insert(Eint 4, 
Insert(Eint 5, 
    Singleton(TInt, Eint 6)))))));;

(*TEST: isEmpty set(af) -> Bool(false) *)
eval (IsEmpty(af)) env0;;

(*TEST: Contains Int 6, set(af) -> Bool(true)*)
eval (Contains(Eint 6,af)) env0;;

(* TEST: Let myset = set(af) \ Int 6 *)
let ag =
Let("myset", af, Remove(Eint 6, Den "myset"));;
eval ag env0;;

(*TEST: Contains Int 6, set(ag) -> Bool(false)*)
eval (Contains(Eint 6,ag)) env0;;

(*TEST: 
    IsSubSet af, ag -> Bool(false)
    IsSubSet ag, af -> Bool(true)
*)
eval (IsSubSet(af, ag)) env0;;
eval (IsSubSet(ag, af)) env0;;

(*TEST: GetMax af -> Int 6
        GetMin af -> Int 0*)
eval (GetMax(af)) env0;;
eval (GetMin(af)) env0;;

(*TEST: Proprietà (x < 6) per ogni elemento di un insieme
ocaml-like code:
(let predicato =)
fun (x) -> 
    if(x<6) then true else false
*)
let predicato =
Fun("x", 
    Ifthenelse( LessThan(Den "x", Eint 6) , 
    (*then*) Ebool(true),
    (*else*) Ebool(false) 
    )
);;

(*TEST: for_all (n < 6) af -> Bool(false) 
        for_all (n < 6) ag -> Bool(true)*)
eval (For_all(predicato, af)) env0;;
eval (For_all(predicato, ag)) env0;;

(*TEST: Exists (n < 6) af -> Bool(true)) 
        Exists (n < 6) ag -> Bool(true)*)
eval (Exists(predicato, af)) env0;;
eval (Exists(predicato, ag)) env0;;

(*TEST: Filter (n < 6) af == ag -> Bool(true) *)
eval (Eq( Filter(predicato, af) , ag)) env0;;


(*TEST: FALSO predicato! per ogni elemento di un insieme
ocaml-like code:
(let predicato =)
fun (x) -> 
    if(x<6) then 10 else 3
*)
let falsopredicato =
Fun("x", 
    Ifthenelse( LessThan(Den "x", Eint 6) , 
    (*then*) Eint(10),
    (*else*) Eint(3) 
    )
);;

(*TEST: for_all (n < 6) af -> Eccezione: La funzione passata non è un predicato *)
eval (For_all(falsopredicato, af)) env0;;




(* TEST: applicazione di fibonacci su numero x -> fib x
ocaml-like code
(let ac =)
fun (x) ->
    let rec fib n = 
        if (n=0 || n=1) then
            1
        else
            if (n > 0) then
                fib (n-2) + fib (n-1)
            else
                failwith("Errore: valore negativo")
in fib x;;
*)
let ac =
Fun("x",
    Letrec(
        "fib", 
        Fun("n", 
            Ifthenelse( Or(IsZero(Den "n") , Eq(Den "n", Eint 1)) , 
            (*then *) Eint 1,
            (*else*) 
                Ifthenelse( Not(LessThan(Den "n", Eint 0)) , 
                (*then *) Sum(Apply(Den "fib", Diff(Den "n", Eint 2)), 
                              Apply(Den "fib", Diff(Den "n", Eint 1))),
                (*else*) Raise (OperationNotAllowed "L'argomento deve essere >= 0")
                )
            )
        ),
        Apply(Den "fib", (Den "x"))
    )
);;
(*trovo sesto numero di fibonacci -> Int 13*)
eval (Apply(ac, Eint(6))) env0;;

(* TEST: applicazione di fibonacci su numero negativo -> eccezione*)
eval (Apply(ac, Eint(-1))) env0;;


(*TEST: mappiamo i valori nel set(af) nei corrispondenti numeri di fibonacci 
    -> Set (TInt, [Int 1; Int 1; Int 2; Int 3; Int 5; Int 8; Int 13])*)
eval (Map(ac, af)) env0;;

(*TEST: mappiamo i valori del set(af) in valori di tipo non valido per un set
    -> Eccezione: Nuovo set non valido, il tipo deve essere TInt o TString*)
let funzioneerrata = 
Fun("x",
    Ebool(false)
);;
eval (Map(funzioneerrata, af)) env0;;