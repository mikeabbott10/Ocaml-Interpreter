type ide = string;;

(** Un tipo contenente una "firma" dei tipi esprimibili che un valore può assumere *)
type typeInfo =
    | TSet
    | TBool
    | TInt
    | TString
    | TFun
    | TFunRec;;

(*Un tipo contente tutte le espressioni del linguaggio (Albero di sintassi astratta) *)
type exp = 
    | Eint of int 
    | Ebool of bool 
    | Estring of string
    (*costruttori insiemi*)
    | Empty of typeInfo
    | Singleton of typeInfo * exp
    (* funzioni su insiemi *)
    | Insert of exp * exp
    | Remove of exp * exp
    | IsEmpty of exp
    | Contains of exp * exp
    | IsSubSet of exp * exp
    | GetMax of exp
    | GetMin of exp
    (*operatori su insiemi*)
    | For_all of exp * exp
    | Exists of exp * exp
    | Filter of exp * exp
    | Map of exp * exp
    (*espressioni di base*)
    | Den of ide 
    | Prod of exp * exp 
    | Sum of exp * exp 
    | Diff of exp * exp 
    | Eq of exp * exp 
    | Minus of exp 
    | IsZero of exp 
    | LessThan of exp * exp
    | Or of exp * exp 
    | And of exp * exp 
    | Not of exp 
    | Ifthenelse of exp * exp * exp 
    | Raise of exn (*il programmatore può scatenare eccezioni*)
    | Let of ide * exp * exp (*Dichiarazione di ide: modifica ambiente*)
    | Fun of ide * exp (*Astrazione di funzione *)
    | Apply of exp * exp (*Applicazione di funzione*)
    | Letrec of ide * exp * exp (*ide = nome funzione, ide parametro, exp corpo*)
    ;;

(*ambiente polimorfo*)
type 't env = ide -> 't;;
let emptyenv (v : 't) = function x -> v;;
let applyenv (r : 't env) (i : ide) = r i;;
let bind (r : 't env) (i : ide) (v : 't) = 
    function x -> if x = i then v else applyenv r x;;

(*tipi esprimibili*)
type evT = 
    | Int of int 
    | Bool of bool 
    | String of string
    | Set of typeInfo * evT list
    | Unbound
    | FunVal of ide * exp * evT env 
    | RecFunVal of ide * ide * exp * evT env;;

(*Eccezioni*)
exception SetIsEmpty of string;;
exception OperationNotAllowed of string;;

(*Type checking dinamico
    @param s: tipo
    @param v: tipo valutato
    @return: true se il tipo di v è s, false altrimenti*)
let typecheck (s : typeInfo) (v : evT) : bool = 
    match s with
	| TInt -> 
        begin 
            match v with
		    | Int(_) -> true
		    | _ -> false
        end
	| TBool -> 
        begin
            match v with
		    | Bool(_) -> true
		    | _ -> false
        end
    | TString ->
        begin
            match v with
            | String(_) -> true
            | _ -> false
        end
    | TSet ->
        begin
            match v with
            | Set(_,_) -> true
            | _ -> false
        end
    | _ -> failwith("not a valid type");;

(*Funzione usata per ottenere il tipo di un elemento valutato
  (Nota: con typeof così definita, potremmo inferire il tipo degli elementi di un insieme direttamente
  usando il costruttore singleton, senza dover per forza ricevere il tipo come parametro. 
  Non lo si fa perchè la consegna esplicita il passaggio a singleton del tipo da parametro)
    @param e: l'elemento valutato da inferire
    @return: il tipo dell'elemento (typeInfo) *)
let typeof e = match e with
    | Set (t, a) -> TSet
    | Bool _ -> TBool
    | Int _ -> TInt
    | String _ -> TString
    | FunVal (_, _, _) -> TFun
    | RecFunVal (_, _, _, _) -> TFunRec
    | Unbound -> failwith("No element, no type");;

(*Costruttore di insieme vuoto
    @param t: tipo dei valori dell'insieme
    @return: set vuoto valutato *)
let empty (t:typeInfo) : evT =
    match t with
    | TInt -> Set(TInt, [])
    | TString -> Set(TString, [])
    | _ -> failwith("wrong set type");;

(*Costruttore di insieme contenente un solo elemento
    @param t: tipo dei valori dell'insieme
    @param x: elemento di tipo t
    @return: set valutato contenente x *)
let singleton (t:typeInfo) (x:evT) : evT =
    match (typecheck TInt x || typecheck TString x , t, x) with
    | (true, TInt, Int(n)) -> Set(TInt, [Int(n)])
    | (true, TString, String(s)) -> Set(TString, [String(s)])
    | (_,_,_) -> failwith("wrong type");;

(*Cerca x in lst
    @param lst: lista in cui cercare
    @param x elemento da cercare 
    @return: true se l'elemento appartiene a lst, false altrimenti *)
let rec lookup lst x =
    match lst with
    | [] -> false
    | h::t -> 
        if x=h then true else lookup t x;;

(*Ottiene il massimo elemento in lista
    @param ls: lista in cui cercare
    @throws: SetIsEmpty se la lista ls è vuota
    @return: il valore massimo nella lista ls *)
let getMaxInList ls =
    let max =
        match ls with
        | [] -> raise (SetIsEmpty "L'insieme è vuoto")
        | h::_ -> h
    in 
        let rec getmax lst m =
            match lst with
            | [] -> m
            | h::t -> 
                if h>m then getmax t h else getmax t m
        in getmax ls max;;

(*Ottiene il minimo elemento in lista
    @param ls: lista in cui cercare
    @throws: SetIsEmpty se la lista ls è vuota
    @return: il valore minimo nella lista ls *)
let getMinInList ls =
    let min =
        match ls with
        | [] -> raise (SetIsEmpty "L'insieme è vuoto")
        | h::_ -> h
    in 
        let rec getmin lst m =
            match lst with
            | [] -> m
            | h::t -> 
                if h<m then getmin t h else getmin t m
        in getmin ls min;;

(*Controlla se tutti gli elementi di lst1 appartengono anche a lst2
    @param lst1: lista 1
    @param lst2: lista 2
    @return: true se tutti gli elementi di lst1 appartengono anche a lst2, false altrimenti *)
let rec isListInList lst1 lst2 =
    match lst1 with
    | [] -> true
    | h::t -> 
        if (lookup lst2 h) then isListInList t lst2 else false;;

(*Restituisce lst senza l'elemento x
    @param lst: lista da cui rimuovere x
    @param x: elemento da rimuovere
    @param acc: accumulatore
    @return: la lista lst sprovvista di x *)
let rec removeElFromList lst x (acc:evT list) =
    match lst with
    | [] -> acc
    | h::t -> 
        if x=h then (removeElFromList t x acc) else (removeElFromList t x (acc @ [h]));;

(*funzioni primitive ============================================================== *)
(*Fa il prodotto tra x e y, interi
    @param x, y: interi (di tipo esprimibile Int)
    @throws: run-time error se x e y non sono interi
    @return: il prodotto tra x e y come tipo esprimibile Int *)
let prod x y = 
    match (typecheck TInt x , typecheck TInt y , x, y) with
    | (true, true, Int(n), Int(u)) -> Int(n*u)
    | (_,_,_,_) -> failwith("run-time error ");;

(*Fa la somma tra x e y, interi
    @param x, y: interi (di tipo esprimibile Int)
    @throws: run-time error se x e y non sono interi
    @return: la somma tra x e y come tipo esprimibile Int *)
let sum x y = 
    match (typecheck TInt x , typecheck TInt y , x, y) with
    | (true, true, Int(n), Int(u)) -> Int(n+u)
    | (_,_,_,_) -> failwith("run-time error ");;

(*Fa la differenza tra x e y, interi
    @param x, y: interi (di tipo esprimibile Int)
    @throws: run-time error se x e y non sono interi
    @return: la differenza tra x e y come tipo esprimibile Int *)
let diff x y = 
    match (typecheck TInt x , typecheck TInt y , x, y) with
    | (true, true, Int(n), Int(u)) -> Int(n-u)
    | (_,_,_,_) -> failwith("run-time error ");;

(*Controlla l'uguaglianza tra x e y
    @param x, y: interi, booleani, stringhe o set (tipi esprimibili)
    @throws: run-time error se x e y non sono dello stesso tipo o se non sono di un tipo accettato
    @return: Bool(true) se x = y, altrimenti Bool(false) *)
let eq x y = 
    match (typecheck TInt x || typecheck TBool x || typecheck TString x || typecheck TSet x, 
           typecheck TInt y || typecheck TBool y || typecheck TString x || typecheck TSet y , x, y) with
    | (true, true, Int(a), Int(b)) -> Bool(a = b)
    | (true, true, Bool(a), Bool(b)) -> Bool(a = b)
    | (true, true, String(a), String(b)) -> Bool(a = b)
    | (true, true, Set(t1,l1), Set(t2,l2)) -> 
        if t1 = t2
        then 
            if ((isListInList l1 l2) && (isListInList l2 l1))
            then Bool(true)
            else Bool(false)
        else Bool(false)
    | (_,_,_,_) -> failwith("run-time error ");;

(*Restituisce x intero negato
    @param x: intero (di tipo esprimibile Int)
    @throws: run-time error se x non è intero
    @return: Int(-n) *)
let minus x = 
    match (typecheck TInt x, x) with
    | (true, Int(n)) -> Int(-n)
    | (_,_) -> failwith("run-time error ");;

(*Controlla l'uguaglianza tra x e 0
    @param x: intero (di tipo esprimibile Int)
    @throws: run-time error se x non è intero
    @return: Bool(true) se x = 0, altrimenti Bool(false) *)
let iszero x = 
    match (typecheck TInt x , x) with
    | (true, Int(n)) -> Bool(n=0)
    | (_,_) -> failwith("run-time error ");;

(*Controlla se x è minore o uguale a y
    @param x, y: interi (di tipo esprimibile Int)
    @throws: run-time error se x e y non sono interi
    @return: Bool(true) se x < y, altrimenti Bool(false) *)
let lessthan x y =
    match (typecheck TInt x , typecheck TInt y , x, y) with
    | (true, true, Int(n), Int(u)) -> Bool(x<y)
    | (_,_,_,_) -> failwith("run-time error ");;

(*Effettua or logico tra x e y
    @param x, y: booleani (di tipo esprimibile Bool)
    @throws: run-time error se x e y non sono booleani
    @return: Bool(x or y)*)
let vel x y = 
    match (typecheck TBool x , typecheck TBool y, x, y) with
    | (true, true, Bool(b), Bool(e)) -> Bool(b || e)
    | (_,_,_,_) -> failwith("run-time error ");;

(*Effettua and logico tra x e y
    @param x, y: booleani (di tipo esprimibile Bool)
    @throws: run-time error se x e y non sono booleani
    @return: Bool(x and y)*)
let et x y = 
    match (typecheck TBool x, typecheck TBool y, x, y) with
    | (true, true, Bool(b), Bool(e)) -> Bool(b && e)
    | (_,_,_,_) -> failwith("run-time error ");;

(*Effettua not logico di x
    @param x: booleano (di tipo esprimibile Bool)
    @throws: run-time error se x non è booleani
    @return: Bool(not(x))*)
let non x = 
    match (typecheck TBool x, x) with
    | (true, Bool(true)) -> Bool(false)
    | (true, Bool(false)) -> Bool(true)
    | (_,_) -> failwith("run-time error ");;
(*end funzioni primitive =========================================================== *)

(*Funzione usata per chiamare una funzione su un argomento già valutato 
  (è necessaria in quanto i Set sono collezioni di elementi di tipo evT 
  e non vanno valutati ulteriormente, cosa che eval farebbe!)
    @param f: espressione funzionale
    @param aVal: argomento passato alla funzione f, già valutata
    @param r: ambiente polimorfo
    @return: valutazione della funzione f applicata ad aVal
*)
let rec funCaller (f:exp) (aVal:evT) (r:evT env) = 
    let fClosure = (eval f r) in
        match fClosure with
        | FunVal(arg, fBody, fDecEnv) -> 
            let aEnv = (bind fDecEnv arg aVal) in
                eval fBody aEnv
        | RecFunVal(g, arg, fBody, fDecEnv) -> 
            let rEnv = (bind fDecEnv g fClosure) in
                let aEnv = (bind rEnv arg aVal) in
                    eval fBody aEnv
        | _ -> failwith("non functional value")

(*Controlla se tutti gli elementi della lista lst
  soddisfano la proprietà definita dal parametro pr.
    @param pr: espressione predicato, la proprietà da controllare
    @param lst: lista di elementi, di tipo valutato, dei quali controllare la proprietà pr
    @return: Bool(true) se pr è soddisfatto da tutti gli elementi della lista lst *)
and forall (pr:exp) (lst:evT list) (r:evT env) :evT =
    match lst with
    | [] -> Bool(true)
    | h::tail ->
        let funapplied = funCaller pr h r in
        begin
            match typeof(funapplied) with
            | TBool -> 
                if funapplied = Bool(true)
                    then forall pr tail r
                    else Bool(false)
            | _ -> failwith("La funzione passata non è un predicato")
        end

(*Controlla se esiste almeno un elemento della lista lst
  che soddisfa la proprietà definita dal parametro pr.
    @param pr: espressione predicato, la proprietà da controllare
    @param lst: lista di elementi, di tipo valutato, dei quali controllare la proprietà pr
    @return: Bool(true) se pr è soddisfatto da almeno un elemento della lista lst *)
and exists (pr:exp) (lst:evT list) (r:evT env) :evT =
    match lst with
    | [] -> Bool(false)
    | h::tail ->
        let funapplied = funCaller pr h r in
            begin
                match typeof(funapplied) with
                | TBool -> 
                    if funapplied = Bool(false)
                        then exists pr tail r
                        else Bool(true)
                | _ -> failwith("La funzione passata non è un predicato")
            end

(*Restituisce gli elementi della lista lst che soddisfano pr.
    @param pr: espressione predicato, la proprietà da controllare
    @param lst: lista di elementi, di tipo valutato, dei quali controllare la proprietà pr
    @return: lista degli elementi della lista lst che soddisfano pr *)
and filter (pr:exp) (lst:evT list) (r:evT env) =
    match lst with
    | [] -> []
    | h::tail ->
        let funapplied = funCaller pr h r in
            begin
                match typeof(funapplied) with
                | TBool -> 
                    if funapplied = Bool(true)
                        then h::(filter pr tail r)
                        else filter pr tail r
                | _ -> failwith("La funzione passata non è un predicato")
            end

(*Restituisce la lista dei valori v tali che v=f(x) con x elementi di lst.
    @param f: espressione funzionale, la funzione da applicare agli elementi di lst
    @param lst: lista di elementi, di tipo valutato, su cui applicare f
    @return: lista dei valori v tali che v=f(x) con x elementi di lst *)
and map (f:exp) (lst:evT list) (r:evT env) =
    match lst with
    | [] -> []
    | h::tail ->
        let v = funCaller f h r in 
            v::(map f tail r)

(*Interprete: Valuta una espressione in un ambiente
    @param e: espressione da valutare
    @param r: l'attuale ambiente di valutazione (immutable) modificato, eventualmente, solo nelle chiamate ricorsive
    @return: la valutazione dell'espressione passata nell'ambiente polimorfo r *)
and eval (e : exp) (r : evT env) : evT = 
    match e with
	| Eint n -> Int n
	| Ebool b -> Bool b
    | Estring s -> String s
    (*| Eset (t, vl) -> Set(t, evalList vl t r)*)
    | Den i -> applyenv r i
    (*costruttori insiemi*)
    | Empty (t:typeInfo) -> empty t
    | Singleton ((t:typeInfo), (x:exp)) -> singleton t (eval x r)
    (* funzioni su insiemi *)
    | Insert((el:exp), (aSet:exp)) -> 
        begin
            match eval aSet r with
            | Set(t, evaluatedLs) ->
                let evaluatedEl = eval el r in 
                    if(typeof evaluatedEl = t && not(lookup evaluatedLs evaluatedEl)) then (*se il tipo dell'elemento combacia col tipo degli elementi nell'insieme e se non esiste già al suo interno*)
                        Set(t, evaluatedEl::evaluatedLs)
                    else
                        failwith("Errore: impossibile inserire un elemento di tipo 'a in un insieme di tipo 'b con 'b<>'a .")
            | _ -> failwith ("Errore: Set non valido")
        end
    | Remove((el:exp), (aSet:exp)) -> 
        begin
            match eval aSet r with
            | Set(t, evaluatedLs) ->
                let evaluatedEl = eval el r in 
                    if(typeof evaluatedEl = t) then (*se il tipo dell'elemento combacia col tipo degli elementi nell'insieme*)
                        Set(t, (removeElFromList evaluatedLs evaluatedEl []))
                    else
                        failwith("Errore: impossibile inserire un elemento di tipo 'a in un insieme di tipo 'b con 'b<>'a .")
            | _ -> failwith ("Errore: Set non valido")
        end
    | IsEmpty (aSet:exp) -> 
        begin
            match eval aSet r with
            | Set(t, evaluatedLs) ->
                begin
                    match evaluatedLs with
                    | [] -> Bool(true)
                    | _ -> Bool(false)
                end
            | _ -> failwith ("Errore: Set non valido")
        end
    | Contains((el:exp), (aSet:exp)) -> 
        begin
            match eval aSet r with
            | Set(t, evaluatedLs) ->
                let evaluatedEl = eval el r in 
                    if(typeof evaluatedEl = t) then (*se il tipo dell'elemento combacia col tipo degli elementi nell'insieme e se non esiste già al suo interno*)
                        Bool(lookup evaluatedLs evaluatedEl)
                    else
                        Bool(false)
            | _ -> failwith ("Errore: Set non valido")
        end
    | IsSubSet((cSet:exp), (aSet:exp)) -> 
        begin
            match (eval cSet r, eval aSet r) with
            | (Set(t1, evaluatedLs1), Set(t2, evaluatedLs2)) -> Bool(isListInList evaluatedLs1 evaluatedLs2)
            | (_,_) -> failwith ("Errore: Set non valido")
        end
    | GetMax (aSet:exp) -> 
        begin
            match eval aSet r with
            | Set(t, evaluatedLs) -> getMaxInList evaluatedLs
            | _ -> failwith ("Errore: Set non valido")
        end
    | GetMin (aSet:exp) -> 
        begin
            match eval aSet r with
            | Set(t, evaluatedLs) -> getMinInList evaluatedLs
            | _ -> failwith ("Errore: Set non valido")
        end
    (*operatori su insiemi*)
    | For_all((predicate:exp), (aSet:exp)) -> 
        begin
            match eval aSet r with
            | Set(t, elementi) -> forall predicate elementi r
            | _ -> failwith ("Errore: Set non valido")
        end
    | Exists((predicate:exp), (aSet:exp)) -> 
        begin
            match eval aSet r with
            | Set(t, elementi) -> exists predicate elementi r
            | _ -> failwith ("Errore: Set non valido")
        end
    | Filter((predicate:exp), (aSet:exp)) -> 
        begin
            match eval aSet r with
            | Set(t, elementi) -> Set(t, (filter predicate elementi r))
            | _ -> failwith ("Errore: Set non valido")
        end
    | Map((f:exp), (aSet:exp)) ->
        begin
            match eval aSet r with
            | Set(t, elementi) -> 
                let nuovaLista = (map f elementi r) in
                    let nuovoTipo = typeof(List.hd(nuovaLista)) in
                        if(nuovoTipo = TInt || nuovoTipo = TString)
                        then Set(nuovoTipo, nuovaLista)
                        else failwith ("Errore: Nuovo set non valido, il tipo deve essere TInt o TString") 
            | _ -> failwith ("Errore: Set non valido")
        end
    (*operatori su interi*)
    | Prod(a, b) -> prod (eval a r) (eval b r)
    | Sum(a, b) -> sum (eval a r) (eval b r)
    | Diff(a, b) -> diff (eval a r) (eval b r)
    | Minus a -> minus (eval a r)
	| IsZero a -> iszero (eval a r)
    | LessThan(a, b) -> lessthan (eval a r) (eval b r)
    (*operatori su booleani*)
	| And(a, b) -> et (eval a r) (eval b r)
	| Or(a, b) -> vel (eval a r) (eval b r)
	| Not a -> non (eval a r)
    (*operatori validi per interi, booleani e stringhe*)
    | Eq(a, b) -> eq (eval a r) (eval b r)
	| Ifthenelse(a, b, c) -> 
		let g = (eval a r) in
			if (typecheck TBool g) then 
                (if g = Bool(true) then 
                    (eval b r)
                else 
                    (eval c r))
			else 
                failwith ("nonboolean guard")
    | Raise ex -> raise ex
	| Let(i, e1, e2) -> eval e2 (bind r i (eval e1 r)) (*valuta e2 nell'ambiente r con aggiunta l'associazione i <-> valutazione di e1*)
	| Fun(i, a) -> FunVal(i, a, r) (*Si nota il legame tra la funzione e l'ambiente r in cui è stata definita, 
                                        sotto detto fDecEnv: function declaration environment.
                                        Si nota dunque che si ha scoping statico*)
	| Apply(f, eArg) -> 
		let fClosure = (eval f r) in
			begin
                match fClosure with
				| FunVal(arg, fBody, fDecEnv) -> 
					let aVal = (eval eArg r) in
                        let aEnv = (bind fDecEnv arg aVal) in
                            eval fBody aEnv
				| RecFunVal(g, arg, fBody, fDecEnv) -> 
					let aVal = (eval eArg r) in
						let rEnv = (bind fDecEnv g fClosure) in
							let aEnv = (bind rEnv arg aVal) in
								eval fBody aEnv
				| _ -> failwith("non functional value")
            end
    | Letrec(f, funDef, letBody) ->
		begin
            match funDef with
    		    | Fun(i, fBody) -> 
                    let r1 = (bind r f (RecFunVal(f, i, fBody, r))) in (*Viene associato al nome f della funzione*)
			                eval letBody r1                              (*ricorsiva una chiusura ricorsiva che contiene il nome della funzione stessa*) 
                | _ -> failwith("non functional def")
        end;;