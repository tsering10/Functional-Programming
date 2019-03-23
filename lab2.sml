(* Tashi tsering *)

	|----------|+	ASSIGNEMENT 2   +|-----------|
			
1. Iota

   TYPE: int -> int list
   PRE: n >= 0
   POST: list of integers from 0 to (n - 1)
*)	fun iota n = 
		let
			fun Iota2 i = if i > n-1 then [] else i :: Iota2(i + 1)
			exception DOMAIN
		in
			if n < 0 
				then raise DOMAIN
			else
				Iota2 0
		end; 
 	
(* 
	
2. Intersection

  2.1 function inter
 
   	TYPE: ''a list * ''a list -> ''a list
	PRE: s1 and s2, where si is an int list
	POST: list containing the common elements of s1 and s2
	VARIANT: list of common elements to s1 and s2
*)	fun inter (s1:int list) (s2:int list) =
		let
			fun member x l =
				if l = [] 
					then false 
				else 
					if x = (hd l) 
						then true
					else 
						member x (tl l)
		in
			if s1 = [] 
				then [] 
			else 
				if (member (hd s1) s2 )
					then (hd s1):: inter (tl s1) s2
				else 
					inter(tl s1) s2
		end;
			
(*
  2.2 function inter'
	
   	TYPE: ''a list * ''a list -> ''a list
   	PRE: s1 and s2, where si is an ordered int list
   	POST: list containing the common elements of s1 and s2
	VARIANT: list of common elements to s1 and s2
*)	fun inter' ((x1::xs1):int list) [] = []
		| inter' [] ((x2::xs2):int list) = []
		| inter'((x1::xs1):int list) ((x2::xs2):int list) =
			if x1 < x2 
				then inter' (xs1) (x2::xs2)
			else 
				if x1 = x2
					then x1::inter' (xs1) (xs2) 
				else
					inter' (x1::xs1) (xs2)
		| inter' _ _ = [];
		
	
(*
  2.3 Comparing speed between inter and inter'
	
	RESULTS : inter : 12.559 second
			  inter': 0.003 second
3. Fruit
	
  3.1 Datatype fruit	
*)
	datatype fruit =  Apple of real| Banana of real| Lemon of int ;
(*

  3.2 Function sumPrice

	TYPE: fruit list -> real -> real -> real -> real 
	PRE: list of fruit with their weight/numbers, with the price of each fruit
	POST: total price of the fruits
	VARIANT: total price of the fruits	

*)	fun sumPrice (fruitList:fruit list) pA pB pL  = 
			let 
				fun distingu (Apple wA) = wA * pA
				|	distingu (Banana wB) = wB * pB
				|	distingu (Lemon uts) = pL * real uts 
			in
				if null fruitList 
					then 0.0
				else
					(distingu (hd fruitList)) + sumPrice (tl fruitList) pA pB pL 
			end;	 			 
				 		 
(*
4. Trees

  4.1 Definition of datatype Itree
*)			
	datatype ''a Itree = Node of ''a * ''a Itree list;
(*
  4.2 
  		a) Function which compute number of nodes of a tree

			TYPE: ''a Itree -> int
			PRE: ''a Itree representing the tree
			POST: number of node of the input ''a Itree
			VARIANT: Number of nodes
			 		 	
*)			fun count (Node (_, branches)) = 1 + numb branches
				and numb [] = 0
				  | numb t = (count (hd t)) + (numb (tl t));
						
(*		b) Function which compute the list of all node labels in a tree			
					
			TYPE: ''a Itree ->  ''a list
			PRE: tree node 
			POST: list containing all nodes'labels	
				
*)			fun labels (Node (lbl, branches))	=	[lbl] @ sublabels branches
				and sublabels [] = []
				 |	sublabels t =  labels (hd t) @ sublabels (tl t); 

(*		c) Function which check presence of a given value

			TYPE: ''a Itree  -> ''a -> bool
			PRE: tree and a value to look for
			POST: tell if the value is present in the tree
			
*)			fun is_present (Node (t, branches)) x =  (t = x) orelse check_in_branch branches x
				and	check_in_branch [] x = false
				|	check_in_branch t  x = (is_present (hd t) x) orelse (check_in_branch (tl t) x);			
					
(*		d) Function which compute the height of a tree

			TYPE: ''a Itree -> int
			PRE: tree node
			POST: the height of the input tree							

*)			fun height (Node (t, branches)) = 1 + digg branches
				and digg [] = 0
				|   digg t  = max (height (hd t)) (digg (tl t)) 
				and max a b = if a > b then a else if a = b then a else b
