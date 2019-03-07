	/////Exercise 1 

1. Step-by-step evaluation of product 3 : 

fun product n =
	if n = 1 then 
		1
	else
		n * product(n - 1);

> product 3;
> if 3 = 1 then 1 else 3 * product(3 - 1);
> if false then 1 else 3 * product(3 - 1);
> 3 * product(3 - 1);
> 3 * product(2);
> 3 * if 2 = 1 then 1 else 2 * product(2 - 1);
> 3 * if false then 1 else 2 * product(2 - 1);
> 3 * 2 * product(2 - 1);
> 3 * 2 * product(1);
> 3 * 2 * if 1 = 1 then 1 else 1 * product(1 - 1);
> 3 * 2 * if true then 1 else 1 * product(1 - 1);
> 3 * 2 * 1;
> 6;
 
2. The function computes ni.e the factorial of n!

3. Specification of the function "product"

   TYPE: int -> int
   PRE: n integer and n > 0
   POST: n!
   EXAMPLES: product 2 = 2
   			 product 5 = 120
   			 
4. Variant of the function : n
 	
	/////Exercise 2 : Currying

- Write the function declaration as a value declaration
*)	
	val plus = fn x => fn y => x + y;
(*	
- If "val foo = plus 4 5"  is entered, will get the result: " val foo = 9: int
- If "val bar = plus 4" is entered, will get the type of the function returned:"val bar = fn: int -> int"
- Step-by-step evaluation of "plus 3 4":
	> plus 3 4;
	> (fn x => fn y => x + y) 3 4;
	> (fn 3 => fn y => 3 + y) 4;
	> (fn y => 3 + y) 4;
	> fn 4 => 3 + 4;
	> 7;




	/////Exercise 3: Types
	
- Give functions with the following types:
*)
	fun fun1 x = x + 1;
	
	fun fun2 x = fn y => x + y + 3;
	
	fun fun3 x = (2, x + 1);
	
	fun fun4 (x, y) = x + y * 2;
	
	fun fun5 x = fn y => fn z => z ^ Int.toString( x + trunc y);
	
	fun fun6 (x, (a, b, c)) = (x+1, Int.toString c ^ a ^ b);
	
(*	 	  
	/////Exercise 4: Divisibility	
	


TYPE: int -> int
   PRE: n integer
   POST: least common multiple of all integers from 1 to n
   SIDE-EFFECTS: if n < 1, a "DOMAIN" exception is raised
*)
fun lcm n =
	let 
		fun gcd (x, y) = if x = y then x 
						else
							if x > y then gcd ( x - y, y)
							else gcd (x, y - x) 
								
		fun tmp(x, y) = (x * y) div (gcd (x, y))
		exception DOMAIN	
	in
		if n < 1 then  
			raise DOMAIN
		else 
			if n = 1 then 1
			else tmp(n, lcm(n-1))
	end; 
			
		
