(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Jay Van Alstyne
* jay.vanalstyne@montana.edu
*
***************************************************************)

(* Define your data type and functions here *)
fun f [] = []			(* a *)
	| f(x::xs) = (x+1) :: (f xs);	(* b *)

datatype 'element set = Empty
	| Set of 'element * 'element set;

fun isMember e Empty = false
	| isMember e (Set(setElement, nextSet)) =
		if e = setElement then true
		else isMember e nextSet;

fun list2Set [] = Empty		(* Still needs to check for duplicates before adding to set. *)
	| list2Set (head::tail) = Set (head, (list2Set tail));


(* Checks to see if the first member of set2 is in set1. If not, adds it to the beginning of a new set.
Regardless of the outcome, it will call itself again until set2 is Empty.
It then adds set1 in its entirety to the end and returns the set it built. *)
fun union set1 Empty = set1		(* This also works now! *)
	| union set1 (Set(setElement, nextSet)) =
		if isMember setElement set1 then union set1 nextSet
		else Set(setElement, (union set1 nextSet));

(* Checks to see if the first member of set2 is in set1. If so, adds it to the beginning of a new set.
Regardless of the outcome, it will call itself again until set2 is Empty.
It then adds Empty to the end and reurns the set it built. *)
fun intersect set1 Empty = Empty		(* This one works. *)
	| intersect set1 (Set(setElement, nextSet)) =
		if isMember setElement set1 then Set(setElement, (intersect set1 nextSet))
		else intersect set1 nextSet;

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);

(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
