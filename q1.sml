datatype 'a DSeq = DNil | DCons of 'a * (unit -> 'a DSeq) * (unit -> 'a DSeq);
datatype 'a Seq = Nil | Cons of 'a * (unit -> 'a Seq);

fun coords (3, _) = DNil
  | coords (_, 3) = DNil
  | coords (x, y) = DCons((x, y), fn () => coords (x + 1, y), fn () => coords (x, y + 1));
  
val s = coords (0, 0);

fun next Nil = Nil
  | next (Cons(x, xf)) = xf ();
  
fun take s 0 = []
  | take Nil _ = []
  | take (Cons (x, xf)) n = x :: take (xf ()) (n - 1);

fun pcoords (3, _) = DNil
  | pcoords (_, 3) = DNil
  | pcoords (x, y) = (
    print ("exec: (" ^ Int.toString x ^ ", " ^  Int.toString y ^ ")\n"); 
    DCons((x, y), fn () => pcoords (x, y + 1), fn () => pcoords (x + 1,y))
  );
  
val p = pcoords (0, 0);

(* val toMatrix = fn : 'a DSeq -> int * int -> 'a list list *)
fun toMatrix DNil _ = []
  | toMatrix _ (0, _) = []
  | toMatrix (DCons(x, next_row, next_col)) (rows, cols) = 
    let
      fun takeRow _ 0 = []
        | takeRow DNil _ = []
        | takeRow (DCons(x, next_row, next_col)) n = x :: takeRow (next_col ()) (n - 1);

      val row = takeRow (DCons(x, next_row, next_col)) cols
    in
      row :: toMatrix (next_row ()) (rows - 1, cols)
    end;


(* val Q = fn: unit -> (int * int) DSeq; *)
fun Q () =
    let
        fun rationals (x, y) = DCons((x, y), fn () => rationals (x, y + 1), fn () => rationals (x + 1, y))
    in
        rationals (1, 1)
    end;



datatype NodeType = Left | Right


fun diags dseq =
  let
    fun getDiags ([], acc) = Nil
       | getDiags ((DNil, _)::rest, acc) =
          if null acc then Nil
          else
            let
              val (next, remainingAcc) = (hd acc, tl acc)
            in
              getDiags (rest @ [next ()], remainingAcc)
            end
      | getDiags ((DCons (x, nextRow, nextCol), nodeType)::rest, acc) =
          let
            val newAcc = case nodeType of
                           Left => acc @ [fn () => (nextCol (), Right), fn () => (nextRow (), Left)]
                         | Right => acc @ [fn () => (nextCol (), Right)]
            val (next, remainingAcc) = (hd newAcc, tl newAcc)
          in
            Cons (x, fn () => getDiags (rest @ [next ()], remainingAcc))
          end
  in
    getDiags ([(dseq, Left)], [])
  end;



(* 
[[(0,0),(1,0),(2,0)],
[(0,1),(1,1),(2,1)],
[(0,2),(1,2),(2,2)]]
  : (int * int) list list
 *)
