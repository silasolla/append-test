fun append_cps xs ys = foldl (fn (x, c) => fn zs => c (x :: zs)) (fn zs => zs) xs ys

fun append_bad [] ys = ys
  | append_bad (x :: xs) ys = x :: append_bad xs ys

fun append_cps' xs ys =
  let fun identity xs = xs
      fun append_cps_aux [] ys c = c ys 
        | append_cps_aux xs ys c = append_cps_aux (tl xs) ys (fn zs => c ((hd xs) :: zs))
  in append_cps_aux xs ys identity
  end

val xs = List.tabulate (100, fn _ => 1)
val ys = List.tabulate (100, fn _ => 2)

fun main () = case CommandLine.arguments () of
                i :: j :: [k] =>
                  let val (xs, ys) = (List.tabulate ((valOf o Int.fromString) i, fn _ => 1),
                                      List.tabulate ((valOf o Int.fromString) j, fn _ => 2))
                  in case k of "1" => (xs @ ys; print "xs @ ys\n")
                             | "2" => (append_cps xs ys; print "append_cps xs ys\n")
                             | "3" => (append_bad xs ys; print "append_bad xs ys\n")
                             | "4" => (append_cps' xs ys; print "append_cps' xs ys\n")
                             | _ => () end         
              | _ => ()

val _ = main () handle Option => ()
val _ = OS.Process.exit OS.Process.success
