(* demo.sml -- a deterministic walk through the pure sml-pkg core:
   parse a real-shaped manifest, resolve an in-memory dependency graph
   (a diamond), and print the lockfile and generated .mlb. No I/O beyond
   `print`, so the output is byte-identical under MLton and Poly/ML. *)

structure P = Pkg

val sj = "github.com/sjqtentacles/"

fun manifestOf (name, reqs) : P.manifest =
  { name = name,
    requires = List.map (fn (p, v) => { path = p, version = v }) reqs }

(* The example app and its in-memory registry: a diamond where both
   `sml-color` and `sml-image` depend on the shared `sml-codec`. *)
val app =
  manifestOf (sj ^ "sml-demo",
    [(sj ^ "sml-color", NONE), (sj ^ "sml-image", SOME "1")])

val registry =
  P.registryOf
    [ (sj ^ "sml-color",
       manifestOf (sj ^ "sml-color", [(sj ^ "sml-codec", NONE)]))
    , (sj ^ "sml-image",
       manifestOf (sj ^ "sml-image",
         [(sj ^ "sml-codec", NONE), (sj ^ "sml-inflate", NONE)]))
    , (sj ^ "sml-codec",   manifestOf (sj ^ "sml-codec",   []))
    , (sj ^ "sml-inflate", manifestOf (sj ^ "sml-inflate", []))
    ]

fun line () = print "----------------------------------------\n"

val () = print "== parse a manifest ==\n"
val () =
  case P.parse
         ("package github.com/sjqtentacles/sml-demo\n\n" ^
          "require {\n" ^
          "  github.com/sjqtentacles/sml-color\n" ^
          "  github.com/sjqtentacles/sml-image 1\n" ^
          "}\n") of
    P.Ok m =>
      ( print ("package: " ^ #name m ^ "\n")
      ; print ("requires: " ^ Int.toString (length (#requires m)) ^ "\n")
      ; List.app
          (fn (r : P.require) =>
             print ("  - " ^ #path r ^
                    (case #version r of NONE => "" | SOME v => " @ " ^ v)
                    ^ "\n"))
          (#requires m) )
  | P.Err e => print (P.parseErrorToString e ^ "\n")

val () = line ()
val () = print "== resolve the graph (diamond) ==\n"
val resolution =
  case P.resolve (app, registry) of
    P.Ok r => r
  | P.Err e => raise Fail (P.resolveErrorToString e)

val () = print "topological order (deps before dependents):\n"
val () =
  List.app
    (fn (d : P.resolved) =>
       print ("  " ^ #repo d ^
              (case #version d of NONE => "" | SOME v => " @ " ^ v) ^ "\n"))
    (#order resolution)

val () = line ()
val () = print "== sml.lock ==\n"
val () = print (P.lockfile resolution)

val () = line ()
val () = print "== generated .mlb ==\n"
val () = print (P.mlb P.defaultMlbConfig resolution)
