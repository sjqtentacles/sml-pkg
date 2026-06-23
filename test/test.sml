(* test.sml -- golden vectors for the pure sml-pkg core.

   Exercises the four pure transforms:
     parse     : string -> manifest
     resolve   : manifest * registry -> resolution
     lockfile  : resolution -> string  (sml.lock)
     mlb       : mlbConfig -> resolution -> string

   Every expected string is spelled out verbatim so the suite is a golden
   test: it asserts byte-exact output, which is what guarantees the
   dual-compiler byte-identical property for the pure core. *)

structure Tests =
struct
  open Harness

  structure P = Pkg

  (* ---- helpers ------------------------------------------------------- *)

  fun okManifest src =
    case P.parse src of
      P.Ok m => m
    | P.Err e => raise Fail ("parse failed: " ^ P.parseErrorToString e)

  fun okResolution (m, reg) =
    case P.resolve (m, reg) of
      P.Ok r => r
    | P.Err e => raise Fail ("resolve failed: " ^ P.resolveErrorToString e)

  fun manifestOf (name, reqs) : P.manifest =
    { name = name,
      requires = List.map (fn (p, v) => { path = p, version = v }) reqs }

  fun orderPaths (r : P.resolution) =
    List.map (fn (d : P.resolved) => #path d) (#order r)

  val sj = "github.com/sjqtentacles/"

  (* ---- parsing ------------------------------------------------------- *)

  fun parseTests () =
    let
      val () = section "parse: basic forms"

      val m0 = okManifest "package github.com/sjqtentacles/sml-cli\n\nrequire {\n}\n"
      val () = checkString "leaf name"
                 ("github.com/sjqtentacles/sml-cli", #name m0)
      val () = checkInt "leaf has no requires" (0, length (#requires m0))

      val m1 = okManifest
        ("package github.com/sjqtentacles/sml-lsp\n\n" ^
         "require {\n" ^
         "  github.com/sjqtentacles/sml-mlast\n" ^
         "  github.com/sjqtentacles/sml-fmt\n" ^
         "  github.com/sjqtentacles/sml-json\n" ^
         "}\n")
      val () = checkInt "lsp has 3 requires" (3, length (#requires m1))
      val () = checkStringList "lsp require paths in order"
                 ([sj ^ "sml-mlast", sj ^ "sml-fmt", sj ^ "sml-json"],
                  List.map #path (#requires m1))

      (* versioned dependency, smlpkg-style *)
      val m2 = okManifest
        ("package github.com/sjqtentacles/sml-astar\n\n" ^
         "require {\n" ^
         "  github.com/sjqtentacles/sml-pqueue 1\n" ^
         "}\n")
      val () = checkStringList "astar dep path"
                 ([sj ^ "sml-pqueue"], List.map #path (#requires m2))
      val () = check "astar dep version = 1"
                 (List.map #version (#requires m2) = [SOME "1"])

      (* pseudo-version string *)
      val m3 = okManifest
        ("package github.com/sjqtentacles/sml-skiplist\n\n" ^
         "require {\n" ^
         "  github.com/sjqtentacles/sml-prng 0.0.0-20260621120446+2466b0395\n" ^
         "}\n")
      val () = check "skiplist pseudo-version preserved"
                 (List.map #version (#requires m3)
                  = [SOME "0.0.0-20260621120446+2466b0395"])

      (* comments + blank lines + tight `require{` and no-blank package *)
      val m4 = okManifest
        ("(* leading comment *)\n" ^
         "package github.com/sjqtentacles/sml-bech32\n" ^
         "require {\n" ^
         "  github.com/sjqtentacles/sml-codec  (* inline comment *)\n" ^
         "\n" ^
         "}\n")
      val () = checkString "comment/blank-tolerant name"
                 (sj ^ "sml-bech32", #name m4)
      val () = checkStringList "inline-comment dep"
                 ([sj ^ "sml-codec"], List.map #path (#requires m4))

      val () = section "parse: round-trip via render"
      val rt = P.render m1
      val () = checkString "render canonical form"
        ("package github.com/sjqtentacles/sml-lsp\n\n" ^
         "require {\n" ^
         "  github.com/sjqtentacles/sml-mlast\n" ^
         "  github.com/sjqtentacles/sml-fmt\n" ^
         "  github.com/sjqtentacles/sml-json\n" ^
         "}\n", rt)
      val () = check "parse o render = id (names)"
                 (#name (okManifest rt) = #name m1)

      val () = section "parse: errors as values"
      val () = check "missing package"
        (case P.parse "require {\n}\n" of P.Err _ => true | _ => false)
      val () = check "unterminated require"
        (case P.parse "package a\nrequire {\n  b\n" of
           P.Err _ => true | _ => false)
      val () = check "duplicate dependency rejected"
        (case P.parse
           ("package a\nrequire {\n  b\n  b\n}\n") of
           P.Err _ => true | _ => false)
      val () = check "malformed require line (3 tokens)"
        (case P.parse "package a\nrequire {\n  b c d\n}\n" of
           P.Err _ => true | _ => false)
      val () = check "content after block rejected"
        (case P.parse "package a\nrequire {\n}\njunk\n" of
           P.Err _ => true | _ => false)

      val () = section "parse: repoName"
      val () = checkString "repoName extracts last segment"
                 ("sml-cli", P.repoName (sj ^ "sml-cli"))
    in () end

  (* ---- registry fixtures --------------------------------------------- *)

  (* A diamond:  app -> {alpha, beta};  alpha -> {delta};  beta -> {delta};
                 delta -> {}.  delta is shared (the diamond join). *)
  val regDiamond =
    P.registryOf
      [ (sj ^ "alpha", manifestOf (sj ^ "alpha", [(sj ^ "delta", NONE)]))
      , (sj ^ "beta",  manifestOf (sj ^ "beta",  [(sj ^ "delta", NONE)]))
      , (sj ^ "delta", manifestOf (sj ^ "delta", []))
      ]

  val appDiamond =
    manifestOf (sj ^ "app", [(sj ^ "alpha", NONE), (sj ^ "beta", NONE)])

  (* A cycle:  x -> y -> z -> x. *)
  val regCycle =
    P.registryOf
      [ (sj ^ "x", manifestOf (sj ^ "x", [(sj ^ "y", NONE)]))
      , (sj ^ "y", manifestOf (sj ^ "y", [(sj ^ "z", NONE)]))
      , (sj ^ "z", manifestOf (sj ^ "z", [(sj ^ "x", NONE)]))
      ]
  val appCycle = manifestOf (sj ^ "x", [(sj ^ "y", NONE)])

  (* version pinning + conflict fixtures *)
  val regVersioned =
    P.registryOf
      [ (sj ^ "alpha", manifestOf (sj ^ "alpha", [(sj ^ "delta", SOME "2")]))
      , (sj ^ "beta",  manifestOf (sj ^ "beta",  [(sj ^ "delta", SOME "2")]))
      , (sj ^ "delta", manifestOf (sj ^ "delta", []))
      ]
  val regConflict =
    P.registryOf
      [ (sj ^ "alpha", manifestOf (sj ^ "alpha", [(sj ^ "delta", SOME "1")]))
      , (sj ^ "beta",  manifestOf (sj ^ "beta",  [(sj ^ "delta", SOME "2")]))
      , (sj ^ "delta", manifestOf (sj ^ "delta", []))
      ]

  (* ---- resolution ---------------------------------------------------- *)

  fun resolveTests () =
    let
      val () = section "resolve: diamond"
      val r = okResolution (appDiamond, regDiamond)
      val () = checkString "root recorded" (sj ^ "app", #root r)
      (* topological, lexicographic tie-break: delta before alpha & beta;
         alpha before beta. delta has no deps so comes first. *)
      val () = checkStringList "diamond topo order"
                 ([sj ^ "delta", sj ^ "alpha", sj ^ "beta"], orderPaths r)
      val () = checkInt "diamond has 3 nodes (delta shared once)"
                 (3, length (#order r))

      val () = section "resolve: missing dependency"
      val () = check "missing reported"
        (case P.resolve
               (manifestOf (sj ^ "app", [(sj ^ "ghost", NONE)]), regDiamond) of
           P.Err (P.Missing _) => true | _ => false)

      val () = section "resolve: cycle detection"
      val () = check "cycle reported"
        (case P.resolve (appCycle, regCycle) of
           P.Err (P.Cycle _) => true | _ => false)
      val () = checkStringList "cycle canonical rotation"
        ([sj ^ "x", sj ^ "y", sj ^ "z", sj ^ "x"],
         (case P.resolve (appCycle, regCycle) of
            P.Err (P.Cycle c) => c | _ => ["?"]))

      val () = section "resolve: versions"
      val rv = okResolution (appDiamond, regVersioned)
      val () = check "shared pinned version selected"
        (List.exists
           (fn (d : P.resolved) =>
              #path d = sj ^ "delta" andalso #version d = SOME "2")
           (#order rv))

      val () = section "resolve: conflict"
      val () = check "version conflict reported"
        (case P.resolve (appDiamond, regConflict) of
           P.Err (P.Conflict _) => true | _ => false)
    in () end

  (* ---- lockfile ------------------------------------------------------ *)

  fun lockTests () =
    let
      val () = section "lockfile: golden"
      val r = okResolution (appDiamond, regDiamond)
      val () = checkString "diamond lockfile"
        ("# sml.lock -- generated by sml-pkg; do not edit by hand.\n" ^
         "root github.com/sjqtentacles/app\n" ^
         "\n" ^
         "github.com/sjqtentacles/alpha *\n" ^
         "github.com/sjqtentacles/beta *\n" ^
         "github.com/sjqtentacles/delta *\n",
         P.lockfile r)

      val () = section "lockfile: versions pinned"
      val rv = okResolution (appDiamond, regVersioned)
      val () = checkString "versioned lockfile"
        ("# sml.lock -- generated by sml-pkg; do not edit by hand.\n" ^
         "root github.com/sjqtentacles/app\n" ^
         "\n" ^
         "github.com/sjqtentacles/alpha *\n" ^
         "github.com/sjqtentacles/beta *\n" ^
         "github.com/sjqtentacles/delta 2\n",
         P.lockfile rv)

      val () = section "lockfile: empty (leaf root)"
      val leaf = okResolution (manifestOf (sj ^ "leaf", []), regDiamond)
      val () = checkString "leaf lockfile"
        ("# sml.lock -- generated by sml-pkg; do not edit by hand.\n" ^
         "root github.com/sjqtentacles/leaf\n",
         P.lockfile leaf)
    in () end

  (* ---- mlb ----------------------------------------------------------- *)

  fun mlbTests () =
    let
      val () = section "mlb: golden (default config)"
      val r = okResolution (appDiamond, regDiamond)
      val () = checkString "diamond mlb"
        ("(* Generated by sml-pkg for github.com/sjqtentacles/app. " ^
           "Do not edit by hand. *)\n" ^
         "$(SML_LIB)/basis/basis.mlb\n" ^
         "\n" ^
         "../lib/github.com/sjqtentacles/delta/sources.mlb\n" ^
         "../lib/github.com/sjqtentacles/alpha/sources.mlb\n" ^
         "../lib/github.com/sjqtentacles/beta/sources.mlb\n",
         P.mlb P.defaultMlbConfig r)

      val () = section "mlb: custom config"
      val () = checkString "custom prefix + file"
        ("(* Generated by sml-pkg for github.com/sjqtentacles/app. " ^
           "Do not edit by hand. *)\n" ^
         "$(SML_LIB)/basis/basis.mlb\n" ^
         "\n" ^
         "lib/github.com/sjqtentacles/delta/sml.mlb\n" ^
         "lib/github.com/sjqtentacles/alpha/sml.mlb\n" ^
         "lib/github.com/sjqtentacles/beta/sml.mlb\n",
         P.mlb { basisPrefix = "lib", eachFile = "sml.mlb" } r)

      val () = section "mlb: leaf (no deps)"
      val leaf = okResolution (manifestOf (sj ^ "leaf", []), regDiamond)
      val () = checkString "leaf mlb"
        ("(* Generated by sml-pkg for github.com/sjqtentacles/leaf. " ^
           "Do not edit by hand. *)\n" ^
         "$(SML_LIB)/basis/basis.mlb\n",
         P.mlb P.defaultMlbConfig leaf)
    in () end

  fun runAll () =
    (reset ();
     parseTests ();
     resolveTests ();
     lockTests ();
     mlbTests ();
     Harness.run ())

  val run = runAll
end
