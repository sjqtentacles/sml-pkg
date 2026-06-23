(* pkg.sml (cli) -- the thin, impure `sml-pkg` command-line front end.

   Built on the vendored `sml-cli` argument parser. Subcommands:

     sml-pkg resolve            read ./sml.pkg, resolve against vendored
                                lib/..., write ./sml.lock and print it.
     sml-pkg lock               alias for resolve.
     sml-pkg sync   [--dry-run] ensure every resolved dependency is present
                                under lib/github.com/...; `git clone` the
                                missing ones (the impure edge). --dry-run
                                only reports what would be fetched.
     sml-pkg vendor             alias for sync.
     sml-pkg mlb    [--out F]   generate the dependency .mlb and print it
                                (or write it to F).
     sml-pkg build  [--out F]   generate the .mlb (default bin/sml-pkg.mlb)
                                and invoke `mlton` on it.

   Only `sync`/`build` touch the network/processes; `resolve`/`lock`/`mlb`
   are pure transforms over on-disk data. All dependency math is delegated
   to the pure `structure Pkg`. *)

structure Main =
struct
  fun err s =
    ( TextIO.output (TextIO.stdErr, "sml-pkg: " ^ s ^ "\n")
    ; OS.Process.exit OS.Process.failure )

  fun ok () = OS.Process.exit OS.Process.success

  val root = "."

  fun resolveOrDie () =
    let
      val m = Driver.loadManifest root
      val reg = Driver.scanRegistry root
    in
      case Pkg.resolve (m, reg) of
        Pkg.Ok r => r
      | Pkg.Err e => err (Pkg.resolveErrorToString e)
    end

  (* ---- subcommands ---------------------------------------------------- *)

  fun cmdResolve () =
    let
      val r = resolveOrDie ()
      val lock = Pkg.lockfile r
    in
      Driver.writeFile (OS.Path.concat (root, "sml.lock")) lock;
      print lock;
      ok ()
    end

  fun cmdMlb out =
    let
      val r = resolveOrDie ()
      val text = Pkg.mlb Pkg.defaultMlbConfig r
    in
      case out of
        NONE => (print text; ok ())
      | SOME f => (Driver.writeFile f text;
                   print ("wrote " ^ f ^ "\n"); ok ())
    end

  fun cmdSync dryRun =
    let
      val r = resolveOrDie ()
      val order = #order r
      fun dest (d : Pkg.resolved) = Driver.vendorDir (#path d)
      val missing =
        List.filter (fn d => not (Driver.exists (dest d))) order
    in
      if null missing then
        (print "all dependencies present.\n"; ok ())
      else if dryRun then
        ( List.app (fn d => print ("would fetch " ^ #path d ^ " -> "
                                   ^ dest d ^ "\n")) missing
        ; ok () )
      else
        let
          fun fetch (d : Pkg.resolved) =
            let
              val url = "https://" ^ #path d ^ ".git"
              val target = dest d
            in
              print ("fetching " ^ #path d ^ " -> " ^ target ^ "\n");
              if Driver.gitClone url target then ()
              else err ("git clone failed for " ^ #path d)
            end
        in
          List.app fetch missing;
          print "sync complete.\n";
          ok ()
        end
    end

  fun cmdBuild out =
    let
      val r = resolveOrDie ()
      val mlbText = Pkg.mlb Pkg.defaultMlbConfig r
      val mlbFile =
        case out of SOME f => f | NONE => "bin/sml-pkg.mlb"
    in
      Driver.writeFile mlbFile mlbText;
      print ("generated " ^ mlbFile ^ "\n");
      if Driver.runMlton mlbFile "bin/sml-pkg" then
        (print "build complete: bin/sml-pkg\n"; ok ())
      else
        err "mlton build failed"
    end

  (* ---- spec ----------------------------------------------------------- *)

  val spec =
    let
      open Cli
      val resolveSpec = spec "resolve" "resolve deps and write sml.lock"
      val lockSpec    = spec "lock"    "alias for resolve"
      val syncSpec    =
        flag "dry-run" NONE "only report what would be fetched"
             (spec "sync" "fetch any missing vendored dependencies")
      val vendorSpec  =
        flag "dry-run" NONE "only report what would be fetched"
             (spec "vendor" "alias for sync")
      val mlbSpec     =
        strOpt "out" (SOME #"o") {required = false, default = NONE}
               "write the .mlb here instead of stdout"
               (spec "mlb" "generate and print the dependency .mlb")
      val buildSpec   =
        strOpt "out" (SOME #"o") {required = false, default = NONE}
               "write the .mlb here (default bin/sml-pkg.mlb)"
               (spec "build" "generate the .mlb and invoke mlton")
      val top = spec "sml-pkg" "pure SML package resolver + thin build driver"
    in
      sub "resolve" resolveSpec
        (sub "lock" lockSpec
          (sub "sync" syncSpec
            (sub "vendor" vendorSpec
              (sub "mlb" mlbSpec
                (sub "build" buildSpec top)))))
    end

  fun dispatch r =
    case Cli.command r of
      ["resolve"] => cmdResolve ()
    | ["lock"]    => cmdResolve ()
    | ["sync"]    => cmdSync (Cli.getBool r "dry-run")
    | ["vendor"]  => cmdSync (Cli.getBool r "dry-run")
    | ["mlb"]     => cmdMlb (Cli.getString r "out")
    | ["build"]   => cmdBuild (Cli.getString r "out")
    | _ => (print (Cli.usage spec); ok ())

  val () =
    case Cli.parseArgv spec of
      Cli.Ok r => dispatch r
    | Cli.Err e => err e
    handle Driver.DriverError m => err m
         | e => err (exnMessage e)
end
