(* driver.sml -- impure I/O edge for the sml-pkg CLI. See driver.sig. *)

structure Driver :> DRIVER =
struct
  exception DriverError of string

  fun cwd () = OS.FileSys.getDir ()

  fun exists path = OS.FileSys.access (path, [])

  fun readFile path =
    let
      val ins = TextIO.openIn path
                handle IO.Io _ => raise DriverError ("cannot open " ^ path)
      val s = TextIO.inputAll ins
    in
      TextIO.closeIn ins; s
    end

  fun writeFile path s =
    let
      val outs = TextIO.openOut path
                 handle IO.Io _ => raise DriverError ("cannot write " ^ path)
    in
      TextIO.output (outs, s); TextIO.closeOut outs
    end

  fun warn s = TextIO.output (TextIO.stdErr, "sml-pkg: " ^ s ^ "\n")

  fun loadManifest root =
    let
      val path = OS.Path.concat (root, "sml.pkg")
      val text = readFile path
    in
      case Pkg.parse text of
        Pkg.Ok m => m
      | Pkg.Err e =>
          raise DriverError (Pkg.parseErrorToString e)
    end

  fun vendorDir importPath =
    OS.Path.concat ("lib", importPath)

  (* Walk lib/github.com/<owner>/<repo> two levels deep under the owner
     directories and collect every directory that holds an `sml.pkg`. We
     keep it simple and only look at the conventional
     `lib/github.com/<owner>/<repo>/sml.pkg` ... but vendored repos in this
     ecosystem do NOT ship their own sml.pkg inside lib/. Instead each
     vendored repo is a flat source dir. So we synthesize manifests from the
     vendoring layout: a vendored repo at lib/github.com/<owner>/<repo> with
     an importPath github.com/<owner>/<repo>, and we read its requires from
     the ROOT manifest's transitive closure when available, else treat as a
     leaf. The authoritative dependency facts come from each repo's own
     sml.pkg if one was vendored; otherwise from the root sml.pkg. *)

  fun listDir d =
    let
      val ds = OS.FileSys.openDir d
      fun loop acc =
        case OS.FileSys.readDir ds of
          NONE => List.rev acc
        | SOME name => loop (name :: acc)
    in
      let val r = loop [] in OS.FileSys.closeDir ds; r end
    end
    handle OS.SysErr _ => []

  fun isDir p = (OS.FileSys.isDir p) handle OS.SysErr _ => false

  (* Collect importPath * dir for every vendored repo under
     lib/github.com/<owner>/<repo>. *)
  fun vendoredRepos root =
    let
      val ghRoot = OS.Path.concat (root, "lib/github.com")
    in
      if not (isDir ghRoot) then []
      else
        List.foldl
          (fn (owner, acc) =>
             let
               val ownerDir = OS.Path.concat (ghRoot, owner)
             in
               if not (isDir ownerDir) then acc
               else
                 List.foldl
                   (fn (repo, acc2) =>
                      let
                        val repoDir = OS.Path.concat (ownerDir, repo)
                      in
                        if isDir repoDir then
                          ("github.com/" ^ owner ^ "/" ^ repo, repoDir) :: acc2
                        else acc2
                      end)
                   acc
                   (listDir ownerDir)
             end)
          []
          (listDir ghRoot)
    end

  (* Build a registry. For each vendored repo, if it ships its own sml.pkg,
     parse it for accurate requires; otherwise register it as a leaf (no
     requires). The root manifest is always registered authoritatively. *)
  fun scanRegistry root =
    let
      val repos = vendoredRepos root
      val vendored =
        List.mapPartial
          (fn (importPath, dir) =>
             let
               val pkgPath = OS.Path.concat (dir, "sml.pkg")
             in
               if exists pkgPath then
                 (case Pkg.parse (readFile pkgPath) of
                    Pkg.Ok m => SOME (importPath, m)
                  | Pkg.Err e =>
                      ( warn ("skipping malformed " ^ pkgPath ^ ": "
                              ^ Pkg.parseErrorToString e)
                      ; SOME (importPath,
                              { name = importPath, requires = [] }) ))
               else
                 SOME (importPath, { name = importPath, requires = [] })
             end)
          repos
      val withRoot =
        (let val m = loadManifest root in (#name m, m) :: vendored end)
        handle _ => vendored
    in
      Pkg.registryOf withRoot
    end

  (* ---- safe shell-outs ----------------------------------------------- *)

  fun shquote s = "'" ^ String.translate
                          (fn #"'" => "'\\''" | c => str c) s ^ "'"

  fun systemOk cmd =
    OS.Process.isSuccess (OS.Process.system cmd)

  fun gitClone url dest =
    (* dest must live under lib/ -- refuse anything else *)
    if String.isPrefix "lib/" dest orelse String.isPrefix "./lib/" dest then
      systemOk ("git clone --depth 1 " ^ shquote url ^ " " ^ shquote dest)
    else
      raise DriverError ("refusing to clone outside lib/: " ^ dest)

  fun runMlton mlbFile out =
    systemOk ("mlton -output " ^ shquote out ^ " " ^ shquote mlbFile)
end
