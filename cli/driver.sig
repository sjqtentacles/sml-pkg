(* driver.sig -- the IMPURE edge of sml-pkg.

   This is the only module that performs I/O: reading files, scanning the
   `lib/github.com/...` vendoring tree, shelling out to `git`/`gh`/`mlton`/
   `poly`. It is deliberately thin and is NOT covered by the deterministic,
   dual-compiler golden suite (the pure logic lives in `structure Pkg`).

   Safety: shell-outs are restricted to `git`, `gh`, `mlton`, `poly`, and
   `polyc`, and clones only ever write into the repo's own `lib/` subtree.
   The driver never deletes anything outside the working directory. *)

signature DRIVER =
sig
  exception DriverError of string

  (* Read a file's full contents (raises DriverError if absent). *)
  val readFile  : string -> string

  (* Write a string to a file, replacing any existing contents. *)
  val writeFile : string -> string -> unit

  (* Does this path exist on disk? *)
  val exists : string -> bool

  (* Scan `<root>/lib/github.com/...` for vendored packages, parse each
     `sml.pkg`, and build a Pkg.registry from the results. The root manifest
     (at `<root>/sml.pkg`) is included if present. Malformed vendored
     manifests are skipped with a warning to stderr. *)
  val scanRegistry : string -> Pkg.registry

  (* Load and parse the root `sml.pkg` at `<root>/sml.pkg`. *)
  val loadManifest : string -> Pkg.manifest

  (* The local vendor directory for an import path, relative to root:
     `lib/<importpath>`. *)
  val vendorDir : string -> string

  (* `gitClone url dest`: shell out to `git clone --depth 1 url dest`.
     Returns true on success. `dest` must be inside the repo's `lib/`. *)
  val gitClone : string -> string -> bool

  (* `runMlton mlbFile out`: shell out to `mlton -output out mlbFile`. *)
  val runMlton : string -> string -> bool

  (* The directory containing the running process's working dir. *)
  val cwd : unit -> string
end
