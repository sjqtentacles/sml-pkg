(* pkg.sig

   sml-pkg: a PURE package-manifest resolver, lockfile, and `.mlb`
   generator for the sjqtentacles Standard ML ecosystem.

   The library models the on-disk `sml.pkg` manifest format used across the
   org (the same format consumed by the external `smlpkg` tool):

       package github.com/sjqtentacles/<name>

       require {
         github.com/sjqtentacles/<dep>
         github.com/sjqtentacles/<dep> <version>
       }

   The `package` line names the package by its import path. The `require`
   block lists zero or more dependencies, one per line, each an import path
   optionally followed by a single whitespace-separated version token (an
   integer like `1`, or a pseudo-version like
   `0.0.0-20260621120446+2466b0395...`). Blank lines and `(* ... *)` line
   comments are ignored.

   Everything in this structure is a pure, deterministic string/data
   transform -- no I/O, no clock, no environment. The same inputs always
   produce byte-identical outputs under MLton and Poly/ML. The impure build
   driver (fetching, process invocation) lives outside this core, in the
   `bin/` CLI.

   The four pure transforms exercised by the test suite are:

     parse     : string -> manifest result               (text  -> manifest)
     resolve   : manifest * registry -> resolution result (graph -> order)
     lockfile  : resolution -> string                     (order -> sml.lock)
     mlb       : mlbConfig -> resolution -> string         (order -> .mlb)
*)

signature PKG =
sig
  (* ---- Results: errors as values, not exceptions --------------------- *)

  (* A small local result type so the library stays portable across
     compilers that predate the Basis `result`. *)
  datatype ('a, 'e) result = Ok of 'a | Err of 'e

  (* ---- Manifest model ------------------------------------------------- *)

  (* A single dependency line: its import path (e.g.
     "github.com/sjqtentacles/sml-cli") and an optional version token,
     preserved verbatim as it appeared in the manifest. *)
  type require = { path : string, version : string option }

  (* A parsed `sml.pkg`: the package's own import path and the (declared,
     source-order) dependency list. Duplicate requires are rejected by the
     parser, so the list is duplicate-free. *)
  type manifest = { name : string, requires : require list }

  (* ---- Parse errors --------------------------------------------------- *)

  (* Parse failures carry the 1-based line number and a deterministic,
     human-readable message. *)
  type parseError = { line : int, message : string }

  (* Parse the textual content of an `sml.pkg` file into a `manifest`.
     Pure and total: never raises, returns `Err` on malformed input. *)
  val parse : string -> (manifest, parseError) result

  (* Render a manifest back to canonical `sml.pkg` text. `parse o render`
     is the identity on the manifest model (round-trips), and the output is
     stable and sorted-free (requires are emitted in declaration order). *)
  val render : manifest -> string

  (* The short repository name of an import path, i.e. the final path
     segment: "github.com/sjqtentacles/sml-cli" -> "sml-cli". *)
  val repoName : string -> string

  (* ---- Registry: injected available manifests ------------------------- *)

  (* A registry maps an import path to its manifest, if known. This is how
     the (otherwise impure) "what packages exist and what do they need"
     question is injected as pure data into resolution. *)
  type registry = string -> manifest option

  (* Build a registry from an explicit association list. Later entries do
     not shadow earlier ones; the list should be duplicate-free. *)
  val registryOf : (string * manifest) list -> registry

  (* ---- Resolution ----------------------------------------------------- *)

  (* One resolved node: the dependency's import path, repo name, the version
     constraint that selected it (the strongest non-NONE requirement seen,
     or NONE if every requirer left it unpinned), and its direct
     dependencies' import paths (sorted). *)
  type resolved = {
    path     : string,
    repo     : string,
    version  : string option,
    deps     : string list
  }

  (* A full resolution: the root package's import path and the transitive
     dependencies in a deterministic topological order (dependencies before
     dependents). The root itself is NOT included in `order`. *)
  type resolution = { root : string, order : resolved list }

  (* Why a resolution failed. `Missing` names an import path that some
     manifest requires but the registry does not know. `Cycle` reports the
     import paths forming a dependency cycle, in a canonical rotation
     (lexicographically smallest path first), with the first path repeated
     at the end to close the loop. `Conflict` reports a dependency required
     at two incompatible explicit versions. *)
  datatype resolveError =
      Missing  of { required_by : string, path : string }
    | Cycle    of string list
    | Conflict of { path : string, versions : string list }

  (* Resolve a root manifest against a registry: walk the transitive
     `require` graph, detect missing packages, version conflicts, and
     cycles, and produce a deterministic topological order. Pure. *)
  val resolve : manifest * registry -> (resolution, resolveError) result

  (* ---- Lockfile ------------------------------------------------------- *)

  (* Serialize a resolution to canonical `sml.lock` text: a stable, sorted,
     reproducible format. The entries are sorted by import path so the
     lockfile depends only on the dependency SET, not on traversal order. *)
  val lockfile : resolution -> string

  (* ---- .mlb generation ------------------------------------------------ *)

  (* Configuration for `.mlb` emission. `basisPrefix` is the relative path
     from the generated file to the vendoring root that contains
     `github.com/...` (e.g. "../lib" for a file in `src/`). `eachFile` names
     the per-dependency basis file to reference under each vendored repo
     (e.g. "sources.mlb"). *)
  type mlbConfig = { basisPrefix : string, eachFile : string }

  (* The conventional config: prefix "../lib", file "sources.mlb". *)
  val defaultMlbConfig : mlbConfig

  (* Generate `.mlb` content that pulls in the basis library and then each
     resolved dependency's vendored basis, in dependency order (dependencies
     before dependents -- the resolution's topological order). *)
  val mlb : mlbConfig -> resolution -> string

  (* ---- Pretty-printing errors (deterministic) ------------------------- *)

  val parseErrorToString   : parseError -> string
  val resolveErrorToString : resolveError -> string
end
