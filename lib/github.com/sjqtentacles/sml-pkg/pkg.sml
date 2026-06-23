(* pkg.sml -- pure core of sml-pkg. See pkg.sig for the contract. *)

structure Pkg :> PKG =
struct
  datatype ('a, 'e) result = Ok of 'a | Err of 'e

  type require = { path : string, version : string option }
  type manifest = { name : string, requires : require list }
  type parseError = { line : int, message : string }
  type registry = string -> manifest option

  type resolved = {
    path     : string,
    repo     : string,
    version  : string option,
    deps     : string list
  }
  type resolution = { root : string, order : resolved list }

  datatype resolveError =
      Missing  of { required_by : string, path : string }
    | Cycle    of string list
    | Conflict of { path : string, versions : string list }

  type mlbConfig = { basisPrefix : string, eachFile : string }

  (* ---- small helpers ------------------------------------------------- *)

  fun isWs c = c = #" " orelse c = #"\t" orelse c = #"\r"

  fun ltrim s =
    Substring.string (Substring.dropl isWs (Substring.full s))
  fun rtrim s =
    Substring.string (Substring.dropr isWs (Substring.full s))
  fun trim s = rtrim (ltrim s)

  (* Strip a single trailing block-comment-style line comment. The manifest
     grammar only uses whole-line comments, so we remove from the first
     comment opener to end-of-line. *)
  fun stripComment s =
    let
      fun scan i =
        if i + 1 >= String.size s then s
        else if String.sub (s, i) = #"(" andalso String.sub (s, i + 1) = #"*"
             then String.substring (s, 0, i)
        else scan (i + 1)
    in scan 0 end

  (* Split a string into fields on runs of whitespace. *)
  fun fields s =
    List.filter (fn t => t <> "")
      (String.tokens isWs s)

  (* Split into lines on #"\n". *)
  fun lines s = String.fields (fn c => c = #"\n") s

  fun repoName path =
    let val segs = String.fields (fn c => c = #"/") path
    in case List.rev segs of
         (last :: _) => last
       | [] => path
    end

  (* ---- parsing ------------------------------------------------------- *)

  (* The parser is a tiny line-oriented state machine. States:
       Top      -- expecting `package <path>` then `require {`
       InReq    -- inside the require block, collecting deps until `}`
       Done     -- after the closing `}`
  *)

  fun parse text =
    let
      val ls = lines text

      (* annotate each line with its 1-based number, stripped of comments
         and surrounding whitespace *)
      fun clean raw = trim (stripComment raw)

      fun err ln msg = Err { line = ln, message = msg }

      (* parse a single require line into a `require`, or an error message *)
      fun parseReq ln content =
        case fields content of
          [] => NONE  (* blank after stripping; skip *)
        | [p] => SOME (Ok { path = p, version = NONE })
        | [p, v] => SOME (Ok { path = p, version = SOME v })
        | _ => SOME (err ln
            "malformed require line: expected `path` or `path version`")

      fun hasDup (r : require) seen =
        List.exists (fn (x : require) => #path x = #path r) seen

      (* phase 1: find package line *)
      fun goTop (ln, []) = err ln "missing `package` declaration"
        | goTop (ln, raw :: rest) =
            let val c = clean raw in
              if c = "" then goTop (ln + 1, rest)
              else
                case fields c of
                  ["package", name] => goReqHeader (ln + 1, rest, name)
                | ("package" :: _) =>
                    err ln "malformed `package` line: expected `package <path>`"
                | _ => err ln "expected `package <path>` declaration"
            end

      (* phase 2: find the `require {` opener (it is mandatory in the
         observed format; an empty block is still written explicitly) *)
      and goReqHeader (ln, [], name) =
            (* no require block at all: treat as zero deps *)
            Ok { name = name, requires = [] }
        | goReqHeader (ln, raw :: rest, name) =
            let val c = clean raw in
              if c = "" then goReqHeader (ln + 1, rest, name)
              else
                case fields c of
                  ["require", "{"] => goInReq (ln + 1, rest, name, [])
                | ["require{"] => goInReq (ln + 1, rest, name, [])
                | ("require" :: _) =>
                    err ln "malformed `require` header: expected `require {`"
                | _ => err ln "expected `require {` block"
            end

      and goInReq (ln, [], _, _) =
            err ln "unterminated `require` block: missing `}`"
        | goInReq (ln, raw :: rest, name, acc) =
            let val c = clean raw in
              if c = "" then goInReq (ln + 1, rest, name, acc)
              else if c = "}" then goDone (ln + 1, rest, name, List.rev acc)
              else
                case parseReq ln c of
                  NONE => goInReq (ln + 1, rest, name, acc)
                | SOME (Err e) => Err e
                | SOME (Ok r) =>
                    if hasDup r acc then
                      err ln ("duplicate dependency: " ^ #path r)
                    else
                      goInReq (ln + 1, rest, name, r :: acc)
            end

      (* phase 3: only blank lines / comments allowed after `}` *)
      and goDone (ln, [], name, reqs) =
            Ok { name = name, requires = reqs }
        | goDone (ln, raw :: rest, name, reqs) =
            let val c = clean raw in
              if c = "" then goDone (ln + 1, rest, name, reqs)
              else err ln ("unexpected content after `require` block: " ^ c)
            end
    in
      goTop (1, ls)
    end

  fun render (m : manifest) =
    let
      val { name, requires } = m
      fun reqLine ({ path, version } : require) =
        case version of
          NONE => "  " ^ path ^ "\n"
        | SOME v => "  " ^ path ^ " " ^ v ^ "\n"
      val body = String.concat (List.map reqLine requires)
    in
      "package " ^ name ^ "\n\n" ^
      "require {\n" ^ body ^ "}\n"
    end

  (* ---- registry ------------------------------------------------------ *)

  fun registryOf assoc =
    (fn path =>
       let
         fun look [] = NONE
           | look ((k, v) :: rest) = if k = path then SOME v else look rest
       in look assoc end)

  (* ---- resolution ---------------------------------------------------- *)

  (* String comparison shortcuts. *)
  val scmp = String.compare
  fun sLt (a, b) = scmp (a, b) = LESS
  fun insertSorted (x, xs) =
    let
      fun go [] = [x]
        | go (y :: ys) =
            (case scmp (x, y) of
               LESS => x :: y :: ys
             | EQUAL => y :: ys
             | GREATER => y :: go ys)
    in go xs end
  fun sortUniq xs = List.foldr (fn (x, acc) => insertSorted (x, acc)) [] xs

  (* version "strength": an explicit (SOME) version beats NONE; two
     differing explicit versions are a conflict. We merge as we discover
     each requirement edge. *)

  exception Resolve of resolveError

  fun resolve (root : manifest, reg : registry) =
    let
      val rootPath = #name root

      (* Collect the closure of import paths reachable from root's requires.
         We also accumulate, per path: the merged version (option) and the
         sorted set of its direct deps. Missing/conflict reported eagerly. *)

      fun member (x, xs) = List.exists (fn y => y = x) xs

      (* path -> (version option, sorted dep paths) accumulator as assoc
         list keyed by path *)
      fun mergeVersion (path, vOld, vNew) =
        case (vOld, vNew) of
          (NONE, x) => x
        | (x, NONE) => x
        | (SOME a, SOME b) =>
            if a = b then SOME a
            else raise Resolve
              (Conflict { path = path,
                          versions = sortUniq [a, b] })

      (* Walk requires of a given manifest, registering edges. `requirer`
         is the path that listed these requires (for Missing reporting). *)

      (* visited: assoc (path -> {version, deps}); pending stack of paths to
         expand. We expand breadth-first deterministically. *)

      fun lookup acc path =
        let
          fun go [] = NONE
            | go ((p, info) :: rest) =
                if p = path then SOME info else go rest
        in go acc end

      fun setInfo acc (path, info) =
        let
          fun go [] = [(path, info)]
            | go ((p, i) :: rest) =
                if p = path then (path, info) :: rest
                else (p, i) :: go rest
        in go acc end

      (* process a require edge requirer -> (path,version) *)
      fun visit (requirer, acc, frontier) ({ path, version } : require) =
        let
          val (curV, curDeps, known) =
            case lookup acc path of
              SOME (v, d) => (v, d, true)
            | NONE => (NONE, [], false)
          val mergedV = mergeVersion (path, curV, version)
        in
          if known then
            (* already discovered; just update version, deps already set *)
            (setInfo acc (path, (mergedV, curDeps)), frontier)
          else
            case reg path of
              NONE => raise Resolve
                (Missing { required_by = requirer, path = path })
            | SOME (m : manifest) =>
                let
                  val childPaths =
                    sortUniq (List.map #path (#requires m))
                  val acc' = setInfo acc (path, (mergedV, childPaths))
                in
                  (acc', frontier @ [(path, #requires m)])
                end
        end

      (* expand the frontier: a worklist of (requirerPath, requireList) *)
      fun expand (acc, []) = acc
        | expand (acc, (requirer, reqs) :: rest) =
            let
              val (acc', newFrontier) =
                List.foldl
                  (fn (r, (a, fr)) => visit (requirer, a, fr) r)
                  (acc, rest)
                  reqs
            in expand (acc', newFrontier) end

      (* seed with the root's own requires *)
      val seeded = expand ([], [(rootPath, #requires root)])

      (* Now `seeded` maps every transitive dep path to (version, deps).
         Build a deterministic topological order via Kahn's algorithm with
         lexicographic tie-breaking, restricting edges to nodes within the
         closure (the root is excluded from the node set). *)

      val nodes = sortUniq (List.map #1 seeded)

      fun depsOf path =
        case lookup seeded path of
          SOME (_, d) => List.filter (fn p => member (p, nodes)) d
        | NONE => []

      (* rotate `raw` (a simple cycle, traversal order, no repeat) so the
         lexicographically smallest path is first, then close the loop by
         repeating that path at the end. *)
      fun canonicalCycle raw =
        case raw of
          [] => []
        | (h :: t) =>
            let
              val minp =
                List.foldl (fn (x, m) => if sLt (x, m) then x else m) h t
              fun idx (_, []) = 0
                | idx (i, y :: ys) = if y = minp then i else idx (i + 1, ys)
              val k = idx (0, raw)
              fun dropN (0, ys) = ys
                | dropN (_, []) = []
                | dropN (n, _ :: ys) = dropN (n - 1, ys)
              fun takeN (0, _) = []
                | takeN (_, []) = []
                | takeN (n, y :: ys) = y :: takeN (n - 1, ys)
              val rotated = dropN (k, raw) @ takeN (k, raw)
            in rotated @ [minp] end

      (* find a cycle within `remaining` by DFS over the restricted edge
         set, returning a canonical rotation. *)
      fun findCycle remaining =
        let
          fun edgesOf p =
            List.filter (fn d => member (d, remaining)) (depsOf p)
          (* stack is traversal order, oldest first *)
          fun dfs (p, stack) =
            if member (p, stack) then
              let
                fun suffixFrom [] = []
                  | suffixFrom (y :: ys) =
                      if y = p then y :: ys else suffixFrom ys
              in SOME (suffixFrom stack) end
            else
              let
                fun tryEdges [] = NONE
                  | tryEdges (e :: es) =
                      (case dfs (e, stack @ [p]) of
                         SOME c => SOME c
                       | NONE => tryEdges es)
              in tryEdges (edgesOf p) end
          fun search [] = []
            | search (p :: ps) =
                (case dfs (p, []) of SOME c => c | NONE => search ps)
        in
          canonicalCycle (search remaining)
        end

      (* Kahn: repeatedly pick the lexicographically-smallest node whose
         remaining in-deps are all already emitted. If none can be picked
         but nodes remain, there is a cycle. *)
      fun kahn (remaining, emitted) =
        if null remaining then List.rev emitted
        else
          let
            fun ready path =
              List.all (fn d => member (d, emitted) orelse not (member (d, remaining)))
                       (depsOf path)
            fun pick [] = NONE
              | pick (p :: ps) = if ready p then SOME p else pick ps
          in
            case pick remaining of
              SOME p =>
                kahn (List.filter (fn r => r <> p) remaining, p :: emitted)
            | NONE =>
                raise Resolve (Cycle (findCycle remaining))
          end

      val orderPaths = kahn (nodes, [])

      fun toResolved path =
        case lookup seeded path of
          SOME (v, d) =>
            { path = path, repo = repoName path, version = v,
              deps = List.filter (fn p => List.exists (fn n => n = p) nodes) d }
        | NONE =>
            { path = path, repo = repoName path, version = NONE, deps = [] }

      val order = List.map toResolved orderPaths
    in
      Ok { root = rootPath, order = order }
    end
    handle Resolve e => Err e

  (* ---- lockfile ------------------------------------------------------ *)

  (* Canonical sml.lock: a header line, the root, and one line per
     dependency SORTED by import path (so the lockfile depends only on the
     dependency set + selected versions, not traversal order). Each line is
     `<path> <version>` where an unpinned dep gets the literal `*`. *)

  fun lockfile (r : resolution) =
    let
      val { root, order } = r
      fun verStr NONE = "*"
        | verStr (SOME v) = v
      fun insBy (x : resolved, xs) =
        let
          fun go [] = [x]
            | go (y :: ys) =
                if sLt (#path x, #path y) then x :: y :: ys else y :: go ys
        in go xs end
      val asc = List.foldr (fn (x, acc) => insBy (x, acc)) [] order
      fun line (d : resolved) =
        #path d ^ " " ^ verStr (#version d) ^ "\n"
      val body = String.concat (List.map line asc)
    in
      "# sml.lock -- generated by sml-pkg; do not edit by hand.\n" ^
      "root " ^ root ^ "\n" ^
      (if null asc then "" else "\n" ^ body)
    end

  (* ---- .mlb generation ----------------------------------------------- *)

  type mlbConfig = { basisPrefix : string, eachFile : string }
  val defaultMlbConfig = { basisPrefix = "../lib", eachFile = "sources.mlb" }

  fun mlb (cfg : mlbConfig) (r : resolution) =
    let
      val { basisPrefix, eachFile } = cfg
      val { root, order } = r
      fun depPath (d : resolved) =
        basisPrefix ^ "/" ^ #path d ^ "/" ^ eachFile
      val depLines =
        String.concat (List.map (fn d => depPath d ^ "\n") order)
    in
      "(* Generated by sml-pkg for " ^ root ^ ". Do not edit by hand. *)\n" ^
      "$(SML_LIB)/basis/basis.mlb\n" ^
      (if null order then "" else "\n" ^ depLines)
    end

  (* ---- error rendering ----------------------------------------------- *)

  fun parseErrorToString { line, message } =
    "sml.pkg:" ^ Int.toString line ^ ": " ^ message

  fun resolveErrorToString e =
    case e of
      Missing { required_by, path } =>
        "missing dependency: " ^ path ^ " (required by " ^ required_by ^ ")"
    | Cycle paths =>
        "dependency cycle: " ^ String.concatWith " -> " paths
    | Conflict { path, versions } =>
        "version conflict for " ^ path ^ ": " ^
        String.concatWith ", " versions
end
