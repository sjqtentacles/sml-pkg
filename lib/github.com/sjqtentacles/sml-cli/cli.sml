(* cli.sml

   Implementation of the declarative argument parser described in cli.sig.
   Everything here is pure Standard ML over the Basis library. *)

structure Cli :> CLI =
struct
  datatype argType = Flag | IntOpt | StrOpt

  type arg = {
    long     : string,
    short    : char option,
    argType  : argType,
    required : bool,
    repeated : bool,
    default  : string option,
    help     : string
  }

  (* A spec carries its name/blurb, declared args (kept in declaration order
     by appending), and named subcommands paired with their own specs. *)
  datatype spec = Spec of {
    name : string,
    about : string,
    args : arg list,
    subs : (string * spec) list
  }

  datatype value =
      VBool   of bool
    | VInt    of int
    | VString of string
    | VList   of string list

  (* Bindings map a long name to its recovered value. *)
  datatype result = Result of {
    cmd : string list,
    positionals : string list,
    bindings : (string * value) list
  }

  type error = string

  datatype 'a parsed = Ok of 'a | Err of error

  (* ---- Building a spec ------------------------------------------------- *)

  fun spec name about = Spec {name = name, about = about, args = [], subs = []}

  fun addArg a (Spec {name, about, args, subs}) =
    Spec {name = name, about = about, args = args @ [a], subs = subs}

  fun flag long short help =
    addArg {long = long, short = short, argType = Flag, required = false,
            repeated = false, default = NONE, help = help}

  fun intOpt long short {required, default} help =
    addArg {long = long, short = short, argType = IntOpt, required = required,
            repeated = false,
            default = Option.map Int.toString default, help = help}

  fun strOpt long short {required, default} help =
    addArg {long = long, short = short, argType = StrOpt, required = required,
            repeated = false, default = default, help = help}

  fun listOpt long short help =
    addArg {long = long, short = short, argType = StrOpt, required = false,
            repeated = true, default = NONE, help = help}

  fun sub subname subspec (Spec {name, about, args, subs}) =
    Spec {name = name, about = about, args = args, subs = subs @ [(subname, subspec)]}

  (* ---- Lookups over a spec -------------------------------------------- *)

  fun specArgs (Spec {args, ...}) = args
  fun specSubs (Spec {subs, ...}) = subs
  fun specName (Spec {name, ...}) = name

  fun findLong (Spec {args, ...}) long =
    List.find (fn a => #long a = long) args

  fun findShort (Spec {args, ...}) c =
    List.find (fn a => #short a = SOME c) args

  fun findSub sp name = List.find (fn (n, _) => n = name) (specSubs sp)

  (* ---- Parsing --------------------------------------------------------- *)

  (* Accumulator state during a single (sub)spec parse. `seen` collects
     (long, raw-occurrence) pairs in encounter order; `pos` the positionals. *)
  fun parseSpec sp (cmdPath : string list) (args0 : string list) : result parsed =
    let
      (* Record an occurrence of option `a` with raw value `raw`. *)
      fun add seen a raw = seen @ [(#long a, #argType a, #repeated a, raw)]

      (* Consume one VALUE-bearing option whose value may be the next token. *)
      fun takeValue a inlineVal rest seen pos =
        case inlineVal of
            SOME v => loop rest (add seen a v) pos
          | NONE =>
              (case rest of
                   v :: rest' => loop rest' (add seen a v) pos
                 | [] => Err ("option --" ^ #long a ^ " requires a value"))

      (* Handle a single long token "--name", "--name=value". *)
      and longTok tok rest seen pos =
        let
          val body = String.extract (tok, 2, NONE)
          val (key, inlineVal) =
            case CharVector.findi (fn (_, c) => c = #"=") body of
                SOME (i, _) =>
                  (String.substring (body, 0, i),
                   SOME (String.extract (body, i + 1, NONE)))
              | NONE => (body, NONE)
        in
          case findLong sp key of
              NONE => Err ("unknown option --" ^ key)
            | SOME a =>
                (case #argType a of
                     Flag =>
                       (case inlineVal of
                            SOME _ => Err ("flag --" ^ key ^ " takes no value")
                          | NONE => loop rest (add seen a "true") pos)
                   | _ => takeValue a inlineVal rest seen pos)
        end

      (* Handle a short cluster "-abc", "-n5", "-n". The leading '-' is at 0. *)
      and shortTok tok rest seen pos =
        let
          (* Walk characters after '-'. A value-taking short option swallows
             the remainder of the token (if any) or the next token. *)
          fun walk i seen =
            if i >= String.size tok then loop rest seen pos
            else
              let val c = String.sub (tok, i) in
                case findShort sp c of
                    NONE => Err ("unknown option -" ^ String.str c)
                  | SOME a =>
                      (case #argType a of
                           Flag => walk (i + 1) (add seen a "true")
                         | _ =>
                             let
                               val rem = String.extract (tok, i + 1, NONE)
                             in
                               if rem <> ""
                               then loop rest (add seen a rem) pos
                               else
                                 (case rest of
                                      v :: rest' =>
                                        loop rest' (add seen a v) pos
                                    | [] =>
                                        Err ("option -" ^ String.str c
                                             ^ " requires a value"))
                             end)
              end
        in
          walk 1 seen
        end

      (* Main token loop. Returns once all tokens are consumed. *)
      and loop [] seen pos = finish seen (List.rev pos)
        | loop (tok :: rest) seen pos =
            if tok = "--" then
              (* Everything after "--" is positional. *)
              finish seen (List.rev pos @ rest)
            else if String.isPrefix "--" tok then
              longTok tok rest seen pos
            else if String.size tok >= 2 andalso String.sub (tok, 0) = #"-"
                    andalso tok <> "-" then
              shortTok tok rest seen pos
            else
              (* A bare token: maybe a subcommand (only as the FIRST
                 positional), otherwise an ordinary positional. *)
              (case (pos, findSub sp tok) of
                   ([], SOME (subname, subspec)) =>
                     parseSpec subspec (cmdPath @ [subname]) rest
                 | _ => loop rest seen (tok :: pos))

      (* Turn the raw occurrences + positionals into a result, applying
         defaults, requiredness, and int typing. *)
      and finish seen pos =
        let
          (* Build the value for one declared arg from its occurrences. *)
          fun occOf long =
            List.mapPartial
              (fn (l, _, _, raw) => if l = long then SOME raw else NONE) seen

          fun build (a : arg) =
            let val occ = occOf (#long a) in
              case #argType a of
                  Flag => Ok (SOME (#long a, VBool (not (null occ))))
                | StrOpt =>
                    if #repeated a then
                      (case occ of
                           [] => Ok NONE
                         | _ => Ok (SOME (#long a, VList occ)))
                    else
                      (case occ of
                           [] =>
                             (case #default a of
                                  SOME d => Ok (SOME (#long a, VString d))
                                | NONE =>
                                    if #required a
                                    then Err ("missing required option --"
                                              ^ #long a)
                                    else Ok NONE)
                         | _ =>
                             Ok (SOME (#long a, VString (List.last occ))))
                | IntOpt =>
                    let
                      val rawOpt =
                        case occ of
                            [] => #default a
                          | _ => SOME (List.last occ)
                    in
                      case rawOpt of
                          NONE =>
                            if #required a
                            then Err ("missing required option --" ^ #long a)
                            else Ok NONE
                        | SOME raw =>
                            (case Int.fromString raw of
                                 SOME n => Ok (SOME (#long a, VInt n))
                               | NONE =>
                                   Err ("option --" ^ #long a
                                        ^ " expects an integer, got \""
                                        ^ raw ^ "\""))
                    end
            end

          fun gather [] acc = Ok (List.rev acc)
            | gather (a :: rest) acc =
                (case build a of
                     Err e => Err e
                   | Ok NONE => gather rest acc
                   | Ok (SOME b) => gather rest (b :: acc))
        in
          case gather (specArgs sp) [] of
              Err e => Err e
            | Ok bindings =>
                Ok (Result {cmd = cmdPath, positionals = pos,
                            bindings = bindings})
        end
    in
      loop args0 [] []
    end

  fun parse sp args = parseSpec sp [] args

  fun parseArgv sp = parse sp (CommandLine.arguments ())

  (* ---- Reading values back -------------------------------------------- *)

  fun command (Result {cmd, ...}) = cmd
  fun positionals (Result {positionals = p, ...}) = p

  fun get (Result {bindings, ...}) long =
    Option.map (fn (_, v) => v)
      (List.find (fn (l, _) => l = long) bindings)

  fun getBool r long =
    case get r long of SOME (VBool b) => b | _ => false

  fun getInt r long =
    case get r long of
        SOME (VInt n) => SOME n
      | SOME _ => raise Fail ("option --" ^ long ^ " is not an int")
      | NONE => NONE

  fun getString r long =
    case get r long of
        SOME (VString s) => SOME s
      | SOME _ => raise Fail ("option --" ^ long ^ " is not a string")
      | NONE => NONE

  fun getList r long =
    case get r long of SOME (VList xs) => xs | _ => []

  (* ---- Help / usage --------------------------------------------------- *)

  (* Render one option line: "  -n, --num <int>   help". Boolean flags omit
     the value placeholder. *)
  fun argLine (a : arg) =
    let
      val shortPart =
        case #short a of SOME c => "-" ^ String.str c ^ ", " | NONE => "    "
      val placeholder =
        case #argType a of
            Flag => ""
          | IntOpt => " <int>"
          | StrOpt => if #repeated a then " <str>..." else " <str>"
      val flags = shortPart ^ "--" ^ #long a ^ placeholder
    in
      "  " ^ flags ^ "  " ^ #help a
    end

  fun usage sp =
    let
      val name = specName sp
      val Spec {about, ...} = sp
      val header = name ^ " - " ^ about ^ "\n"
      val usageLine = "usage: " ^ name ^ " [options]"
        ^ (case specSubs sp of [] => "" | _ => " <command>") ^ "\n"
      val optLines =
        case specArgs sp of
            [] => ""
          | args =>
              "\noptions:\n"
              ^ String.concatWith "\n" (List.map argLine args) ^ "\n"
      val subLines =
        case specSubs sp of
            [] => ""
          | subs =>
              "\ncommands:\n"
              ^ String.concatWith "\n"
                  (List.map
                     (fn (n, Spec {about = a, ...}) => "  " ^ n ^ "  " ^ a)
                     subs)
              ^ "\n"
    in
      header ^ usageLine ^ optLines ^ subLines
    end
end
