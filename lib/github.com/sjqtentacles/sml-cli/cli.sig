(* cli.sig

   A declarative, dependency-free command-line argument parser for Standard
   ML (Basis only). You describe a program as a `spec` -- a list of options,
   flags and positionals, optionally split into named subcommands -- and then
   parse an explicit `string list` of arguments into a `result` from which
   typed values are read back.

   The core `parse` is pure: it never touches `CommandLine.arguments`, so
   tests are fully deterministic. A thin `parseArgv` wrapper is provided for
   real programs that do want the process argv.

   Parsing understands the usual GNU-ish conventions:
     --key value      --key=value      --flag
     -n value         -n5              -abc  (clustered short flags)
     --               terminates option parsing; the rest are positionals
   Unknown options, missing required options, and ill-typed values are
   reported as a distinguished `error`. *)

signature CLI =
sig
  (* ---- Argument kinds ------------------------------------------------- *)

  (* The type of value an option carries. `Flag` options take no value and
     are always boolean; `IntOpt`/`StrOpt` consume a following value. *)
  datatype argType = Flag | IntOpt | StrOpt

  (* A single declared option/flag. Build these with the helpers below
     rather than by hand; the record is exposed only for documentation. *)
  type arg = {
    long     : string,         (* long name, given WITHOUT the leading "--" *)
    short    : char option,    (* optional short name, e.g. #"n" for -n     *)
    argType  : argType,        (* Flag | IntOpt | StrOpt                    *)
    required : bool,           (* error if absent and no default            *)
    repeated : bool,           (* collect every occurrence into a list      *)
    default  : string option,  (* default raw value when absent (not Flag)  *)
    help     : string          (* one-line help text                        *)
  }

  (* A parser specification: a program name, a help blurb, the declared
     options/flags, and any named subcommands (each with its own spec). *)
  type spec

  (* ---- Building a spec ------------------------------------------------- *)

  (* Start an empty spec: `spec name about`. *)
  val spec : string -> string -> spec

  (* Append a declared option. The smart constructors below cover the
     common cases; `addArg` takes a full `arg` record for the rest. *)
  val addArg : arg -> spec -> spec

  (* `flag long short help`: a boolean flag, default false, repeatable-safe. *)
  val flag : string -> char option -> string -> spec -> spec

  (* `intOpt long short {required, default} help`: an option taking an int.
     The value must parse within the signed 32-bit range; a non-numeric or
     oversized value is a parse error ("expects an integer"), never an
     overflow of the default `int`. *)
  val intOpt :
    string -> char option -> {required : bool, default : int option}
    -> string -> spec -> spec

  (* `strOpt long short {required, default} help`: an option taking a string. *)
  val strOpt :
    string -> char option -> {required : bool, default : string option}
    -> string -> spec -> spec

  (* A repeatable string option whose occurrences collect into a list. *)
  val listOpt : string -> char option -> string -> spec -> spec

  (* `sub name subspec parent`: register a subcommand. When the first
     positional token matches `name`, the remaining args are parsed with
     `subspec` and dispatched there. *)
  val sub : string -> spec -> spec -> spec

  (* ---- Parse results --------------------------------------------------- *)

  (* A parsed value, as recovered for a given option name. *)
  datatype value =
      VBool   of bool
    | VInt    of int
    | VString of string
    | VList   of string list

  (* The outcome of a successful parse. `cmd` is the dispatched subcommand
     path (innermost last), `positionals` are the leftover non-option args in
     order, and option values are read with the accessors below. *)
  type result

  (* A parse failure with a human-readable, deterministic message. *)
  type error = string

  (* The outcome of parsing: either a usable `result` or an `error`. We use
     a small local datatype rather than the Basis `Either` so the library
     stays portable across compilers that predate it. *)
  datatype 'a parsed = Ok of 'a | Err of error

  (* ---- Parsing --------------------------------------------------------- *)

  (* The pure core: parse an explicit argument list against a spec. *)
  val parse : spec -> string list -> result parsed

  (* Convenience wrapper that reads `CommandLine.arguments ()`. Not used by
     the test suite (which always calls `parse` with an explicit list). *)
  val parseArgv : spec -> result parsed

  (* ---- Reading values back -------------------------------------------- *)

  (* The subcommand path that was dispatched, e.g. ["add"]. Empty at top. *)
  val command : result -> string list

  (* Positional (non-option) arguments, in the order given. *)
  val positionals : result -> string list

  (* Raw tagged lookup by long name; NONE if the option was never declared. *)
  val get : result -> string -> value option

  (* Typed accessors. `getBool` is total (absent flag => false); the others
     return NONE when the option is absent and has no default. They raise
     `Fail` only if you ask for the wrong type of a declared option. *)
  val getBool   : result -> string -> bool
  val getInt    : result -> string -> int option
  val getString : result -> string -> string option
  val getList   : result -> string -> string list

  (* ---- Help / usage ---------------------------------------------------- *)

  (* A deterministic usage/help string for a spec: the program name and
     blurb, an OPTIONS section listing each option with its help text, and a
     COMMANDS section for any subcommands. *)
  val usage : spec -> string
end
