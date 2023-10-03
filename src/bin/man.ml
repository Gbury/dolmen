
(* This file is free software, part of dolmen. See file "LICENSE" for more information *)

(* Manpage *)
(* ************************************************************************* *)

let cli = [
  `S Cmdliner.Manpage.s_description;
  `P "Dolmen is a tool to parse and type input files that contain problem \
      used in automated deduction.";
  `S Options.common_section;
  `P "Generic options for CLI binaries";
  `S Options.option_section;
  `P "Common options for the dolmen binary";
  `S Options.model_section;
  `P "Options to use dolmen to verify models output by provers";
  `S Options.flow_section;
  `P "Options to control checking of flow constraints in input files.";
  `P "Some languages are descriptive (e.g. tptp), and thus are correct by \
      construction, whereas some languages are imperative (in the sense that \
      instead of describing a problem, they consist of a list of instructions \
      or statements). Such imperative languages often have constraints on \
      what statements can appear in which contexts. For instance, smtlib \
      requires that any assertion be preceded by a set-logic statement, and \
      similarly any get-model statement must be preceded by a check-sat \
      statement (with no assertion in beetween)";
  `S Options.header_section;
  `P "Options to control the checking of headers in the input file";
  `S Options.error_section;
  `P "Options to customize the behaviour of dolmen on errors/warnings";
  `P "A warning can have one of three status: Disabled, Enabled, and Fatal. \
      When disabled, a warning will be ignored, when enabled, it will be
        printed, and when fatal, it will be transformed into an error.";
  `S Options.profiling_section;
  `P (Format.asprintf
        "Options to profile Dolmen.%s"
        (if Memory_profiler.available then "" else
           " WARNING: Memory profiling is not available on this version
            of Dolmen. You should install memtrace and recompile Dolmen
            if you desire to use memory profiling.")
     );
  `S Options.gc_section;
  `P "Options to fine-tune the gc, only experts should use these.";
  `S Options.internal_section;
  `P "Options for internal testing of Dolmen, use at your own risks, ^^";
  `S Cmdliner.Manpage.s_exit_status;
  `P "dolmen exits with the following status:";
  `S Cmdliner.Manpage.s_bugs;
  `P "You can report bugs at https://github.com/Gbury/dolmen/issues";
  `S Cmdliner.Manpage.s_authors;
  `P "Guillaume Bury <guillaume.bury@gmail.com>";
]
