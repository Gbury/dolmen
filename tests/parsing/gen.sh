#!/bin/bash -e

shopt -s globstar nullglob extglob

process_dir () {

  rm -f dune

  echo "; File generated for tests by a script"               >> dune
  echo                                                        >> dune

  for f in *.{cnf,icnf,p,smt2,zf}; do

    echo -n "Generating rule for file $f"
    echo "; Test for $f"                                      >> dune
    echo "(rule"                                              >> dune
    echo "  (target  ${f%.*}.output)"                         >> dune
    echo "  (deps    $f)"                                     >> dune
    echo "  (package dolmen_bin)"                             >> dune
    echo "  (action (chdir %{workspace_root} (with-outputs-to %{target} (run dolmen --type=false %{deps}))))" >> dune
    echo ")"                                                  >> dune
    echo "(rule"                                              >> dune
    echo "  (alias runtest)"                                  >> dune
    echo "  (action (diff ${f%.*}.output ${f%.*}.expected))"  >> dune
    echo ")"                                                  >> dune
    echo ""                                                   >> dune

    if [ ! -f "${f%.*}.expected" ]; then
      echo " + empty expect file"
      touch "${f%.*}.expected";
    else
      echo ""
    fi
  done

}

process_dirs () {
  # Generate dune file for the local directory
  process_dir

  # Recurse on subdirs
  for d in */; do
    cd "$d"
    process_dirs
    cd ..
  done
}

process_dirs

