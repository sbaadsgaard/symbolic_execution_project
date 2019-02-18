name := "SymbolicExecution"

version := "0.1"

scalaVersion := "2.12.8"

val libPath = "/home/baaden/bachelor_symbolic_execution/misc_ressources/z3/build"

javaOptions in run += s"-Djava.library.path=.:$libPath"

fork := true
