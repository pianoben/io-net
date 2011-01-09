module Ast

type Tree =
  | Expressions of Expression list

and Expression =
  | Expression of Message list

and Message =
  | Message of Symbol * Expression list

and Symbol =
  | Identifier of string
  | Number of float
  | Operator of string
  | Quote of string