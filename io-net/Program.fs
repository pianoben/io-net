// Learn more about F# at http://fsharp.net

open System
open Microsoft.FSharp.Text.Lexing

open IoLex
open IoParse

let rec msort l =
  let halve l =
    let rec halve_acc l rest count =
      match count with
      | 0 -> (List.rev l, rest)
      | _ -> halve_acc (List.head rest :: l) (List.tail rest) (count - 1)

    let len = List.length l
    halve_acc [] l (len / 2)

  let merge l r =
    let rec merge_acc l r res =
      if List.isEmpty l && List.isEmpty r then res

      if not List.isEmpty l && not List.isEmpty r then
        let hl = List.head l
        let hr = List.head r

        if hl < hr then
          merge_acc (List.tail l) r (hl :: res)
        else
          merge_acc l (List.tail) r (hr :: res)
      elif not List.isEmpty l then
        let h::rest = l
        merge_acc rest [] (l :: res)
      else
        let h :: rest = r
        merge_acc rest [] (r :: res)

    merge_acc l r []

  let (left, right) = halve l
  let mleft = msort mleft
  let mright = msort mright

  merge mleft mright


let matcher url =
  (new System.Text.RegularExpressions.Regex("")).Match url

type Util =
  static member lexeme (lexbuf: LexBuffer<byte>) = lexbuf.Lexeme |> Text.Encoding.UTF8.GetString
  static member lexeme (lexbuf: LexBuffer<char>) = new System.String(lexbuf.Lexeme)

let parseHex (str: string) =
  let n = str.Substring 2
  float <| Int32.Parse(n, Globalization.NumberStyles.HexNumber)

do
  let test = "Test := Object clone(\"test\", 0x123)"
  let lexbuf = LexBuffer<char>.FromString test
  
  let tokens = seq { while true do yield IoLex.token lexbuf }
               |> Seq.takeWhile (fun t -> t <> EOF)
               |> Seq.append
               <| seq { yield EOF }

  let tree = IoParse.start (IoLex.token) (LexBuffer<char>.FromString test)

  try
    tokens
      |> Seq.iter (fun t -> printf "%A " t)
  with
    ex -> printf "Error: %A" ex

  printf "\n"