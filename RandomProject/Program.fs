// Learn more about F# at http://fsharp.org

open System

module Configuration =
    let HIDDEN = '*'
    let CASE_SENSITIVE = true
    let ALLOW_BLANKS = true
    let HELP = false //todo
    let MULTIPLE = false //todo
    let WORDS = ["En";"Masse";"Svaere"]
    let WORDS_MULTIPLE = ["En grim ko";"Seje solbriller";"Hunden tisser i skoven";"Cocio og matilde haand i haand lang saetning";"Hej med dig"]


let hangman = ["1";"2";"3";"4";"5"]

let checkCaseSensitivityChar (c:char) =
    if  Configuration.CASE_SENSITIVE = false 
    then let c' = c |> Char.ToLower
         c'
    else c

let toPartialWord (word:string) (used:char seq) =
   let used' = Seq.append used [' ']
   word |> String.map (fun c -> 
      if Seq.exists ((=) c) used' then c else Configuration.HIDDEN
   )

let isGuessValid (used:char seq) (guess:char) : bool= //  
      let validChars = ['A'..'Z'] @ ['a'..'z']
      Seq.contains guess validChars &&
      not (used |> Seq.exists ((=) guess))

let rec readGuess used =
   let guess = Console.ReadKey(true).KeyChar
   let g = checkCaseSensitivityChar guess
   if isGuessValid used g then g
   else readGuess used

let getGuess used =
   printfn "Guess: "
   let guess = readGuess used
   printfn "%c" guess
   guess

let checkCaseSensitivityWord (word:string) =
    if  Configuration.CASE_SENSITIVE = false 
    then let w = word.ToLower()
         w
    else word


let checkAllowMultipleWords (singleWords:string list, multipleWords:string list) = 
    if Configuration.ALLOW_BLANKS = true 
    then 
        let combined = singleWords @ multipleWords
        combined
    else singleWords
  
let rec play word used tally =
   printfn "%s" hangman.[tally]
   let word' = toPartialWord word used
   printfn "%s" word'
   if word = word' then 
      printfn "CORRECT"
      Console.ReadKey(true).KeyChar
   elif tally = hangman.Length-1 then 
      printfn "HANGMAN"
      Console.ReadKey(true).KeyChar
   else
      let guess = getGuess used
      let used = guess::used

     
      if word |>  String.exists ((=) guess)
      then play word used tally
      else play word used (tally+1)
    

let wordlist = checkAllowMultipleWords (Configuration.WORDS, Configuration.WORDS_MULTIPLE)
let word = wordlist.[Random().Next(wordlist.Length)]
let word2 = checkCaseSensitivityWord word
do play word2 [] 0



[<EntryPoint>]
let main argv =

    printfn "Hello World from F#!"
    0 // return an integer exit code
