// Learn more about F# at http://fsharp.org

open System

(*Configuration for the game:
  HIDDEN: Determines which symbol is used for unknown letters
  CASE_SENSITIVE: Determines if the letters are case sensitive
  ALLOW_BLANKS: Determines whether the word(s) can contain spaces
  HELP: Determines whether its possible to get a letter revealed 
  MULTIPLE: Allows the user to enter multiple letters in one go. 
  WORDS: List of words
  WORDS_MULTIPLE: List of words containing spaces*)
module Configuration =
    let HIDDEN = '*'
    let CASE_SENSITIVE = true
    let ALLOW_BLANKS = true
    let HELP = false 
    let MULTIPLE = false //todo
    let WORDS = ["Successful";"Blush";"Claim";"Hang";"Property";"Structure";"Brown"]
    let WORDS_MULTIPLE = ["Id rather be a bird than a fish";
                            "I am never at home on Sundays";
                            "Rock music approaches at high velocity";
                            "The book is in front of the table";
                            "We need to rent a room for our party";
                            "Lets all be unique together until we realise we are all the same";]

//Number of incorrect guesses the user is allowed, before losing
let numberOfWrongGuesses = ["1";"2";"3";"4";"5";"6";"7";"8"]

//Checks the case sensitive value. If its false it changes the input to lower case
let checkCaseSensitivityChar (c:char) =
    if  Configuration.CASE_SENSITIVE = false 
    then let c' = c |> Char.ToLower
         c'
    else c

//Inputs:
//word(string): The word that needs to be guessed
//used(char sequence): The characters that have been guessed

//Returns the word: E.G ***e* j*g
let showGuessChars (word:string) (used:char seq) =
   let used' = Seq.append used [' '] // Adds "space" to the list of used characters, to allow the user to see them. E.G *** ***
   word |> String.map (fun c -> 
      if Seq.exists ((=) c) used' then c else Configuration.HIDDEN // Takes the word and convert it into stars (if the charaters havnt been guessed)
   )

//Inputs:
//used(char sequence): The characters that have been guessed
//guess(char) the users guess
let checkValidGuess (used:char seq) (guess:char) : bool =
      let validChars = ['A'..'Z'] @ ['a'..'z'] //List of valid characters
      Seq.contains guess validChars && //If the guess is in the validChars list and havn't been guessed before (Is in "used") then return true, else false
      not (used |> Seq.exists ((=) guess))

//Inputs:
//used(char sequence): The characters that have been guessed
//word(char sequence) the users guess
let getNextChar (used: char seq) word : char=
    let usedarray = used|> Seq.toArray // converts the guesses chars to array
    let usedset = set usedarray // build a set of the array
    let wordset = set word // build a set of the word

    let diff:char array = Set.difference wordset usedset |> Set.toArray //find the differences between the 2 arrays 

    let c = diff.[0] //takes the first element and return it
    c
    
    

//Gets the input form the user and validates it
//Inputs:
 //used(char sequence): a list of characters that have been guessed
 //word(char sequence):the word that needs to be guessed
let rec getUserGuess used word=
   let key = Console.ReadKey(true) // reads input
   if key.Key.Equals(ConsoleKey.Tab) then //if key is tab (HELP option) get a correct character
        let (nextchar: char) = getNextChar used word
        nextchar //return correct letter
   else let guess = key.KeyChar 
        let g = checkCaseSensitivityChar guess // make lower case if CASE_SENSITIVE is set to false
        if checkValidGuess used g then g //check if the input is valid
        else getUserGuess used word //if invalid call the method again

// make lower case if CASE_SENSITIVE is set to false
let checkCaseSensitivityWord (word:string) =  
    if  Configuration.CASE_SENSITIVE = false 
    then let w = word.ToLower()
         w
    else word

//Combined the standard word list (single words) with the multiple word list if the ALLOW_BLANKS is set to true
let checkAllowMultipleWords (singleWords:string list, multipleWords:string list) = 
    if Configuration.ALLOW_BLANKS = true 
    then 
        let combined = singleWords @ multipleWords //Concat the 2 lists
        combined
    else singleWords
 
 //The main "loop" of the game, a recursion function.
 //Inputs: 
 //word(string):the word that needs to be guessed
 //used(char sequence): a list of characters that have been guessed
 //wrongGuesses(int): how many wrong guesses the user have made
let rec play word used wrongGuesses = 
   printfn "%s" numberOfWrongGuesses.[wrongGuesses] //Prints number of wrong guesses
   let word' = showGuessChars word used // Gets the word with stars and guessed chars E.G ***e* j*g
   printfn "%s" word'
   if word = word' then //If the word matches the player won the game
      printfn "You guessed the word!"
      Console.ReadKey(true).KeyChar
   elif wrongGuesses = numberOfWrongGuesses.Length-1 then //If the player have used all his guesses he loses
      printfn "You used too many guesses. Better luck next time!"
      Console.ReadKey(true).KeyChar
   else
      let guess = getUserGuess used word //Gets the user input
      let used = guess::used //Adds the guess to the list of used characters

     //if the guess exists in the word we allow the user for another round without adding +1 to the wrong guesses
      if word |>  String.exists ((=) guess) 
      then play word used wrongGuesses
      else play word used (wrongGuesses+1) // the guess was incorrect and therefore 1 is added
    
//Setting up the possible words based on the Configuration.
let wordlist = checkAllowMultipleWords (Configuration.WORDS, Configuration.WORDS_MULTIPLE) 
let word = wordlist.[Random().Next(wordlist.Length)] // pick a random word
let word2 = checkCaseSensitivityWord word //Makes lower case if the CASE_SENSITIVE setting is false
do play word2 [] 0 //Starting the game

