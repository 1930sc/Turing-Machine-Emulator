# Turing Machine Haskell Emulator

**This project it's separated in 3 parts:**

*   Emulator itself (Main.hs + MachineFunctions.hs).
*   Read file module (FileReading.hs).
*   .tm File extension.

The first one is a program that essentially takes a Turing Machine and an
initial word and returns all the different tapes in sequential order, but it's
possible to ask just for the last one.

**Here is an example of how it works with the ``` example.tm``` file:**
```
$ stack setup
$ stack build
$ stack exec turing-machine-emulator-exe

 Turing Machine Path:
 Machines/example.tm

 Initial Tape:
 0011

 Visible length of the Tape:
 10

 Just final tape[0], or every step[1]?:
 1
 .....[0]011..
 .....[0]11...
 ....0[1]1....
 ...01[1].....
 ..011[.].....
 ...01[1].....
 ....0[1].....
 .....[0]1....
 .....[.]01...
 .....[0]1....
 .....[1].....
 ....1[.].....
 .....[1].....
 .....[.].....
 .....[.].....
 .....[.].....
 .....[.].....

 N° of steps : 17
```

The second part has only one objective, and it's to read a ```.tm``` file and
transforms the conteined information, in to a useful data.

And lastly but not leastly, the ```.tm``` file extension: This extension doesn't
have any property by itself, it's just a nice way to indicate that it's a file
that represent a Turing machine in a way that this emulator can read. In fact
you can give to the emulator a ```.txt``` file, and it will perfectly work if
it's correctly written.  To understand how exactly a Turing Machine must be
represented in this type of files, read the explanation below.

## .tm File extension explained:

**To understand this easier, you might want to have a new window with
the ```exmple.tm``` file open**

**The first thing that you have to know is that every line that starts with
```;;``` is a commentary. Other than that, there are just three aspects to talk
about**
-   Initial State.
-   Blank Symbol.
-   Rules.

**A Turing Machine it's usually defined as a 7-tuple, where the elements are:**
 1. A finite non-empty set of States.
 2. A finite non-empty set of alphabet symbols.
 3. A symbol that represents a blank slot of the Tape.
 4. A set of symbols allowed to appear in the initial Tape.
 5. The Rules, usually defined as a function.
 6. The Initial State.
 7. A set of accepting final or terminating states.

See:[Wikipedia Turing Machine formal-definition](https://en.wikipedia.org/wiki/Turing_machine#Formal_definition)
for more information.

1.  **In this program the set of states**, it's given by the states that appear
    in the Rules.

2.  **The alphabet**, it's given by the characters that are used on the rules,
    or if you want, the characters allowed by the program or the terminal in
    use.

3.  **The blank symbol** has to be explicitly specified on the file, and it's
    done like this: ``` Blank = <YOUR SYMBOL>``` (without '<' and '>') in a new
    line before the rules. In the case of ``` example.tm ``` , the blank symbol
    would be a ```.``` (a dot).

4.  **The set of allowed symbols** (characters) that may appear in the initial
    tape, are just the characters allowed by the program or the Terminal.

5.  **To define the rules** of a Turing Machine you must write ```Rules = ```,
    and after that, you write every rule you need. But a rule itself has a
    syntax too.   Every rule will be a 5 elements tuple, where the elements are
    separated by spaces.

    Example :  The rule ``` (A 1 0 -> B)``` mean, that if the machine it's in the
    state ```A``` and sees a ```1``` then, it replace it by ```0```, moves to
    the ```->``` (right), and changes to state ```B```.  Where the first element
    is a Haskell-String, the second and the third are characters, the forth can
    be either ``` -> ``` (Right), ```<-``` (Left) or ```><```(Stay), and the
    fifth element is again a Haskell-String, in the sense that they can be
    larger then 1 character, because there are names for states, not symbols.

6.  **The Initial State** has to be specified, and it's done like the blank
    symbol. The example ```Initial = A``` is a definition of a Initial state,
    just as it's used in the ```example.tm ``` file.

7.  **The accepting final states** aren't specified in this emulator, we just
    assume that the machine will end when there is no rule that can be applied,
    on the current state with the current tape.
