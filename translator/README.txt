Solution for Assignment 4: Translator

***This translator is provided by Cormac Tighe. Thanks for this excellent work.***

The translator can be excuted as: ./translator.rb argument1 [argument..]. Here is the description of the arguments.
My program takes up to 3 arguments. Only the first is required.
The first argument, as the project specifies, is the input file.
The second argument, which is optional, is the output file. The output file will be overwritten. If not specified, or if the output file is "-", the output will be printed to stdout.
The third argument is the same as in my parser project, either "print" for a pretty-printed parse tree, or "debug" for a frankly obscene amount of information.
These will always be printed to stdout, regardless of whether or not an output file is specified.
To specify print or debug information without needing to write the output code to a file, specify a hyphen "-" as the output file. This will cause the code to be printed to stdout.

I used my parser from the previous project, rather than the provided parser. I had to heavily modify it for this purpose.
Because of the short amount of time I had to do this project, I couldn't rework the grammar to fix the empty-data-decls problem I mentioned in the previous project.
I still have to pretend in one instance that I have infinite lookahead.

I implemented arrays by using what amounts to base and offset notation in the global and local arrays.
To do this, I stored the location of the base of an array. To get the "address", I then add to the base an offset in the form of a local "register,"
be it a named variable in the original program or a "register" that holds a constant. This is why, when translating programs with arrays, any array access looks something like the following:
local[<a>] = <b> + local[<c>];
global[ local[<a>] ] = ...
Where <a> is the a number of a previously unused "register", <b> is the first "register" in the contiguous section of the global/local array that belongs to the array in question,
and <c> is the index in the array in question that the original code accesses. For a more complete example of this, see the translation of fibonacci.c or of the included test file, zilch.c.

Another point I'd like to bring up is control flow. I used three styles; one for while loops, one for if/else statements, and one for if statements without an else.
For all of these, rather than using the original code's direct conditional expression, I evaluate the conditional beforehand and use a comparison of my own.
In this way, I was able to cut down the complexity of the code and allow for the 2 different if-statement styles.
I don't know exactly why I decided to do it this way. It has been several years since I have written any assembly, but this is how I would do it in assembly, so I guess I remember more than I thought.
All of my labels had a number in their name, with each one incrementing the number for the next control flow group. In this way, I avoided erroneous jumps and allowed for continues and breaks.
To allow for continue and break statements (which the project specification doesn't mention but I thought were rather important) I had to modify my
first plan to allow passing around the number of the label a block of statements would break or continue, and implemented breaks as "goto LABEL<n>OUT;" and continues as "goto LABEL<n>START;".
The three styles of control flow are as follows:
while loop:
	LABEL<n>START:
	local[<x>] = <conditional>;
	if(local[<x>] == 0) goto LABEL<n>OUT;
	<loop contents>;
	goto LABEL<n>START;
	LABEL<n>OUT:
	<rest of program>;
if:
	local[<x>] = <conditional>;
	if(local[<x>] == 0) goto LABEL<n>OUT;
	<if contents>;
	LABEL<n>OUT:
	<rest of program>
if/else:
	local[<x>] = <conditional>;
	if(local[<x>] != 0) goto LABEL<n>THEN;
	<else contents>;
	goto LABEL<n>OUT;
	LABEL<n>THEN:
	<if contents>;
	LABEL<n>OUT:
	<rest of program>;

My program ends up "allocating" more "registers" than it strictly needs.
It will store the contents of an operation in one "register," set a new "register" equal to the first, and use the second to assign some other "register." It also doesn't search for repeated constants.
While I first thought this to be rather wasteful, upon spending some time trying to figure out a way to be more conservative,
I realized that to do so would require making the translator vastly more complicated than it already is.

Ultimately, I had fun with this project. It was an interesting challenge. I have considered making my own transcompiler to make a new language, and having this experience, I may finally take a stab at it.
