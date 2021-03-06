# Function Syntax

Anytime there is a comment ignore the // because comments in Impala are '$'
The '//' are just to make it green.

```js

//$ z is an implicit type of number because of default parameter
make funStuff(x oftype number, y oftype number, z = 5) {
	//$ equivalent of "return"
	send x + y + z;
	//$ below wont work
	log(x - y);
}

//$ adding a return type
make coolStuff() oftype number {
	send 5;
}

```

# Function Call Syntax

```js
log(funStuff('5', 4));
//$ returns 14

//$ skip first two parameters (automagically filled in with nothing / null)
log(funStuff(,,5));

//$ assign function call to variable (whatever the function returns)
make returnVal = funStuff(,,6);
log(returnVal);

//$ assign function to variable
make funcvar oftype function;
funcvar = funStuff;

//$ or
make funcvar = funStuff;

make variable = funcvar(,,6);

log(variable);
```