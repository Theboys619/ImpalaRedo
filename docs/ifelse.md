# If - Else Statements

Two types of if statements

Block statements via curly braces '{', '}'
```js
if (5 == 5) {
	log(true)
} else if (4 == 6) {
	log('false');
} else {
	log('bad')
}
```

or ternary like version:

```js
make add(x oftype number, y oftype number) {
	send x + y;
}

make something() {
	send 'hi world';
}

make someone() {
	send 'gross';
}

//$ basically a ternary
make x = if 5 == add: something() else: someone();
//$ or
if 5 == add: something();
else: someone();
```