# Classes

Classes can only have one Constructor
Class properties are always public

Example of an Impala Class

```js
class Console {
  make x;
  make y;

  //$ Constructor
  make Console(x, y) {
    //$ Param names if matching property names will set the property name with that value passed
    //$ so the first arg 'x' will also get set into the 'this.x' variable or the property
    //$ the x local var will be the same as the property x of the class
    //$ very confusing ik but I for some reason can't explain it right

    this.new = "test"; //$ using this. for defining new properties
  };

  //$ A regular function
  make log() {
    Impala.stdout.write(x);
    Impala.stdout.write(y);
    Impala.stdout.flush();
  }
};
```