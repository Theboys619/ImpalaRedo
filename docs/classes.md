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
    this.x = x; //$ have to use 'this' if param names are also the properties' name
    this.y = y;
  };

  make log() {
    Impala.stdout.write(x);
    Impala.stdout.write(y);
  }
};
```