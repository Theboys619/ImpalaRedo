# Import Syntax

```js
// tells Impala that you are importing code as an Impala file, 
import 'https://impala.co/factorial.imp' as 'imp'; // Currently not supported yet

// tells Impala you want to store contents as UTF-8
import text from 'https://link.com/text' as 'text'; // Currently not supported yet

// Imports all explicit exports from file into the exports variable as an impala import ("as 'imp'")
// the 'exports' can be any normal variable name
import exports from 'https://impala.co/std/http/http.imp'; // Supported

//then you can just log the text file
log(text);
```