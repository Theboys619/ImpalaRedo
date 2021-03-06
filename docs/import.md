# Import Syntax

```js
// tells Impala that you are importing code as an Impala file, 
import 'https://impala.co/factorial.imp' as 'imp';

// tells Impala you want to store contents as UTF-8
import text from 'https://link.com/text' as 'text';

//then you can just log the text file
log(text);
```