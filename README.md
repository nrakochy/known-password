# secured

A static trie builder/searcher of known-passwords 

## Usage
The API is exposed in secured.core, with two methods -- `compile-password-tries` and `check-password`. See `(source method-name)`
from the REPL for usage instructions.

Tests can be run with `lein test`

## TODO 
* Revise the in-memory trie-builder 
* Make `resources/passwords-data` path ready for uberjar-ing 
* Rename from secured to known-password

## License

Copyright © 2016 Nick Rakochy 

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
