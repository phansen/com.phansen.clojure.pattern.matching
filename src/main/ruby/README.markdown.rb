require 'rdiscount'
require 'fileutils'

doc = <<-END 
# `com.phansen.clojure.pattern.matching`

A pattern matching library for Clojure inspired by OCaml and Haskell pattern matching. 

## Dependencies

This library depends [com.phansen.clojure.adt](https://github.com/phansen/com.phansen.clojure.adt) library.  

    <repository>
  	  <id>clojars.org</id>
  	  <url>http://clojars.org/repo</url>
	</repository>

	<dependency>
      <groupId>com.phansen</groupId>
  	  <artifactId>clojure.adt</artifactId>
  	  <version>1.0.0</version>
	</dependency>

## Usage

The main namespace for pattern matching is `com.phansen.clojure.pattern.matching.core`. 

    => (use 'com.phansen.clojure.pattern.matching.core)

## Wildcard bindings

Wildcards will match against anythings.  Specify a wildcard by using _.

    => (when-match 10 _ 
         (println "always matches"))
    
## Var Bindings

Var Bindings will match against anything and the matched value will bound to the variable.

    => (when-match 10 _a
         (println "matched " a))
         
This will always print "matched 10".

## Var Test

Var Tests will test that the matched value is equal to the variable.

    => (let [a 10]
         (when-match 10 a 
           (println "matched")))

## Value Test

Value Tests will test that the matched value equal to the value.

    => (when-match 10 10 
         (println "matched"))

This will print "matched".

## Exact Sequence Checks

Exact Sequence Checks is a binding for sequences that have exactly the same number of 
elements as there are binding forms.  The match will fail if this constraint is not met.

    => (let [b "c"]
         (when-match [1 2 "c"] [_ _a b]
           (println a)))
           
The exact sequence check binding `[_ _a b]` will always match the first element, always
match the second element and bind `a` to its value, and test that the third element is equal
to b.  In the example the match succeeds and `2` is printed

## Sequence Check

Sequence Checks will match against any sequence regardless of the number of elements in the 
sequence.  Sequence checks do this by adding a & `pattern` to match the rest of the sequence
against `pattern`.  

    => (let [b "c"]
         (when-match [1 2 "c" 3 4] [_ _a b & _d]
           (println d)))

The sequence check binding `[_ _a b & d]` will always match the first element, always
match the second element and bind `a` to its value, test that the third element is equal
to b, and bind d to the rest of the sequence.  In the example the match succeeds and `(3 4)` 
is printed.

## Map Check

Map checks match patterns against a map.  Map checks check a pattern against 
the value of a key.  Any keys specified in the `map-pattern` must exist in the map.  

    => (let [n 5]
         (when-match {:a 1 :b 2 :c "c" :d 5} {_ :a _b :b n :d}
           (println b)))

The map check binding `{_ :a _b :b n :d}` will check that the keys :a :b :d exist in the
matched map.  The binding always matches against `key :a`'s value, always matches against
`key :b`'s value and bind the variable to b, and tests that `key :d`'s value is equal to n.

## Algebriac Data Type Patterns

ADT patterns take on the form of the type constructors where the values of the pattern
will match against the values of the ADT.  An example is shown below

    => (let [tree (T (E) 1 (E))]
         (when-match tree (T _ _v (E)) 
           (println "v: " v)))
           
This will print "v: 1".  The pattern `(T _ _v (E))` will always match whatever is in the 
`left` field of tree, bind the value of the `value` field to v, and match the `right` field
only if it is an `E`.  Note that the patterns will be matched against the fields in the order the fields
were declared in `def-adt` or `extend-adt`. 

## Installation

You can get `clojure.pattern.matching-1.0.0.jar` from the [Clojars repository](http://clojars.org/com.phansen/clojure.adt).
Using maven use the following elements

	<repository>
  	  <id>clojars.org</id>
  	  <url>http://clojars.org/repo</url>
	</repository>

	<dependency>
      <groupId>com.phansen</groupId>
  	  <artifactId>clojure.adt</artifactId>
  	  <version>1.0.0</version>
	</dependency>

## Development



## Thanks

This project was inspired by Haskell and OCaml and the various other Clojure
pattern matching libraries.  

## License

Released under the MIT License: <http://www.opensource.org/licenses/mit-license.php>

END

html = RDiscount.new(doc).to_html()

markupFile = File.new("../markdown/README.markdown", "w")
markupFile.write(doc)
markupFile.close
puts "wrote markup to #{File.basename(markupFile.path)}"

readmeFile = File.new("../../../README.markdown", "w")
FileUtils.cp(markupFile, readmeFile)
puts "wrote markup to #{File.basename(readmeFile.path)}"

FileUtils.mkdir_p('../../../target/docs')

testFile = File.new("../../../target/docs/index.html", "w")
testFile.write(html)
testFile.close
puts "wrote html to #{File.basename(testFile.path)}"