# setting a value

    myNum = 3

    myNum: Int
    myNum = 3

    myNum = (3: Int) -- inline type signatures must be wrapped in parentheses

    myNum: (Integral a) => a
    myNum = 3

    myNum = (3: (Integral a) => a)


# function application

    myNum = 1 + 2
    mynum = 3 - 1

    mynum = -1
    mynum = 1.negate

    myNum = 2.pow(4)

defining a function "add-one" that takes a parameter "a" - no signature. Zoda will use type inference to give `a` the trait `Addable`

    a.add-one = a + 1
     
defining a function with a signature

    add-one: Int -> Int
    a.add-one = a + 1

    add: Int -> Int -> Int
    a.add3(b) = a + b

    add3: Int -> Int -> Int -> Int
    a.add(b, c) = a + b + c


# Function chaining

you can chain functions using `.`. this is useful for "pipelines"

    my-value = 3.f.g.h -- take 3, then apply f, then apply g, then apply h
    my-value = 3.add(4).subtract(1).times(4).negate.divide(2).absolute-value


# Currying

if you want to, you can curry a function by leaving a `-` in the place of an argument.

    i.under3 = i.(3.divide(_))

you can also apply none of the arguments of a function just by not doing anything with it at all

    my-val = my-list.map(add-one)


# Style tips

with the x.f(z) syntax, you probably want to make function names read nicely

    3.max(4)        -- bad
    3.lower-bound(4) -- good


# Indentation-sensitive Syntax

any time after a `:`, you should indent if you choose to make a newline


# if expressions

these use indentation for alignment to support nesting

    a.lower-bound(b) = 
        if a > b then:
            a
        else: 
            b

these are expressions so you may use them however you like. 

    a.lower-bound-times(b, x) = 
        x * if a > b then:
            a
        else: 
            b

for any token that ends in a `:`, followed immediately by another bit of syntax that ends with a `:`, the first can and should be left off. 

    my-val = do:
        -- blah blah
    my-val = if 2==2 then:
                True
            else if 3==3 then:
                True
            else do:
                pure False


# Lists

    my-list = [1,2,3,4]
    my-list = 1 ::> [2,3,4]   -- [1,2,3,4]

These aren't lists specifically, as they can be overloaded so `[1,2,3]` can represent a list, a vector, an array, etc.
Also available are Iterators, which are like lists but lazy and useful for iterating over a range of values.

    my-iterator = [1..10]
    my-iterator = [10,20..100]

Since they're lazy, they're useful for functions like the following:

    n.factorial = [1..n].product

If iterators were not lazy, this would require a O(n) space. Since they are, it requires O(1) space. 
list slicing is supported and similar to python's

    [1,2,3,4].getIndex(0) == Just 1

    [1,2,3,4,5].slice(1, 4) == Just [2,3,4]

    [1,2,3,4,5].drop(2) == Just [3,4,5]

    [1,2,3,4,5].take(2) == Just [1, 2]



List slicing doesn't just work with lists, it works with anything that has the `Slicable` trait
This includes Iterators, Vectors, Strings, etc.
We have list comprehensions too. Of course, they're not specific to lists. They can actually be used with any Monad, and has an additional `then` and `then by` feature 
not present in standard haskell.

    myList = [ (x, y) | x <- xs,
                         y <- ys,
                         then reverse,
                         then sortWith by (x + y) ]



# higher order functions

    x.add-one = x + 1
    my-list = [1,2,3,4,5].map(add-one)
    my-list = [1,2,3,4,5].map(_.add(1))



# lambdas

    x.add-one = x.(|x| ->: x + 1)

is equivalent to...

    x.add-one = x + 1

In some languages you can set a lambda to a value directly, but this is not allowed in Zoda


# The value-passing operator

not really a language feature, just a built-in operator. simply passes a value to a function, left associative

    my-val = add-one <| 3
    my-val = _.add-one <| 3
    my-val = 3.add(_) <| 1
    my-val = 4


# do notation

    a.print-value-cool = do:
        putString("=============")
        print(a)
        putString("=============")

desugars to...

    a.print-value-cool =
            putString("=============") >>= \- ->
            print(a)                 >>= \- ->
            putString("=============") >>= \- ->


# where statements. 

valid for functions as well as values. 

    my-val = 
        i + j
        where:
            i = 3
            j = 4


#inline test statements. 
valid for functions as well as values, but there's not really a good reason to put them on a value

these can be run (and are automatically run when creating an optimized build), and only rerun if the function or its dependencies change.
    a.add-one = a + 1
        test:
            "can add one to two".test(
                (2.add-one).should-be(3)
            )


the "test" section is just a single value - to contain multiple `should-be`s in one test, use do notation.
    a.add-one = a + 1
        test:
            "can add one to positive values".test(_) <| do:
                (2.add-one).should-be(3)
                (3.add-one).should-be(4)

and to contain multiple tests, use do notation again
    a.add-one = a + 1
        test do:
            "can add one to positive values".test(_) <| do:
                (2.add-one).should-be(3)
                (3.add-one).should-be(4)
            "can add one to negative values".test(_) <| do:
                (-2.add-one).should-be(-1)
                (-3.add-one).should-be(-2)

if you know you haven't implemented the functionality for a test yet, just make it pending
    a.incrementAbsoluteValue = a + 1
        test do:
            "can increment to positive values".test(_) <| do:
                2.add-one.should-be(3)
            "can increment to positive values".test(_) <| do:
                -2.add-one.should-be(-3).pending


# match

    a.luckyNumber7 = match a:
        7 ->: putString("Lucky number 7!")
        _ ->: putString("sorry, you are a loser")
    l.head = match l:
        x::>_ ->: x.Just
        _    ->: nothing 

all pattern matches must be exhaustive to perform an optimized build - optimized Zoda builds have no runtime errors 


# inline docs 

Seperated into "short-doc" and "long-doc". The short-doc should be very short, and has access to the actual values that are passed to the function (in this case, x and y). 
The short-doc is intended to be shown when the cursor is over the function. So if you hover the cursor over `2.add(3)`, you would see `Adds 2 and 3 -> 5`.
The long-doc can be as long as you want. It is intended to be shown on documentation pages and should give an in-depth explanation with examples of how it should be used.
These examples are also tested at compile time. 
Comments with `--` are allowed inside short-docs and long-docs, and will not be included in the output.
    x.add(y) = 
        sum
        where:
            sum = x + y

        test do:
            "can add positive values".test(_) <| do:
                2.add(2).should-be(4)
                3.add(5).should-be(8)

        short-doc: Adds `x` and `y`  
        long-doc:
            Essentially just a wrapper for the `+` operator.
            It works on any values that have the an `Addable` instance, so it can be used anywhere you'd like to add two values together
            
            >> 3.add(5)
            -> 8

            Negative numbers can be added as well.   -- do we need to say this? this seems kind of obvious.

            >> 3.add(-5)
            -> -2

            One use might be to compute a new bank account balance after some money is deposited:

            >> oldBalance = 300
            >> amountToAdd = 10
            >> newBalance = oldBalance.add(amountToAdd)
            >> newBalance
            -> 310

            Although since this function is just a wrapper for `+` there's little reason to use it unless you need currying. 

            >> [1,2,3].map(_.add(3)) 
            -> [4,5,6]

            Since Tuples do not have an `Addable` instance, they cannot be added with this function

            >>error (3,4).add((5,6))

Supported editors will have the long-doc collapsed by default, because otherwise it takes quite a bit of space which will probably not always be that helpful. 
The long-doc requires two newlines for a linebreak to be present in the output, except in code examples. This makes it possible to set the max line width to whatever 
you want within the long-doc and the formatter will adjust it for you.


# Strings and Chars

We use the same approach to handling Strings that Rust does. To borrow from their documentation:
A String is a sequence of Unicode scalar values encoded as a stream of UTF-8 bytes. 
All Strings are guaranteed to be a valid encoding of UTF-8 sequences. 
Additionally, unlike some languages, Strings are not null-terminated and can contain null bytes. 
Strings are allowed to span multiple lines, and are indentation-sensitive 
They can also contain inline expressions with { expr } and {s expr }. 

    my-val = 3
    my-string = "My value is: { my-val }" -- my-val.repr is automatically called. x.repr is a function that expresses a value as a String, in a way that should be developer-friendly.
    my-string = "3 + 3 is { 3 + 3 }"
    my-string = "My name is {s "E. Cummings".toLowercase }" -- with `{s expr }`, .repr is not automatically called. For this reason the expression is expected to return a String.
    my-string = "Line 1 
                 Line 2" -- equivalent to "Line 1\nLine2"
    my-string = "Line 1\ 
                 Line 2" -- equivalent to "Line 1Line2"

String literals are escaped with a backslash. it's acceptable to use "\n", "\t", "\""and "\\". any other backslashes are an error. 

    my-string = "\"What is it?\" she asked.\n"

for use with regular expression libraries, prefix the string with 'r'. This disables `{}`, 
    
    my-regex = r""

# recursion

Zoda does not allow unrestricted recursion. The reason is that unrestricted recursion makes it easy to create a stack explosion:

    x.fib = match x: 
        0 ->: 1 
        1 ->: 1 
        x ->: (x - 1).fib + (x - 2).fib

This is bad because `100000.fib` will result in a stack overflow, which is a form of runtime exception. Since Zoda attempts to make runtime exceptions very rare,
we need a way to prevent this. There is a way to do recursion without running the risk of a stack overflow - if the recursive call is returned immediately,
its very simple for the compiler to unroll it into a loop which will not result in unbounded growth of the stack. 

    x.fib = internalFib x 0 1 
        where counter.internalFib(y,z) = match counter:
            0       ->: z
            counter ->: (counter - 1).internalFib(z, y+z)

so, we can prevent unbounded growth of the stack by disallowing all recursion that is not tail recursion. But, this is annoying because it means recursive calls are 
"special". I think this would be easier to teach to people if we used a special keyword for tail calls. I'm thinking `self`.

    x.fib = internalFib x 0 1 
        where counter.internalFib(y,z) = match counter:
            0       ->: z
            counter ->: (counter - 1).self(z, y+z)

so `self` is a special function that calls the function it's used in with the same parameters, and the compiler will not let you do anything to a the output of 
the `self` function. The self function is the only situation where recursion is possible - all other functions form a DAG of their dependencies. 
The one exception to the "you can't do anything to the output of the `self` function" rule is functions with the trait `Chainable`.
This doesn't blow up the stack because the `Chainable` trait includes instructions on how to compute an intermediate value. `+`, `*` and `::>` are examples of Chainable.

    x.factorial = x * (x-1).self
    l.filter(f) = match l:
        []      ->: []
        x ::> xs ->: 
            if (f x) then: 
                x ::> xs.self(f)
            else:
                xs.self(f)
                       
# info statements

Functions and values can have certain facts stated about them in `info` statements which serve to provide extra information to the compiler.
The only one I have any ideas for is `always-terminates`, which says about a function exactly what it says on the tin.

    x.addOne = x + 1 
        info: always-terminates

Although I expect `always-terminates` to not be that useful... if a function doesn't call `recurse` and only calls functions marked as 
`always-terminates`, then I think it should always terminate.
For higher-order functions, `always-terminates` is not available but `always-terminates-if-input-functions-terminate` is.  


# unwrap

the Zoda compiler will do some tricks to attempt to deduce information about the 
values passed to various functions and how that relates to their output.
For example:

    divided-by: Float -> Float -> Optional<Float>
    numerator.divided-by(denominator) = match denominator:
        0 ->: Nothing
        denominator ->: (numerator / denominator).Just

Now, we might want to write a `divided-by-3` function, and reuse `divided-by`. But we have an issue! `divided-by-3` will always work, but 
if we just plug it into `divided-by`, our function will end up returning `Optional<Float>`, instead of `Float`
To solve this, we add a new magical function - unwrap. 

    x.divided-by-3 = (x.divided-by(3)).unwrap

unwrap converts a value from a `Optional<a>` (actualy, any value with the `Unwrappable` trait) to an `a` *if* the compiler can prove that it will 
always actually have a corresponding value. In this case, it can see the call to `divided-by` will only return `Nothing` if the `denominator`
is `0`. Since we know at compile time the denominator is `3`, the `unwrap` function can automatically unwrap the `Just`. 



# type-wrapper 

Equivalent to "newtype" in haskell

    type-wrapper Year = Year Int
    type-wrapper Name = Name String


# type declarations and ADTs

Used for creating a new ADT 

    type Bool = True | False
    type CarBrand = GM | Ford | Toyota
    type Car = CarBrand.Car(Year)
    type Person = Name.Person(List Car)
    type IntList = EmptyIntList | Int.Cons(IntList) -- recursive data types are allowed, but only one level deep. This restriction also applies to functions, as we'll discuss later

parameterized types
    type Optional<a> = Nothing | Just a 
    type Result<e, r> = e.Error | r.Result 

# Aliases

    alias FilePath = String
    alias Either<a><b> = Result<a><b>


# trait and instance declarations

Unlike in default haskell, type classes can have multiple parameters. In this case, we're relating two types by saying one is a collection
and another is an element. We also say that `Eq` is a supertrait for `e`, so the elements of any collection are required to have the `Eq` trait.
We probably wouldn't do that in real life though because it's useful sometimes to have collections of functions and it's not possible to give
functions an `Eq` instance. Also, we've added a functional dependency `c -> e`, by saying that the type of the element is soley determined by the type
of the collection. This just means that for any `Collection c => c`, there should be a unique type `e` known at compile time.  

    new-trait Eq e => Collection c e | c -> e where
        insert: c -> e -> c
        member: c -> e -> Bool

    has-trait Eq a => Collection (List a) a where
        collection.insert(element) = element.Cons(collection)
        collection.member(element) = match collection:
            EmptyList  ->: False
            x.Cons(xs) ->:
                if x == element then:
                    True 
                else: 
                    xs.recurse(element)

We can also write that a specific class does not have a certain trait 

    hasnt-trait Serializable (a -> b) where
        -- "general" shows up when someone trys to pass a function assuming it has the trait "Serializable"
        general = "Functions do not have the Serializable trait because they don't have a consistent representation in memory."
        -- "hint" shows up every time the typeclass is used improperly. It is intended for giving general information about a topic. 
        hint = "Functions can't be serialized because by the time the program actually runs, the notion of a function doesn't really exist anymore, \
               so it makes no sense to serialize it.

               For more info, see https://example.com."
        -- this error message shows up when someone attempts to pass the a function to `serialize` directly.
        serialize = "You are attempting to serialize a function, but functions cannot be serialized"

# Rows 

A row of types represents an unordered collection of named types, with duplicates. 
Duplicate labels have their types collected together in order, as if in a NonEmptyList. 
This means that, conceptually, a row can be thought of as a type-level Map<Label, NonEmptyList<Type>>.
Rows are not of kind Type: they have kind Row<k> for some kind k, and so rows cannot exist as a value. 
Rather, rows can be used in type signatures to define record types or other type where labelled, 
unordered types are useful.
example of a row: ( name :: String, age :: Int )
example of a row with a type constraint: (Num n) => ( name :: String, age :: n )
example of an "open row": ( name :: String, age :: Int | l )
rows are allowed to have duplicates
for convenience, you can give a row an alias 

    type Sel = ( value :: Int, values :: [Int] )
    type Sel<l> = ( value :: Int, values :: [Int] | l )

You won't be using Rows very often in standard Zoda, they're mostly useful for library authors to play with interesting features


# Records 

A record is like a type-safe dictionary, implemented internally using Rows. 

    author: { name: String, interests: List String }
    author =
        { name: "Phil"
        , interests: ["Functional Programming", "JavaScript"]
        }

Records have no inherent order
    author: { name: String, interests: List String }
    author =
        { interests: ["Functional Programming", "JavaScript"]
        , name: "Phil"
        }

You can write a function that takes a record, if you want

    getName: { name: String | l } -> String 

The "| l" is a type variable. This means it can take any record as an input, as long as it has a record with the type `name :: String`.
You can retrieve values from a record . 

    r.name

You can fill in a record with another with special syntax 

    person: { name: String, postcode: Int }
    person = {name: "Bob", postcode: 11111} 
    person2 = person{name: "Steve", ...person}
    dog: { name: String, person: {name: String, age: Int} }
    dog = {name: "Fido", owner: {name: "Bob", age: 33}}
    dog2 = dog{owner: {name: "Mike", ...dog.owner}, ...dog} 

example of a function that modifies the name field of a record

    addJrSuffix :: forall r. { name :: String | r } -> { name :: String | r }
    addJrSuffix hasName = hasName { name = hasName.name <> ", Jr." }
    addJrSuffix { name: "Bob" }
    addJrSuffix { age: 42, name: "Gerald" }

You can give a name to a record type with `type`

    alias Book = { name: String, text: String }
    alias Author = { name: String, books: List Book }

You can unwrap a record into another when defining them

    alias Name = { name: String }
    alias Country = { population: Int | Name }
    myCountry = {name: "usa", population: 2000}

Curried values

    ["usa", "canada"].map({name: _, population: 2000})

like how `a` is a variable that can take any type, `{ | r }` is a record with any (or no) values

    myFunc: { r } -> { r }
    r.myFunc = r -- the only possible implementation of a function with this type is the identity function.

Unlike in rows, duplicate keys in records are not allowed.


# Corecords

Records:Corecords ~ Product types:Sum types

While a record must have every value in its type, a corecord can only have one

    a: {' b: Int, c: String '}
    a = {' b: 3 '}

    {' name: String | v '}

A useful function exists for rows, `on`. `on` takes a function to handle a possible value, and a function to handle the rest if that value is not present.

    fooToString: {' foo :: Int | v '} -> String
    foo.fooToString = foo.on<foo>(show, |_| -> "not foo") -- haven't decided on the specifics of the `<>` syntax for functions yet

