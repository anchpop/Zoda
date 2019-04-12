Zoda's goal is to become the best language for the development of games and game engines.

Towards this goal, we have some subgoals.

1) Beginner friendliness. Every feature is evaluated against how easy it is for beginners to understand, and simplicity is an important goal of the language. Zoda is not a research language like Haskell. Documentation is deeply integrated into the language. Research into a visual representation of code is being looked into. High-quality errors are also a priority for Zoda. This is the reason that Zoda is so opinionated, more opinionated than almost any mainstream programming language. Beginers wonder "should I do it this way, or that way?" It is a goal of Zoda that nobody asks this question, and when they ask it there is always a good answer.

2) Good tooling. The syntax of Zoda is designed to be conducive towards IDE autocompletion and informative errors.

3) Efficiency. Zoda lacks features that make it easy to write inefficient code. Code written in Zoda will be efficient without effort, automatically use multiple cores, and even automatically offload some work onto the GPU.

4) Enabling fast prototyping. You will be able to compile Zoda code in two modes, "prototyping" and "production". Prototyping is when you want to run your code quickly and not worry about catching bugs (yet), Production is when you want to your code to run the most efficiently possible and want to be confident that it is bug-free. Code with warnings does not compile under Production, and code must compile under Production to be included in the standard repository of Zoda code.

5) Steering users towards writing bug-free code. At it's core, `Zoda` is a functional programming language. With features like a powerful type system, algebraic effects, checked exceptions, inline tests, and more, it is possible to be confident in the quality of your code. In practice, production Zoda code does not crash at runtime (when it does, it's due to unavoidable issues such as out-of-memory errors). When you do have a bug, Zoda will have a time-travelling debugger and multiple useful features for debugging.

6) Self-embedding. Zoda code should be able to read and run other Zoda code at runtime, and run it just as efficiently as if it had been written in the original executable.

7) Cross platform. Compiling to LLVM should make writing cross-platform code easier

## Setting a value

An `=` is always followed by a value, it's how you set a value. A ` ~ ` is always followed by a type, it's how you tell Zoda the type of a value.

Here we're defining the same value multiple times, this is illegal but we're just doing it for demonstrative purposes.

    myNum = 3

    myNum ~ Int
    myNum = 3

    myNum = (3 ~ Int) -- inline type signatures must be wrapped in parentheses

Types can be ambiguous with "traits" - here we say "myNum" has the trait "Integral", but we're not any more specific.

    myNum ~ Integral a => a
    myNum = 3

    myNum = 3 ~ (Integral a => a)


## Function Application

Operators always take two values. the format for applying them is `value1 SPACE operator SPACE value2` 

    myNum = 1 + 2
    mynum = 3 - 1

Negation is the only expection - just pop a `-` in front of a value.

    mynum = -1

Function application is simple, it's `argument1.functionName(argument2, argument3, ...)`. If a function takes only one argument you don't need the parentheses

    mynum = 1.negate

    myNum = 2.pow(4)


## Function Creation

Defining a function "add-one" that takes a parameter "a" - no signature. Zoda will use type inference to give `a` the trait `Addable`.
 
    a.plus-one = a + 1
     
We can give a signature to a function. All functions are values, the only difference is that their signature contains `->` 

    add-one ~Int -> Int
    a.plus-one = a + 1

    add ~ Int -> Int -> Int
    a.plus3(b) = a + b

    add3 ~ Int -> Int -> Int -> Int
    a.plus(b, c) = a + b + c

    add3 ~ (Addable a) => a -> a -> a -> a
    a.plus(b, c) = a + b + c

## Polymorphic Functions

The last section had a function that worked on any type that has the trait `Addable`.

    add3 ~ (Addable a) => a -> a -> a -> a
    a.plus(b, c) = a + b + c

The `=>` creates a "type variable", in this case it has the constraint that it must have the type `Addable`. You can have ones without the type.

    apply-to ~ (a, b) => (a -> b) -> a -> b 
    f.apply-to(a) = a.f

The `=>` does not have to be at the very front of the type.

    id-both ~ (b, c) => (a => a -> a) -> b -> c -> {* b, c *}
    f.id-both(b, c) =  {* f.b, f.c *}

But type inference typically won't assume a type has a `=>` in the middle of a type, so if you want to have a function like the above you should give it a type signature.  

# Existentials

By default, polymorphic values are universally quantified. Putting a `^` before the type variable makes it existentially qualified.


    useless ~ (a, ^b) => a -> ^b -- a is universally quantified, ^b is existentially quantified
    s.useless = True -- can return literally any value

Since the return value of `useless` is existentially quantified, it could be any value, but you don't know what the value is.

    useless ~ (a, ^b) => a -> ^b -- a is universally quantified, ^b is existentially quantified
    s.useless = True -- can return literally any value, but all possible return values must be the same type

    v ~ ^b => ^b
    v = 3.useless -- really nothing I can do with this value 

These are useful for recovering some nice properties from dynamic languages in a type safe way. For example, the following is allowed:

    myList = [1, "test", True] ~ List<^a => a>

Although you can't do anything with the contents of that list (yet).

## Function chaining

You can chain functions using `.`. This is useful for "pipelines"

    my-value = 3.f.g.h -- take 3, then apply f, then apply g, then apply h
    my-value = 3.plus(4).subtract(1).times(4).negate.divide(2).absolute-value


## Currying

If you want to, you can curry a function by leaving an `_` in the place of an argument.

    my-val = my-list.map(_.plus(1))

    my-val = my-list.map(1.plus(_))

This returns the curried function *immediately* - more functions afterward is probably a bad idea.

    my-val = my-list.foldl(_.plus(_), 0)  -- okay

    my-val = my-list.foldl(_.plus(2).divided-by(_), 0) -- not okay, `_.plus(2)` returns a function and `.divided-by` takes a number.

you can also apply none of the arguments of a function just by not doing anything with it at all, because functions are first-class values.

    my-val = my-list.map(add-one)


## Style tips

With the x.f(z) syntax, you probably want to make function names read nicely

    3.max(4)         -- bad
    3.lower-bound(4) -- good


## Indentation-sensitive Syntax

any time after a `:`, you should indent if you choose to make a newline


## if expressions

These use indentation for alignment to support nesting

    a.lower-bound(b) = 
      if a > b then:
        a
      else: 
        b

This is equivalent to:

    a.lower-bound(b) = 
      match a > b:
        True  ->: a
        False ->: b

You may be wondering - why have `if` if we can just use `match`? The answer is when we need to have many levels of if.

    a.stable-lower-bound(b) = 
      if a > b then:
        a
      else if a < b then:
        b
      else:
        a


The reason you don't need a `:` after that else is because for any token that ends in a `:`, followed immediately by another bit of syntax that ends with a `:`, the first can and should be left off. 

    my-val = 
      if 2==2 then:
        True
      else if 3==3 then:
        True
      else do:
        pure False

These are expressions so you may use them however you like. 

    a.lower-bound-times(b, x) = 
      x * if a > b then:
        a
      else: 
        b

## Tiny Docs

Tiny-docs are like comments, but better. These are tiny little docs that get scattered across your code, designed to aid other developers and anyone interested in using your library.  In general, you can put them after before `:` sign. They are seperated by \` symbols, and can reference in-scope values with `{ }`

    a.lower-bound-times(b, x) = 
      x * if a > b then `We don't need to choose {b} as our lower bound`:
        a
      else `Default to {b}`: 
        b

This seems sort of unreadable, I'm hoping syntax highlighting makes it better.

The hope is that since these are so close to the code, we can encourage developers to use them and keep them updated. The programmer is motivated to keep them short because we don't allow them to span more than one line. Since the reference existing values with special syntax, the dream is we can use this to give much better explanations and knowledge to the user. 

They're allowed in a couple other places, too. 

1) After function definitons

    a.lower-bound-times(b, x) `Compute the lower bound between {a} and {b}, then multiply it by {x}` = 
      x * if a > b then `We don't need to choose {b} as our lower bound`:
        a
      else `Default to {b}`: 
        b

2) After a type in a type signature for a function

    draw-circle ~ Int `Radius of the circle, in pixels` 
               -> Int `Stroke width of the circle, in pixels` 
               -> IO () `IO action that draws a circle`
    radius.draw-circle(stroke-width) = ...



## Lists

    my-list = [1,2,3,4]
    my-list = 1.prepend-to([2,3,4])   -- [1,2,3,4]

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



## higher order functions

    x.plus-one = x + 1
    my-list = [1,2,3,4,5].map(add-one)
    my-list = [1,2,3,4,5].map(_.plus(1))



## lambdas

    x.plus-one = x.(|x| ->: x + 1)

is equivalent to...

    x.plus-one = x + 1

In some languages you can set a lambda to a value directly, but this is not allowed in Zoda


## The value-passing operator

not really a language feature, just a built-in operator. simply passes a value to a function, left associative

    my-val = add-one <| 3
    my-val = _.plus-one <| 3
    my-val = 3.plus(_) <| 1
    my-val = 4


## do notation

    a.print-value-cool = do:
      putString("=============")
      print(a)
      putString("=============")

desugars to...

    a.print-value-cool =
      putString("=============") >>= \- ->
      print(a)                   >>= \- ->
      putString("=============") >>= \- ->


## where statements. 

Valid for functions as well as values. 

    my-val = 
        i + j
      where:
        i = 3
        j = 4


##inline test statements. 


These can be run with a compiler flag (and are automatically run when creating an optimized build), and only rerun if the function or its dependencies change.

    a.plus-one = a + 1
      test:
        Can add one to two
        >>> 2.plus-one
        ->: 3

You can have as many of these as you want.

    a.plus-one = a + 1
      test:
        Can add one to positive values
        >>> 2.plus-one
        ->: 3
        >>> 3.plus-one 
        ->: 4

And to contain multiple tests, use do notation again

    a.plus-one = a + 1
      test do:
        Can add one to positive values
        >>> 2.plus-one
        ->: 3
        >>> 3.plus-one
        ->: 4

        Can add one to negative values
        >>> -2.plus-one
        ->: -1
        >>> -3.plus-one
        ->: -2

If you know you haven't implemented the functionality for a test yet, just make it pending

    a.incrementAbsoluteValue = a + 1
      test do:
        Can increment to positive values
        >>> 2.plus-one 
        ->: 3
        
        Can increment to positive values
        >>>pending -2.plus-one
        ->: -3

If a test must pass

    a.incrementAbsoluteValue = a + 1
      test do:
        Can increment to positive values
        >>> 2.plus-one 
        ->: 3
        
        Can increment to positive values
        >>critical -2.plus-one
        ->: -3


## Match

You can do a full `match`, like so:

    a.luckyNumber7 = match a:
      7 ->: putString("Lucky number 7!")
      * ->: putString("sorry, you are a loser")
    l.head = match l:
      x.Cons(*) ->: x.Just
      *         ->: nothing 

All pattern matches must be exhaustive to perform an optimized build - this is to keep the convenient fact that optimized Zoda builds have no runtime errors in practice.

`_` can also be used with `match`:

    [Just 3, Nothing].filter(_) <| match _: 
      Just _ -> True
      Nothing -> False                        

A common pattern is to do a pattern match to see if a value is matches a pattern, and to return some kind of "true" result if it is, and an error result if it isn't. `match-partial` exists to solve this need - it returns a value with the trait `Errorable` where successful matches are on the right an unsuccessful matches are on the left.

    luckyNumber7 ~ (Errorable e) => Int -> e<Int, String>
    a.luckyNumber7 = partial-match a:
      7 ->: "Lucky number 7"
      else notSeven ->: notSeven
         

## Inline docs 

Seperated into "devl-doc" and "long-doc". The long-doc can be as long as you want. It is intended to be shown on documentation pages and should give an in-depth explanation with examples of how it should be used. The devl-doc is for people who'd like to modify the function, and should contain important information and context. These examples are also tested at compile time. 

Comments with `--` are allowed inside long-docs and devl-docs, and will not be included in the output.

    x.plus(y) `Add {x} and {y}` = 
      sum
      where:
        sum = x + y

      test do:
        Can add positive values
        >>> 2.plus(2)
        ->: 4
        >>> 3.plus(5)
        ->: 8

      long-doc:
        Essentially just a wrapper for the `+` operator.
        It works on any values that have the an `Addable` instance, so it can be used anywhere you'd like to add two values together
           
        >>> 3.plus(5)
        ->: 8

        -- Do we need to say this? this seems kind of obvious
        Negative numbers can be added as well.  

        >>> 3.plus(-5)
        ->: -2

        One use might be to compute a new bank account balance after some money is deposited:

        >>> oldBalance = 300
        >>> amountToAdd = 10
        >>> newBalance = oldBalance.plus(amountToAdd)
        >>> newBalance
        ->: 310

       Although since this function is just a wrapper for `+` there's little reason to use it unless you need currying. 

        >>> [1,2,3].map(_.plus(3)) 
        ->: [4,5,6]

      devl-doc:
        Since this function is just a wrapper for `+`, it's super snazzy and short. It might be worth it to write a `.minus` function as well.
        
        Maybe this function should be moved into the numeric-utils module.


Supported editors will have the long-doc collapsed by default, because otherwise it takes quite a bit of space which will probably not always be that helpful. 
The long-doc requires two newlines for a linebreak to be present in the output, except in code examples. This makes it possible to set the max line width to whatever 
you want within the long-doc and the formatter will adjust it for you.


## Strings and Chars

We use the same approach to handling Strings that Rust does. To borrow from their documentation:
A String is a sequence of Unicode scalar values encoded as a stream of UTF-8 bytes. 
All Strings are guaranteed to be a valid encoding of UTF-8 sequences. 
Additionally, unlike some languages, Strings are not null-terminated and can contain null bytes. 
Strings are allowed to span multiple lines, and are indentation-sensitive 
They can also contain inline expressions with and {s expr }. 

    my-val = 3
    my-string = "3 + 3 is {s 3 + 3 }" -- toString is called on the output of `3 + 3` and then concatenated to the rest of the String
    my-string = "My name is {s "E. Cummings".toLowercase }" 
    my-string = "Line 1 
                 Line 2" -- equivalent to "Line 1\nLine2"
    my-string = "Line 1\ 
                 Line 2" -- equivalent to "Line 1Line2"

String literals are escaped with a backslash. it's acceptable to use "\n", "\t", "\""and "\\". any other backslashes are an error. 

    my-string = "\"What is it?\" she asked.\n"

for use with regular expression libraries, prefix the string with 'r'. This disables `{}`, 
    
    my-regex = r""

## recursion

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
                       
## Tags statements

Functions and values can have certain facts stated about them in `tags` statements which serve to provide extra information to the compiler.
The only one I have any ideas for is `always-terminates`, which says about a function exactly what it says on the tin.

    x.plusOne = x + 1 
        tags: always-terminates

Although I expect `always-terminates` to not be that useful... if a function doesn't call `recurse` and only calls functions marked as 
`always-terminates`, then I think it should

In practice, all Zoda functions return a value or become caught in an infinite loop (there are some rare exceptions). This is useful because it prevents the vast majority of uncontrolled runtime crashes. But it can be counterproductive to writing useful functions that are composable.

For example:

    divided-by: Float -> Float -> Optional<Float>
    numerator.divided-by(denominator) = match denominator:
        0 ->: Nothing
        denominator ->: (numerator / denominator).Just

Now, we might want to write a `divided-by-3` function, and reuse `divided-by`. But we have an issue! `divided-by-3` will always work, but 
if we just plug it into `divided-by`, our function will end up returning `Optional<Float>`, instead of `Float`
To solve this, we add the magical `.!` function to the end of it. 

    x.divided-by-3 = x.divided-by(3).!

`.!` converts a value from a `Optional<a>` (actually, any value with the `Unwrappable` trait) to an `a` *if* the compiler can prove that it will always actually have a corresponding value. In this case, it can see the call to `divided-by` will only return `Nothing` if the `denominator` is `0`. Since we know at compile time the denominator is `3`, the `unwrap` function can automatically unwrap the `Just`. A more trivial example would be:

    3.Just.! -- 3
    Nothing.! -- compile time error!

So what's the magic? Simple. During compilation, we compile the program down to a set of verification conditions. These are conditions that are only valid if your program has a given property. One example of a verification condition might be "`divided-by` always returns an Optional<Float>". But these conditions also contain information about how functions behave given their inputs, for example "`divided-by` always returns a `_.Just` when passed a nonzero value". Then, your whole program is fed into an SMT solver, which checks whether all the conditions hold. If you can check that "`divided-by` always returns a `_.Just` when passed a nonzero value", and "`divided-by-3` will always pass a nonzero value to `divided-by`", then we can know it's safe to unwrap the `_.Just` to just `_`.

In addition, there is also the `!!` function, which unwraps *anything*.

    a = 3.divided-by(1).!!
    b = 3.divided-by(0).!! 

This function is obviously partial, because it will fail when it tries to evaluate `b`. It's allowed in Prototype builds but not Production builds, because sometimes you just want to bang out something fast without worrying about all the ways a function call could go wrong. 



## type-wrapper 

Equivalent to "newtype" in haskell

    type-wrapper Year = Year Int
    type-wrapper Name = Name String

The difference between 

    type-wrapper Year = Year Int

and

    type Year = Year Int

Is that type-wrappers are guaranteed to be equivalent at runtime. This means you can use the `.coerce` function to wrap or unwrap types, even deeply nested ones. 

## type declarations and ADTs

Used for creating a new ADT 

    type Bool = True :: Bool 
              | False :: Bool
    type CarBrand = GM :: CarBrand
                  | Ford :: CarBrand
                  | Toyota :: CarBrand
    type Car = Car :: CarBrand -> Year -> Car
    type Person = Person :: Name -> List<Car> -> Person
    type IntList = EmptyIntList :: IntList 
                 | Cons :: Int -> IntList -> IntList -- Recursive data types are allowed, but only "one level deep". 
                                                     -- This restriction also applies to functions, as we'll discuss later

parameterized types

    type Optional<a> = Nothing :: Optional<a> 
                     | Just :: a -> Optional<a>

    type Result<e, r> = Error :: e -> Result<e, r> 
                      | Result :: r -> Result<e, r>  

## Synonyms

These are just useful when you want to give a possibly more descriptive name to a type. They can have parameters but cannot be partially applied.

    synonym FilePath = String
    synonym Either<a><b> = Result<a><b>


## Trait and instance declarations

Unlike in default haskell, type classes can have multiple parameters. In this case, we're relating two types by saying one is a collection
and another is an element. We also say that `Eq` is a supertrait for `e`, so the elements of any collection are required to have the `Eq` trait.
We probably wouldn't do that in real life though because it's useful sometimes to have collections of functions and it's not possible to give
functions an `Eq` instance. Also, we've added a functional dependency `c -> e`, by saying that the type of the element is soley determined by the type
of the collection. This just means that for any `Collection c => c`, there should be a unique type `e` known at compile time.  

    new-trait Eq e => Collection c e | c -> e where
        insert: c -> e -> c
        member: c -> e -> Bool

    has-trait Eq a => Collection (List<a>) a where
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

`has-trait` declarations for a trait `MyTrait a b c` must be either in the same file as the trait is defined, or in the same file as the type `a`, `b`, or `c` is defined. 

## Tuples

Usage of tuples is typically discouraged but they can sometimes make code simpler. Tuples cannot have more than 64 values.

    {* "Hello", 2 *} ~ {- String, Int *}

These spaces are required

These can sometimes be useful when you want to return a value from a function. 

    one-greater-one-less ~ Addable a => a -> {* a, a *}
    x.one-greater-one-less = {* x - 1, x + 1 *}

You can retrieve a value from a tuple with pattern matching.

    x.very-bad-double = match x.one-greater-one-less:
        {* one-greater, one-less *} -> one-greater + one-less

You can also use `.get1st`, `.get2nd`, etc.

    x.very-bad-double = t.get1st + t.get2nd
        where: t = x.one-greater-one-less

You can write a function that inserts a value into a tuple with an `_`.

    list-to-hello-tuple ~ List<a> -> List<{* a, String *}>
    l.list-to-hello-tuple = l.map({* _, "hello" *})





=============== I'm unsure about how all this will work ===============
=================== Need to get batter at purescript ==================


## Records 

http://www.cs.ioc.ee/tfp-icfp-gpce05/tfp-proc/21num.pdf

A record is like a type-safe dictionary, implemented internally using Rows. Duplicate keys are allowed.

    author ~ { name ~ String, interests ~ List<String> }
    author =
        { name = "Phil"
        , interests = ["Functional Programming", "JavaScript"]
        }

Records have no inherent order

    author ~ { name ~ String, interests ~ List<String> }
    author =
        { interests = ["Functional Programming", "JavaScript"]
        , name = "Phil"
        }

As a bit of syntax sugar, if you have a value who's name is the same as a record's field then you can just insert it wholesale

    interests = ["Functional Programming", "JavaScript"]
    
    author ~ { name ~ String, interests ~ List<String> }
    author =
        { interests
        , name = "Phil"
        }

Records have 3 operations that work on them:

    1) Combining two records with `...`

        fst = { i = 4, f = 3.5 }
        snd = { a = "test", ...fst }   -- { a = "test", i = 4, f = 3.5 }

    2) Extracting a value from a record with auto-generated functions 

        fst = { i = 4, f = 3.5 }
        fst.i     -- 4
                  -- i's signature is `{ i ~ a, ...r } -> a 
    
    3) Removing a value from a record using the magic `without-x` function 

        fst = { i = 4, f = 3.5 }
        fst.without-i     -- { f = 3.5 }
                          -- without-i ~ { i ~ a, ...r } -> { ...r }

There's also some convencience syntax sugar for updating values

    fst = { i = 4, f = 3.5 }
    fst{ i = 3 } -- { i = 3, f = 3.5 }

Nested values can be updated with the `.` sugar

    snd = { i = { b = { c = 4 } } }
    snd{ i.b.c = 3 }      -- { i = { b = { c = 3 } } }

To apply a function to a nested value, you can use `$=`

    snd = { i = { b = { c = 4 } } }
    snd{ i.b.c $= _.plus(1) }      -- { i = { b = { c = 5 } } }

You can write a function that takes a record, if you want.

    getName ~ { name ~ String, ...l } -> String 
    r.getName = r.name

The "...l" is a type variable. This means it can take the place of literally any other record (including records that have a `name` key in them, as duplicate keys are allowed).

You can perform pattern matching on records, as you might expect.

    changeNameUnlessNamedSteve ~ { name ~ String, ...l } -> Bool
    r.changeNameUnlessNamedSteve = match r with:
        { name = "Steve", ...r } -> { name = "Steve", ...r }
        { name = _, ...r }       -> { name = "Mike", ...r }

Example of a function that modifies the name field of a record

    addJrSuffix ~ { name ~ String | r } -> { name ~ String | r }
    addJrSuffix hasName = hasName{name $= _.concat(", Jr.") }
    addJrSuffix { name = "Bob" }                -- { name = "Bob, Jr." }   
    addJrSuffix { age = 42, name = "Gerald" }   -- { age = 42, name = "Gerald, Jr." }   
    addJrSuffix { age = 42, name = "Gerald", name = "Steven" }   -- { age = 42, name = "Gerald, Jr.", name = "Steven" }   

You can give a name to a record type with `synonym`

    synonym Book = { name ~ String, text ~ String }
    synonym Author = { name ~ String, books ~ List<Book> }

You can unwrap a record into another when defining them

    synonym Name ~ { name ~ String }
    synonym Country ~ { population ~ Int, ...Name }
    myCountry = { name = "usa", population = 2000 }

A `_` inside a record literal creates a function which takes some values and creates a record with that type

    ["usa", "canada"].map({name ~ _, population ~ 2000})

Like how `a` is a variable that can take any type, `{ ...r }` is a record with any (or no) values

    myFunc ~ { ...r } -> { ...r }
    r.myFunc = r -- the only possible implementation of a function with this type is the identity function.


In general, two record types are equal if they contain the same `key ~ type` pairs. The order is irrelevant when they keys are all unique. With duplicate keys, the order in the type always matches the order in the literal. Here is an example:

    fst = { x = 2, x = True }      -- { x ~ Int, x ~ Bool }
    snd = { x = True, x = 2 }      -- { x ~ Bool, x ~ Int }
    thrd = { x = True, x = 2 } ~     { x ~ Bool, x ~ Int } -- type error
    frth = { x = True, x = False } -- { x ~ Bool, x ~ Bool } -- type error
    
    fst.without-x.x = True
    snd.without-x.x = 2
    snd.without-x.x = False

    -- if you had a function of type  `{ x ~ Int, x ~ Bool } -> Bool`, you would not be able to pass it `snd`.

Due to the inherent complexity of having records with duplicate keys, I reccomend you keep their usage to a minimum. 

Oh and I glossed over this, but records can have type constraints

    fst = { x = 2, x = True } ~ Integral a => { x ~ a, x ~ Bool }


## Corecords

Records:Corecords ~ Product types:Sum types

While a record must have every value in its type, a corecord can only have one.

    a ~ {+ b ~ Int, c ~ String +}
    a = {+ b = 3 +}

These can also be open, like so:

    {+ name ~ String, ...v +}

Again, duplicate keys are allowed:

    {+ name ~ String, name ~ String, name ~ Bool +}

This basically means it can have any possible `key ~ value` pair but definitely might have `name ~ String`. This seems useless until you use it with functions:

    {+ name ~ String, ...v +} -> {+ ...v +} 

This function takes a corecord has the possibility of containing a `name ~ String` pair, and returns one that has "one less possibility" to have a `name ~ String` pair.

These can be pattern matched with `match`, naturally.

    -- if the corecord is closed, it can easily be pattern-matched exhaustively  
    getFooOrBarLength ~ {+ foo ~ String, bar ~ String +} -> Int 
    r.getFooOrBarLength = match r:
        {+ foo = f +} ->: f.length
        {+ bar = b +} ->: b.length

    -- if it's open, you must have a wildcard element
    getFooOrBarLength ~ {+ foo ~ String, bar ~ String | l +} -> Int 
    r.getFooOrBarLength = match r:
        {+ foo = f +} ->: f.length
        {+ bar = b +} ->: b.length
        *             ->: 0 
    
You can have an empty corecord with `{+ +}`, but it is impossible to create a value that occupies this type and the type system knows this.

    getRight ~ Result<{+ +}, a> -> a
    e.getRight = match e:
        e.Result ->: e 
        -- No need to pattern match Error because we all know that `{+ +}` can't have any values.
        
The snazzy thing is that `match-partial` has special support for these - the values you match against are removed from the type.

    deal ~ {+ a ~ Int, b ~ String +} -> Result<{+ a ~ Int +}, String>
    a.deal = match-partial a with:
        {+ b = b +} ->: b 
        else a ->: a


## Modules, packages and imports

There are 3 "tiers" in Zoda. The lowest is values - these are functions and constants. They have metadata like `tiny-doc`, `long-doc`, and `tags`. These all live inside "modules". Values form a DAG of their dependencies.

A module is just a collection of values. Every file is a module. Like values, modules can depend on each other via `import` and form a DAG. If a module is named `main`, it must be a "root" module and cannot be imported by any other module. The name of a module is always the name of the file (without the `.zoda` extension), with optional descriptive words at the end seperated by `-`. If a module is named `main`, it must have a value named `main` of type `IO a`. Like functions, they have "tiny-doc" and "long-doc". This is what a module might look like:

    -- adding-utils.zoda
    module adding-utils `Collection of utilities related to adding values`
      author: Andre Popovitch
      user-doc: 
        These utilities are part of the greater "adding" library. 
        They're useful in all situations where you need to add something.
           
        -- maybe include more info here on when to use these?
        If you need to add 3, check out the `plus-3` function. Other useful 
        functions include `plus-4` and `plus-5`.           

        Examples: 

        >>> 3.plus-4 
        ->: 7

        >>> 3.plus-5
        ->: 8
      devl-doc:
        Research is ongoing whether a `x.plus-6` function is possible. Contributions would be welcomed!
      importing:
        (plus) from generic-plus           -- import the "plus" function from the module "generic-plus".
        negatory:negation                  -- import the "negation" module from the "negatory" package
      exporting:
        plus-3, plus-4, plus-5, plus-negative
                  
    x.plus-0 = x -- not exported because it's useless
    x.plus-3 = x.plus(3) -- these would typically have their own tests, tiny-docs, user-docs, and devl-docs, but I've left them off for the sake of brevity.
    x.plus-4 = x.plus(4) 
    x.plus-5 = x.plus(5) 
    x.plus-negative(y) = x.plus(y.negation:negate)         -- namespacing is done with `:`

When executing a package, execution begins at the `main` function inside the `main` module. More specifically, it just executes whatever actions the `main` module returns. Every Zoda package has a `main` module - since it must be the root level module, it cannot export anything (and does not have a `user-doc`). The `devl-doc` of your main module serves as a guide to new contributors to how they should look at and edit the whole package. It is *not* intended to be a high level description of your package for end-users - that should be the `introduction` chapter of the book for your package. (a `readme.md` will be automatically created from the book, more on this later).

A package is always contained entirely within one folder. It also always has a `book` directory and a `zoda.toml` file. The book directory contains all the documentation about the package that does not make sense to be stored in the source files. The `zoda.toml` file contains information about what other packages this package depends on, and what modules it exports. 



# Grammar

This isn't a full grammar for the whole language. WIP

```
Module := ModuleHeader Definitions
ModuleHeader := "module" LowercaseIdentifier 

```