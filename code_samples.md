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


An `=` is always followed by a value, it's how you set a value. A `:` is always followed by a type, it's how you tell Zoda the type of a value.


Here we're defining the same value multiple times, this is illegal but we're just doing it for demonstrative purposes.

    myNum = 3

    myNum : Int
    myNum = 3

    myNum = (3 : Int) -- inline type signatures must be wrapped in parentheses

Types can be ambiguous with "traits" - here we say "myNum" has the trait "Integral", but we're not any more specific.

    myNum : (a : Type) @-> a precondition a has-trait Integral
    myNum = 3

    myNum = 3 : ((a : Type) @-> a precondition a has-trait Integral



## Function Application

Operators always take two values. the format for applying them is `value1 SPACE operator SPACE value2` 

    myNum = 1 + 2
    mynum = 3 - 1

Negation is the only expection - just pop a `-` in front of a value.

    mynum = -1

Zoda supports both of the manners in which you are likely accustomed to calling functions:

    mynum = 1.negate
    
    mynum = negate(1)

    mynum = 2.pow(4)

    myNum = pow(2, 4)


Althought he former (called UFCS) is preferred in the vast majority of cases. Since the two are basically equivalent, the fomatter may decide for you which to use.  

If you want to curry a function, you can also use `...`:

    myFunc = pow(2, ...)
   
This returns a function that takes `4` and outputs the result of `pow(2, 4)`. If a function expects additional functions *according to its written type signature* then you must either use `...`, `_`, or give it all the parameters it expects. For example:

   id : (a : Type) @-> a -> a
   id(x) = x
   myfunc : Int -> Int
   myfunc(a) = a + 1
   id(myfunc) -- id only takes one parameter so you only need to give it one. 
   
   -- Let's specialize the id function
   id' : (Int -> Int) -> Int -> Int -- equivalent to (Int -> Int) -> Int -> Int
   id'(myfunc, 3) == 4
   
## Uniqueness

Each type has a "Uniqueness attribute". I haven't been writing what them elsewhere because I'm still figuring it out. A uniqueness attribute is either `Unique` or `Borrowed`. These have the kind `UA` (Uniqueness Attribute). Base types, like `Int` and `Bool`, have kind `T`. Types, as you would expect, have kind `Type`.

This does not come up when writing an ordianry polymorphic function:

    id : (a : Type) @-> a -> a

But, to be polymorphic over `Type`, your function must follow the restrictions placed by both `Unique` and `Borrowed`. This essentially means values may be `consumed` at most once - they can be passed without worry to any function which takes a `Borrowed`, but can only be passed once to a function which takes a `Unique`.

The classic example is the `write` function, which takes an array, an index, and a value, and returns a new array which has the updated value.

    [10, 20].write(1, 2) == [10, 2]
   
For efficiency, `write` takes a unique array. This means that the value can be updated in place, as there is only one reference at runtime to any unique value at the time the function is called.

    [10, 20].write(1, 2).write(0, 1) == [1, 2] -- both `write` operations performed in-place, no copying and no garbage to be collected.
    
The other attribute is `Borrowed`, which represents a value for which there may be multiple references at runtime. You cannot pass a `Borrowed` value to a function which expects a `Unique`. 

Here is an example of the type of `write`:

    write : Array^Unique -> Int -> Int -> Array

The `^Unique` means that the value must be `Unique` - the values without a tag represent them being polymorphic in their uniqueness. This means that you must use them in a way that is consistent with them being either `Unique` or `Borrowed`, which pretty much just means that inside the function you must act as if they're a borrowed value.

If you want to require that a parameter be borrowed, naturally you can use `^Borrowed`. You also could require that the output be `Unique`.

    write : Array^Unique -> Int^Borrowed -> Int^Borrowed -> Array^Unique
    
The only time you should use this is when you have to guarantee that the return value is used uniquely, for some reason. A value is either unique or borrowed, but in the prior case it is left ambiguous using universal quantification. `Int` is actually syntax sugar for `(u : UA) @-> Int^u` - it can be parameterized with `Unique` or `Borrowed` depending on the situation. This simulates the intutitive fact that if a function expects a borrowed value, you may pass it a unique, but not the other way around.

Also, a tip: the type of `->` is a bit tricky: if the positive position is a unique type (and it's used in the function body), then the type of `->` is also unique, and unique functions can only be applied once. If this were not the case, you would be able to do sneaky things like this:

    one-up-one-down : (a : T) @-> a^Unique -> {fst: a^Unique, snd: a^Unique}
    one-up-one-down(x) = (|f| -> {fst: f Unit, snd: f Unit})(const(x, _))
        where
            const(a, b) = a
            
This does not use `x` multiple times, which is the requirement - however, it does use `f` twice, and since `x` is of a unique type, that means `f` must be `unique` as well, which means this won't typecheck. This is all done pretty much automatically for you. Here's the type signature of `const`:

    const : (a : Type) @-> (b : Type) @-> a -> b -> a
    const(a, b) = a

This looks simple because a lot of the work is being done by the kind system. Specifically `->` is automatically unique if its positive position is unique. Other cases like records and corecords are unique when any of their values are unique. While this is helpful, you might need to have more control over uniqueness types (such as when making your own types), so you can pass them in manually:

    storeBoth : (u : UA) @-> Int^u -> Int^u -> (Int^u, Int^u)
    
To implement something like a pair yourself, you can take advantage of the fact that boolean operations work on Uniqueness types (where `Unique` is seen as `True` and `Borrowed` is seen as `False`). Here's how to make a pair type that is unique if any of its elements are unique:

    data Pair(a, b) : (a : T) @-> (au : UA) @-> (b : T) @-> (bu : UA) @-> a^au -> b^bu -> T^(au or bu) where
        mkPair : (a : Type) @-> (b : Type) -> a -> b -> Pair(a, b)
        
God, this is verbose. Luckily I'm pretty sure that this should be inferable 99% of the time and you won't actually have to write it. If it could be inferred, this is what it looks like:

    data Pair(a, b) where
        mkPair : (a : Type) @-> (b : Type) -> a -> b -> Pair(a, b)

## Function Creation

Defining a function "add-one" that takes a parameter "a" - no signature. Zoda will use type inference to give `a` the trait `Addable`.
 
    plus-one(a) = a + 1
     
We can give a signature to a function. All functions are values, the only difference is that their signature contains `->` 

    add-one : Int -> Int
    plus-one(a) = a + 1


    add : Int -> Int -> Int
    add(a, b) = a + b

    add3 : Int -> Int -> Int -> Int
    add3(a, b, c) = a + b + c

    add3 : (a : Type) @-> a -> a -> a -> a precondition a has-trait Addable
    add3(a, b, c) = a + b + c
    
    
## Polymorphic Functions

The last section had a function that worked on any type that has the trait `Addable`.

    add3 : (a : Type) @-> a -> a -> a -> a precondition a has-trait Addable
    plus(a, b, c) = a + b + c

## Type signatures

`:` denotes a type sig. Here's an example of a polymorphic function:

    equals : (e : Type) @-> e -> e precondition
               Eq(e)
    equals(a, b) = a == b
   
`@` is used to denote an implicit parameter. All functions are always parametric with respect to their implicit parameters (never branch based on them or otherwise observe their value directly). One may pass implicit variables explicitly, like this: `equals(@Int, 3, 4)`. 

The `precondition` section describes the constraints that are placed on variables on the function. These can be simple or complex. For example:

    get-index : (e : Type) @-> List(e) -> (n : Int) -> e 
        precondition
            length(list) > n
            n > 0

This ensures that finding the `n`th element of the list with `get-index` will never fail, because it is statically guaranteed at runtime that the length of the list is greater than the index we're trying to access, and the index we're trying to access is not negative.

There are two more things we can do with type signatures, to further their explanatory power. We can create an error type (which is not actually used directly by the function):

    type ListIndexAccessError = NegativeIndex | IndexOutOfBounds
    get-index : (e : Type) @-> (l : List(e)) -> (index : Int) -> e 
        precondition
             length(l) > n, index-out-of-bounds
             n > 0        , negative-index 

Finally, we can give a description of the cause of the error in backticks, and information that might be useful for error-catching. The backtick'd message is a tiny-doc, which will be touched on more later:

    get-index : (e : Type) @-> (l : List(e)) -> (index : Int) -> e 
        precondition
            length(l) > n, index-out-of-bounds, {length: length(l), index: n}, `You cannot access index {n} of {l} because 
                                                    it is only {length(l)} long.`
            n > 0        , negative-index   , {index: n}, `You cannot access the negative index {n} of {l}.`

These show up when the condition is *not* satisfied for whatever reason. The message in the backtics is used to give a more human-friendly explanation of the error and why it exists than that which can be generated automatically.

This is also where you put trait constraints. Here's an example of using a trait constraint to make our `get-index` function work with any type of container:

    get-index : @(e : Type) -> @(c : Type) -> (l : c(e)) -> (index : Int) -> e 
        precondition
            c has Collection(e)
            length(l) > n, index-out-of-bounds, {length: length(l), index: n}, `You cannot access index {n} of {l} because 
                                                    it is only {length(l)} long.`
            n > 0        , negative-index   , {index: n}, `You cannot access the negative index {n} of {l}.`

(Trait constraints don't get an error-doc or the ability to add an error type)

For any situation where you *don't* want to bother making sure a function's constraints are satisfied, you can just add a `?` to the end of the function name. This makes a function that returns a result type that indicates if the constraints were not found to hold at runtime, or contains the result otherwise. For result types that support it, it can also contain some more detailed information inside a `DebugInfo`, containing the interpolated tiny-doc, the stack trace, all the arguments to the function, etc. Obviously, this would only be done in development builds. This would seem impure, but since the contents of a `DebugInfo` can never affect the execution of a program it's probably okay.

Names bound in type signatures can only be used "after" they're bound. 

(Note that when you're actually defining a function, the names of parameters inside the type need to match the names of parameters to your function, or a warning will be emitted)

In addition to `precondition`, there is also `post-condition`:

    add : (a : Int) -> (b : Int) -> (result : Int) 
        post-condition 
            if b > 0 then result > a else True
        
Shorthand for `if x then y else True` is the `==>` operator:

    add : (a : Int) -> (b : Int) -> (result : Int) 
        post-condition 
            b > 0  ==>  result > a 
        
Putting a `?` after the name of a function will allow you to use it with values that are not known to match those predicates at runtime, but if they do not match the predicate it will return an error value, and if they do it will return a result value. The error value will be a corecord (polymorphic variant) containing the first mismatched predicate found (predicates are checked in the order in which they appear).

    get-name-from-id(id) = match nameAccess with
            name.Res                          -> print("The name for id {id} is {name}.")
            {+ index-out-of-bounds = * +}.Err -> print("The id {id} was not present in our database.")
            {+ negative-index      = * +}.Err -> print("Negative ids are not permitted.")
        where
            nameAccess = nameList.get-index?(id)

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

You can also apply none of the arguments of a function just by not doing anything with it at all, because functions are first-class values.

    my-val = my-list.map(add-one)


## Style tips

With the x.f(z) syntax, you probably want to make function names read nicely

    max(3, 4)         -- bad
    lower-bound(3, 4) -- good


## Indentation-sensitive Syntax

## if expressions

These use indentation for alignment to support nesting. They have similar syntax to match expressions, except the last element doesn't have a rhs (it is always matched if there are no other matches. 

    lower-bound(a, b) = 
      if 
        a > b -> a
        else  -> b

This is equivalent to

    lower-bound(a, b) = 
      match a > b with
        True  -> a
        False -> b

These are expressions so you may use them however you like. 

    lower-bound-times(a, b, x) = 
      x * if 
        a > b -> a
        else  -> b


You can have as many cases as you like:

    get_salary(name, profession, company) = 
      if 
        name == "bob"                     -> 3000
        profession = "janitor"            -> 40000
        company.lower.starts-with("goog") -> 2000
        else                              -> 8000
    
Compared to if/else if/else syntax, this is very concice and easy to read. Every if expression must end with an `else` (this ensures exhaustiveness).

## Tiny Docs

Tiny-docs are like comments, but better. These are tiny little docs that get scattered across your code, designed to aid other developers and anyone interested in using your library.  They are seperated by \` symbols, and can reference in-scope values with `{ }`. Newlines are allowed but discouraged.

The hope is that since these are so close to the code, we can encourage developers to use them and keep them updated. Since the reference existing values with special syntax, the dream is we can use this to give much better explanations and knowledge to the user. Here is where they're allowed: 

1) After function definitons

    a.lower-bound-times(b, x) `Compute the lower bound between {a} and {b}, then multiply it by {x}` = 
      x * if a > b then 
        a
      else  
        b

2) After a type in a type signature for a function

    draw-circle : Int `Radius of the circle, in pixels` 
               -> Int `Stroke width of the circle, in pixels` 
               -> IO () `IO action that draws a circle`
    radius.draw-circle(stroke-width) = ...

3) After constraints (see section on type sigs)

## Lists

    my-list = [1,2,3,4]
    my-list = 1.prepend-to([2,3,4])   -- [1,2,3,4]

These aren't lists specifically, as they can be overloaded so `[1,2,3]` can represent a list, a vector, an array, etc. But we'll be focusing on lists for now.

    my-iterator = [1..10]
    my-iterator = [10,20..100]

Since they're lazy, they're useful for functions like the following:

    factorial(n) = [1..n].product

If iterators were not lazy, this would require a O(n) space. Since they are, it requires O(1) space. 
list slicing is supported and similar to python's

    [1,2,3,4].getIndex(0) == Just 1

    [1,2,3,4,5].slice(1, 4) == Just [2,3,4]

    [1,2,3,4,5].drop(2) == Just [3,4,5]

    [1,2,3,4,5].take(2) == Just [1, 2]



List slicing doesn't just work with lists, it works with anything that has the `Slicable` trait This includes Iterators, Vectors, Strings, etc. We have list comprehensions too. Of course, they're not specific to lists. They can actually be used with any Monad.

    myList = [ (x, y) | x <- xs,
                        y <- ys ]



## Higher Order Functions

    x.plus-one = x + 1
    my-list = [1,2,3,4,5].map(add-one)
    my-list = [1,2,3,4,5].map(_.plus(1))


## Lambdas

    x.plus-one = x.(|x| -> x + 1)

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

    print-value-cool(a) = do
      putString("=============")
      print(a)
      putString("=============")

desugars to...

    print-value-cool(a) =
      putString("=============") >>= \- ->
      print(a)                   >>= \- ->
      putString("=============") >>= \- ->


## where statements. 

Valid for functions as well as values. 

    my-val = 
        i + j
      where
        i = 3
        j = 4


## Inline test statements. 

These can be run with a compiler flag (and are automatically run when creating an optimized build), and only rerun if the function or its dependencies change.

    a.plus-one = a + 1
      test
        Can add one to two
        >>> 2.plus-one
        ->: 3

You can have as many of these as you want.

    a.plus-one = a + 1
      test
        Can add one to positive values
        >> 2.plus-one
        -> 3
        >> 3.plus-one 
        -> 4

And to contain multiple tests, use do notation again

    a.plus-one = a + 1
      test
        Can add one to positive values
        >> 2.plus-one
        -> 3
        >> 3.plus-one
        -> 4

        Can add one to negative values
        >> -2.plus-one
        -> -1
        >> -3.plus-one
        -> -2

If you know you haven't implemented the functionality for a test yet, just make it pending

    a.incrementAbsoluteValue = a + 1
      test do:
        Can increment to positive values
        >> 2.plus-one 
        -> 3
        
        Can increment to positive values
        >>pending "incrementing negative values is currently not allowed"
        >> -2.plus-one
        -> -3


## Match

You can do a full `match`, like so:

    a.luckyNumber7 = match a with
      7 -> putString("Lucky number 7!")
      * -> putString("sorry, you are a loser")
    l.head = match l with
      x.Cons(*) -> x.Just
      *         -> nothing 

All pattern matches must be exhaustive to perform an optimized build - this is to keep the convenient fact that optimized Zoda builds have no runtime errors in practice.

`_` can also be used with `match` to create a match function. If you don't care about the value of something that might be matched, use `*` to indicate a wildcard.

    [Just 3, Nothing].filter(_) <| match _ with
      Just(*) -> True
      Nothing -> False                        

A common pattern is to do a pattern match to see if a value is matches a pattern, and to return some kind of "true" result if it is, and an error result if it isn't. `match-partial` exists to solve this need - it returns a value with the trait `Errorable` where successful matches are on the right an unsuccessful matches are on the left.


    luckyNumber7 : (e : Type -> Type) => Int -> e(Int) given Errorable(Int, e)
    a.luckyNumber7 = partial-match a:
      7 ->: "Lucky number 7"
      else notSeven ->: notSeven
         

## Inline docs 

Seperated into "devl-doc" and "long-doc". The long-doc can be as long as you want. It is intended to be shown on documentation pages and should give an in-depth explanation with examples of how it should be used. The devl-doc is for people who'd like to modify the function, and should contain important information and context. These examples are also tested at compile time. 

Comments with `--` are allowed inside long-docs and devl-docs, and will not be included in the output.

    x.plus(y) `Add {x} and {y}` = 
      sum
      where
        sum = x + y

      test do
        Can add positive values
        >> 2.plus(2)
        -> 4
        >> 3.plus(5)
        -> 8

      long-doc
        Essentially just a wrapper for the `+` operator.
        It works on any values that have the an `Addable` instance, so it can be used anywhere you'd like to add two values together
           
        >> 3.plus(5)
        -> 8

        -- Do we need to say this? this seems kind of obvious
        Negative numbers can be added as well.  

        >> 3.plus(-5)
        -> -2

        One use might be to compute a new bank account balance after some money is deposited:

        >> oldBalance = 300
        >> amountToAdd = 10
        >> newBalance = oldBalance.plus(amountToAdd)
        >> newBalance
        -> 310

       Although since this function is just a wrapper for `+` there's little reason to use it unless you need currying. 

        >> [1,2,3].map(_.plus(3)) 
        -> [4,5,6]

      devl-doc
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
They can also contain inline expressions with and {expr}. 

    my-val = 3
    my-string = "3 + 3 is {3 + 3}" -- toString is called on the output of `3 + 3` and then concatenated to the rest of the String
    my-string = "My name is {"E. Cummings".toLowercase}" 
    my-string = "Line 1 
                 Line 2" -- equivalent to "Line 1\nLine2"
    my-string = "Line 1\ 
                 Line 2" -- equivalent to "Line 1Line2"

String literals are escaped with a backslash. it's acceptable to use "\n", "\t", "\""and "\\". any other backslashes are an error. 

    my-string = "\"What is it?\" she asked.\n"

for use with regular expression libraries, consider raw strings (done the exact same way as rust): 
    
    my-regex = r###" test #"# lala\\efdsfds "###

These contain newlines but still need indentation:

    my-regex = r###"test
                    test"### -- equivalent to "test\ntest"


## Recursion

Zoda does not allow unrestricted recursion. The reason is that unrestricted recursion makes it easy to create a stack explosion:

    x.fib = match x 
        0 -> 1 
        1 -> 1 
        x -> (x - 1).fib + (x - 2).fib

This is bad because `100000.fib` will result in a stack overflow, which is a form of runtime exception. Since Zoda attempts to make runtime exceptions very rare, we need a way to prevent this. There is a way to do recursion without running the risk of a stack overflow, though. If the result of the recursive call is returned immediately, its very simple for the compiler to unroll it into a loop which will not result in unbounded growth of the stack. 

    x.fib = internalFib x 0 1 
        where internalFib(counter, y, z) = match counter:
            0       ->: z
            counter ->: internalFib(counter - 1, z, y+z)

so, we can prevent unbounded growth of the stack by disallowing all recursion that is not tail recursion. But, this is annoying because it means recursive calls are  "special". I think this would be easier to teach to people if we used a special keyword for tail calls. I'm thinking `and-recurse`.

    x.fib = internal-fib x 0 1 
        where internalFib(counter, y,z) = match counter:
            0       ->: z
            counter ->: and-recurse(counter - 1, z, y+z)

so `and-recurse` is a special function that calls the function it's used in. The compiler will not let you do anything to a the output of the `and-recurse` function, this ensures TCE can be performed. The `and-recurse` function is the only situation where recursion is possible - besides this, all functions form a DAG of their dependencies (this means no mutual recursion). 

Also polymorphic recursion is not allowed for performance reasons (prevents monomorphization).

The one exception to the "you can't do anything to the output of the `and-recurse` function" rule is functions with the tag `Chainable`. This doesn't blow up the stack because the `Chainable` tag includes instructions on how to compute an intermediate value. `+`, `*` and `Cons` are examples of Chainable.

    x.factorial = x * (x-1).and-recurse
    
    filter(l, f) = match l:
        []      ->: []
        x.Cons(xs) ->: 
            if (f x) then: 
                x.Cons(xs.and-recurse(f))
            else
                xs.and-recurse(f)
                       
## Tag statements

Functions and values can have certain facts stated about them in `tags` statements which serve to provide extra information to the compiler.

    x.plusOne = x + 1 
        tags
            always-terminates

Although I expect `always-terminates` to not be that useful. We should be able to prove most functions terminating with the SMT solver.

Other ideas:

A deprecation warning tag could be used to provide compile-time warnings when you use a deprecated function.

    plusOne :: Int -> Int
        tags
            deprication-warning("This function is being depricated, use `+ 1` instead.")
    plusOne(x) = x + 1 
           
A `reversable` tag could be used to do some tricks such as allowing more advanced pattern matching, in theory:
           
           
    plusOne :: Int -> Int -> Int
        tags
            reversable-in-first-argument ( \(result, x) -> result - x )
            reversable-in-second-argument( \(result, y) -> result - y )
    plus(x, y) = x + y
            
    -- later... 
    
    max-index + 1 = length(l) -- gets converted at compile time to max-index = length(l) - 1
    
An `unsafe` tag could be used to allow a value to reference other values that have the `unsafe` tag:

    to-Int : Age -> Int
        tags
            unsafe
    to-Int(age) = age.unsafeCoerce -- unsafeCoerce is a function that turns a 
                                   -- value of one type into a value of any other type, with the same
                                   -- represenation in memory. Since this could obviously call a segfault,
                                   -- it's an unsafe function
            
A `safe-wrapper-over-unsafe` tag could be used to allow a value to reference values that have the `unsafe` tag, while being able to be referenced by safe values. The idea being that if you use `safe-wrapper-over-unsafe`, you *really* have made sure this function won't ever do unsafe stuff, even though it calls functions that could potentially be used in an unsafe way.

    coerce : a -> b precondition 
        Coercable(a, b)     -- the Coercable trait only relates types which are 
                            -- guaranteed to have the same representation in memory
        tags
            safe-wrapper-over-unsafe
    coerce(a) = a.unsafeCoerce
            
A `chainable` tag could be used to allow more ergonomic tail-recursion:

    cons : (a : Type) @-> a -> List(a) -> List(a)
        tags
            chainable({
                        start-flattened  = \(a)    -> (\x -> a.Prepend(x)),
                        append-flattened = \(c, a) -> (\x -> c(a) ++ x),
                        expand           = \(c, b) -> c(b)
                     })
    cons(x, xs) = x.Prepend(xs)
                     
    -- This function can be made stack-safe by transforming it to be tail-recursive. 
    map(xs, f) = match xs with
        x.Prepend(Nil) -> x.f.Prepend(Nil)
        x.Prepend(xs) -> x.f.cons(and-recurse(xs, f))
    


## type declarations and ADTs

Used for creating a new ADT 

    type Bool where
                True  : Bool 
                False : Bool
    type CarBrand where
                    GM     : CarBrand
                    Ford   : CarBrand
                    Toyota : CarBrand
    type Car where Car : CarBrand -> Year -> Car
    type Person where Person : Name -> Car -> Person

Parameterized types

    type Optional(a) where
                       Nothing : Optional(a)
                       Just    : a -> Optional(a)

    type Result(e, r) where
                        Error  : e -> Result(e, r) 
                        Result : r -> Result(e, r)  
                        
Some instances are automatically made for types - the Zoda equivalent of Haskell's `Show`, `Eq`, `Read`, `Ord`, `Functor`, `Traversible`, `Foldable`, `Typable`, `Generic`, and `Data`. In addition we add some new ones - `Hashable`, which generates unique hashes from values, and a whole array of `Functor2` (corresponds to a `Bifunctor`), `Functor3` (corresponds to a trifunctor), etc., and the same for `Traversable` and `Foldable`. Any of these can be overridden as long as its in the same file as the data definition, either with `Has-Trait` or `Hasn't-Trait`. If any of these can't be automatically generated, they won't be. 

## Synonyms

These are just useful when you want to give a possibly more descriptive name to a type. They can have parameters but cannot be partially applied.

    synonym FilePath = String
    synonym Either(a, b) = Result<a><b>


## Trait and instance declarations

Unlike in default haskell, type classes can have multiple parameters. In this case, we're relating two types by saying one is a collection and another is an element. We also say that `Eq` is a supertrait for `e`, so the elements of any collection are required to have the `Eq` trait. We probably wouldn't do that in real life though because it's useful sometimes to have collections of functions and it's not possible to give functions an `Eq` instance. Also, we've added a functional dependency `c -> e`, by saying that the type of the element is soley determined by the type of the collection. This just means that for any `c : Type given Collection(c)`, there should be a unique type `e` known at compile time.  

    new-trait Collection(e) for c | c -> e 
        precondition 
            Eq(e) 
        where
            insert: c -> e -> c
            member: c -> e -> Bool

    has-trait List(a) has Collection(a) given Eq(a) 
        where
            insert(collection, element) = element.Cons(collection) -- prepend the element
            member(collection, element) = match collection:
                EmptyList  ->: False
                x.Cons(xs) ->:
                    if x == element then:
                        True 
                    else
                        xs.and-recurse(element)

## Tuples

Usage of tuples is typically discouraged but they can sometimes make code simpler. Tuples cannot have more than 64 values.

    {* "Hello", 2 *}: {* String, Int *}

These spaces are required

These can sometimes be useful when you want to return a value from a function. 

    one-greater-one-less : (a : Type) @-> a -> {* a, a *} given a has-trait Addable
    one-greater-one-less(x) = {* x - 1, x + 1 *}

You can retrieve a value from a tuple with pattern matching.

    x.very-bad-double = match x.one-greater-one-less
        {* one-greater, one-less *} -> one-greater + one-less

You can also use `.get1st`, `.get2nd`, etc.

    x.very-bad-double = t.get1st + t.get2nd
        where t = x.one-greater-one-less

You can write a function that inserts a value into a tuple with an `_`.

    list-to-hello-tuple : List(a) -> List({* a, String *})
    list-to-hello-tuple(l) = l.map({* _, "hello" *})





=============== I'm unsure about how all this will work ===============
=================== Need to get batter at purescript ==================


## Records 

http://www.cs.ioc.ee/tfp-icfp-gpce05/tfp-proc/21num.pdf

A record is like a type-safe dictionary, implemented internally using Rows. Duplicate keys are allowed.

    author : { name : String, interests : List<String> }
    author =
        { name = "Phil"
        , interests = ["Functional Programming", "JavaScript"]
        }

Records have no inherent order

    author: { name: String, interests: List(String) }
    author =
        { interests = ["Functional Programming", "JavaScript"]
        , name = "Phil"
        }

As a bit of syntax sugar, if you have a value who's name is the same as a record's field then you can just insert it wholesale

    interests = ["Functional Programming", "JavaScript"]
    
    author : { name : String, interests : List<String> }
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
                  -- i's signature is `{ i : a, ...r } -> a 
    
    3) Removing a value from a record using the magic `without-x` function 

        fst = { i = 4, f = 3.5 }
        fst.without-i     -- { f = 3.5 }
                          -- without-i : { i : a, ...r } -> { ...r }

There's also some convenience syntax sugar for updating values

    fst = { i = 4, f = 3.5 }
    fst{ i = 3 } -- { i = 3, f = 3.5 }

Nested values can be updated with the `.` sugar

    snd = { i = { b = { c = 4 } } }
    snd{ i.b.c = 3 }      -- { i = { b = { c = 3 } } }

To apply a function to a nested value, you can use `$=`

    snd = { i = { b = { c = 4 } } }
    snd{ i.b.c $= _.plus(1) }      -- { i = { b = { c = 5 } } }

You can write a function that takes a record, if you want. (`@` gets the value of a record)

    getName: { name: String } -> String 
    r.getName = r.name

Type variables can take the place of some elements in a record:

    getName : { name : String, ...l } -> String 
    r.getName = r.name

`l` can take the place of literally any other values, so this function works on every record with a `name` field.

You can perform pattern matching on records, as you might expect. Excess values are matched with `...<identifier>`.

    changeNameUnlessNamedSteve : { name : String, ...l } -> Bool
    r.changeNameUnlessNamedSteve = match r 
        { name = "Steve", ...r } -> { name = "Steve", ...r }
        { name = _, ...r }       -> { name = "Mike", ...r }

A field with name `<name>` may be matched just by putting it in there

    changeNameUnlessNamedSteve: { name: String, ...l } -> Bool
    r.changeNameUnlessNamedSteve = match r
        { name = "Steve", title, ...r } -> { name = title <> "Steve", title, ...r }
        { title, ...r }       -> { name = title <> "Mike", title, ...r }

Example of a function that modifies the name field of a record

    addJrSuffix : { name : String, ...r } -> { name : String, ...r }
    addJrSuffix hasName = hasName{name $= _.concat(", Jr.") }
    addJrSuffix { name = "Bob" }                -- { name = "Bob, Jr." }   
    addJrSuffix { age = 42, name = "Gerald" }   -- { age = 42, name = "Gerald, Jr." }   
    addJrSuffix { age = 42, name = "Gerald", name = "Steven" }   -- { age = 42, name = "Gerald, Jr.", name = "Steven" }   

You can give a name to a record type with `synonym`

    synonym Book = { name : String, text : String }
    synonym Author = { name : String, books : List<Book> }

You can unwrap a record into another when defining them

    synonym Name : { name : String }
    synonym Country : { population : Int, ...Name }
    myCountry = { name = "usa", population = 2000 }

A `_` inside a record literal creates a function which takes some values and creates a record with that type

    ["usa", "canada"].map({name : _, population : 2000})

Like how `a` is a variable that can take any type, `{ ...r }` is a record with any (or no) values

    myFunc : { ...r } -> { ...r }
    r.myFunc = r -- the only possible implementation of a function with this type is the identity function.


In general, two record types are equal if they contain the same `key : type` pairs. The order is irrelevant when they keys are all unique. With duplicate keys, the order in the type always matches the order in the literal. Here is an example:

    fst = { x = 2, x = True }      -- { x : Int, x : Bool }
    snd = { x = True, x = 2 }      -- { x : Bool, x : Int }
    thrd = { x = True, x = 2 } :     { x : Bool, x : Int } -- type error
    frth = { x = True, x = False } -- { x : Bool, x : Bool } -- type error
    
    fst.without-x.x = True
    snd.without-x.x = 2
    frth.without-x.x = False

    -- if you had a function of type  `{ x : Int, x : Bool } -> Bool`, you would not be able to pass it `snd`.

Due to the inherent complexity of having records with duplicate keys, I recommend you keep their usage to a minimum. 

Oh and I glossed over this, but records can have constraints
    fst = { x = 2, x = True } : (a : Type) @-> { x : a, x : Bool } given a has-trait Integral


## Corecords

Records:Corecords : Product types:Sum types

While a record must have every value in its type, a corecord can only have one.

    a : {+ b : Int, c : String +}
    a = {+ b = 3 +}

These can also be open, like so:


    {+ name : String, ...v +}

Again, duplicate keys are allowed:

    {+ name : String, name : String, name : Bool +}

This basically means it can have any possible `key: value` pair but definitely might have `name: String`. This seems useless until you use it with functions:

    {+ name : String, ...v +} -> {+ ...v +} 

This function takes a corecord has the possibility of containing a `name : String` pair, and returns one that has "one less possibility" to have a `name : String` pair.

These can be pattern matched with `match`, naturally.

    -- if the corecord is closed, it can easily be pattern-matched exhaustively  

    getFooOrBarLength : {+ foo : String, bar : String +} -> Int 
    r.getFooOrBarLength = match r:
        {+ foo = f +} ->: f.length
        {+ bar = b +} ->: b.length

    -- if it's open, you must have a wildcard element
    getFooOrBarLength : {+ foo : String, bar : String, ...l +} -> Int 
    r.getFooOrBarLength = match r:
        {+ foo = f +} ->: f.length
        {+ bar = b +} ->: b.length
        *             ->: 0 
    
You can have an empty corecord with `{+ +}`, but it is impossible to create a value that occupies this type and the type system knows this.

    getRight : Result<{+ +}, a> -> a
    e.getRight = match e:
        e.Result ->: e 
        -- No need to pattern match Error because we all know that `{+ +}` can't have any values.
        
The snazzy thing is that `match-partial` has special support for these - the values you match against are removed from the type.

## Coercion on rows

if two rows' types are identical except for the names, the `.coerce` function can be used to convert one to another. In the case of multiple k/v pairs with the same name, the order in the type is used.

    {name = "test"}.coerce == {buildingName = "test"}

To make it less confusing, use `<>` in coerce: 

    {name = "test"}.coerce<{name: String}, {buildingName: String}> == {buildingName = "test"}

## The Debuggable Trait

The Debuggable trait only had one function, `createDebugRepresentation`. It takes something with the `Debuggable` trait and outputs a `DebugInfo`. But `createDebugRepresentation` is special because it cannot be called in Production builds (this is verified statically).  

## DebugInfos

`DebugInfo` is a special type. It can be constructed with two constructors - `debugInfo`, which takes anything with the `Debuggable` trait, and `debugEmpty` which just represents no debug information. The constructors for `DebugInfo` are not exposed, so it is impossible to deconstruct. Since the only typeclass that has any functions for `DebugInfo` is `Debuggable`, and `Debuggable`'s functions cannot be used in Production builds, this guarantees that any code which creates, manipulates, or stores `DebugInfo`s cannot affect the runtime behavior of a production build and can therefore be safely optimized away. The utility of this is to create a distinction between values passed around for debugging purposed and values passed around for control flow.

## Modules, packages and imports

There are 3 "tiers" in Zoda. The lowest is values - these are functions and constants. They have metadata like `tiny-doc`, `long-doc`, and `tags`. These all live inside "modules". Values form a DAG of their dependencies.

A module is just a collection of values. Every file is a module. Like values, modules can depend on each other via `import` and form a DAG. If a module is named `main`, it must be a "root" module and cannot be imported by any other module. The name of a module is always the name of the file (without the `.zoda` extension), with optional descriptive words at the end seperated by `-`. If a module is named `main`, it must have a value named `main` of type `IO a`. Like functions, they have "tiny-doc" and "long-doc". This is what a module might look like:

    -- adding-utils.zoda
    module adding-utils `Collection of utilities related to adding values`
      authors 
        Andre Popovitch
      user-doc 
        These utilities are part of the greater "adding" library. 
        They're useful in all situations where you need to add something.
           
        -- maybe include more info here on when to use these?
        If you need to add 3, check out the `plus-3` function. Other useful 
        functions include `plus-4` and `plus-5`.           

        Examples 

        >> 3.plus-4 
        -> 7

        >> 3.plus-5
        -> 8
      devl-doc
        Research is ongoing whether a `x.plus-6` function is possible. Contributions would be welcomed!
      importing
        { plus } from generic-plus           -- import the "plus" function from the module "generic-plus".
        negatory                           -- import the "negatory" package and everything inside it (this one has a `negation` module inside of it)
        string-concatenation               -- import everything exported by the main module in the "string-concatenation" package
      exporting
        plus, plus-3, plus-4, plus-5, plus-negative -- re-export the imported "plus" function
                  

    plus-0(x) = x -- not exported because it's useless
    plus-3(x) = x.plus(3) -- these would typically have their own tests, tiny-docs, user-docs, and devl-docs, but I've left them off for the sake of brevity.
    plus-4(x) = x.plus(4) 
    plus-5(x) = x.plus(5) 
    plus-negative(x, y) = x.plus(negation:negate(y))         -- namespacing is done with `:`. `:` is actually just `.` that only works for functions of one argument.


When executing a package, execution begins at the `main` function inside the `main` module. More specifically, it just executes whatever actions the `main` module returns. Every Zoda package has a `main` module. It cannot be imported by any other module, and anything it exports is included if you just import the name of the package. The only things available to users of you package are things exported by the main module - for obvious reasons you should not export multiple things with the same name. The `devl-doc` of your main module serves as a guide to new contributors to how they should look at and edit the whole package. It is *not* intended to be a high level description of your package for end-users - that should be the `introduction` chapter of the book for your package. (a `readme.md` will be automatically created from the book, more on this later).

A package is always contained entirely within one folder. It also always has a `book` directory and a `zoda.toml` file. The book directory contains all the documentation about the package that does not make sense to be stored in the source files. The `zoda.toml` file contains information about what other packages this package depends on, and what modules it exports. 

## Notes

Notes are a special type of comment in Zoda. The syntax looks like this:

```
note [Equality-constrained types]
  The type `forall ab. (a : [b]) => blah`
  is encoded like this:

     ForAllTy (a:*) $ ForAllTy (b:*) $
     FunTy (TyConApp (~) [a, [b]]) $
     blah
```

Then, inside any `devl-doc`, `user-doc`, or comment, one can write `[Equality-constrained types]`. This generates a link in the IDE that allows you to easily navigate to the note (view it inline?) and navigate back. Notes are scoped to a package. Multiple notes with the same name result in a compilation error. Markdown-like formatting is allowed within notes.



# Experiments

add(x: Int, y: Int): Int = x + y
id(@a: Type, x: a): a = x
get-index(@e: Type, @n: Nat, v: Vector(n, e), index: Nat): a = v.access(index)