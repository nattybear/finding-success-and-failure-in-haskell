> Your task right now is to query the kinds of the following types and decide wheter or not they ***could*** be monads. That is, do they have the same arity as `Maybe` or `Either String`.

1. `String`

```haskell
String :: *
```

`String` cannot be monads.

2. `[]`

```haskell
[] :: * -> *
```

`[]` can be monads.

3. `(,)`

```haskell
(,) :: * -> * -> *
```

`(,)` cannot be monads.

4. `(,) Int`

```haskell
(,) Int :: * -> *
```

`(,) Int` can be monads.

5. `data Pair a = Pair a a`

```haskell
Pair a :: * -> *
```

`Pair a` can be monads.
