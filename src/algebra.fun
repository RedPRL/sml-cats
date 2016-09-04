functor MonadApplicative (M : MONAD) : APPLICATIVE =
struct
  open M

  val pure = ret
  fun map f = bind (ret o f)
  fun ap mf mx = bind (fn f => map f mx) mf
end

functor MonadNotation (M : MONAD) =
struct
  fun >>= (x, f) = M.bind f x
  fun >> m n = m >>= (fn _ => n)
end

functor FunctorNotation (F : FUNCTOR) =
struct
  fun <$> (f, x) = F.map f x
end

functor ApplicativeNotation (F : APPLICATIVE) =
struct
  local
    structure FunctorNotation = FunctorNotation (F)
  in
    open FunctorNotation
    fun <*> (f, x) = F.ap f x
  end
end

functor TraversableUtil (T : TRAVERSABLE) : TRAVERSABLE_UTIL =
struct
  open T

  fun dist t = traverse (fn x => x) t
end

structure IdMonad : MONAD =
struct
  type 'a t = 'a

  fun ret x = x
  fun bind f x = f x
end

structure ListMonad : MONAD =
struct
  type 'a t = 'a list

  fun ret x = [x]
  fun bind f = List.foldl (fn (x, ys) => ys @ f x) []
end

structure OptionMonad : MONAD =
struct
  type 'a t = 'a option
  fun ret x = SOME x
  fun bind f =
    fn NONE => NONE
     | SOME x => f x
end

functor ListMonoid (type t) : MONOID =
struct
  type t = t list

  val one = []
  val mul = op@
end

functor OptionTraversable (F : APPLICATIVE) : TRAVERSABLE =
struct
  type 'a t = 'a option
  type 'a f = 'a F.t

  fun traverse f =
    fn NONE => F.pure NONE
     | SOME x => F.map SOME (f x)
end

functor ListTraversable (F : APPLICATIVE) : TRAVERSABLE =
struct
  type 'a t = 'a list
  type 'a f = 'a F.t

  structure Notation = ApplicativeNotation (F)
  open Notation
  infixr 2 <$>
  infixr 1 <*>

  fun cons x xs = x :: xs

  fun traverse f =
    List.foldr
      (fn (x, ys) => F.map cons (f x) <*> ys)
      (F.pure [])
end

structure FunctionCategory : CATEGORY =
struct
  type ('a, 'b) hom = 'a -> 'b

  fun id x = x
  val cmp = op o
end


functor OppositeCategory (C : CATEGORY) : CATEGORY =
struct
  type ('a, 'b) hom = ('b, 'a) C.hom
  val id = C.id
  fun cmp (f, g) = C.cmp (g, f)
end

functor CategoryFoldMap (structure C : CATEGORY and F : FOLDABLE) =
struct
  fun foldMap (f : 'a -> ('b, 'b) C.hom) =
    F.foldr (fn (a, phi) => C.cmp (f a, phi)) C.id
end
