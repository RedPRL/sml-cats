signature CATEGORY =
sig
  type ('a, 'b) hom
  val id : ('a, 'a) hom
  val cmp : ('b, 'c) hom * ('a, 'b) hom -> ('a, 'c) hom
end

signature FUNCTOR =
sig
  type 'a t
  val map : ('a -> 'b) -> 'a t -> 'b t
end

signature APPLICATIVE =
sig
  include FUNCTOR

  val pure : 'a -> 'a t
  val ap : ('a -> 'b) t -> 'a t -> 'b t
end

signature MONAD =
sig
  type 'a t

  val ret : 'a -> 'a t
  val bind : ('a -> 'b t) -> 'a t -> 'b t
end

signature TRAVERSABLE =
sig
  type 'a t
  type 'a f
  val traverse : ('a -> 'b f) -> 'a t -> 'b t f
end

signature TRAVERSABLE_UTIL =
sig
  include TRAVERSABLE
  val dist : 'a f t -> 'a t f
end

signature FOLDABLE =
sig
  type 'a t
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
end

signature MONOID =
sig
  type t
  val one : t
  val mul : t * t -> t
end

