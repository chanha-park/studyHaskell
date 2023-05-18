{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -Wextra #-}

-- https://apotheca.io/articles/Birecursion-Schemes.html

import Control.Arrow ((<<<))
import Control.Monad ((<=<))
import Data.Bifunctor (Bifunctor, bimap)
import Data.Bitraversable (Bitraversable, bitraverse)
import Data.Kind (Type)

type family Base t :: Type -> Type

class Functor (Base t) => Recursive t where
  project :: t -> Base t t

class Functor (Base t) => Corecursive t where
  embed :: Base t t -> t

type Iso a b = (Recursive a, Corecursive b, Base a ~ Base b)

hylo :: (Functor f) => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = h where h = alg <<< fmap h <<< coalg

-- -- alternative representation
-- hylo alg coalg = h where h = alg . fmap h . coalg
-- hylo up down = down >>> fmap (hylo up down) >>> up

cata :: (Recursive t) => (Base t a -> a) -> t -> a
cata alg = hylo alg project

ana :: (Corecursive t) => (a -> Base t a) -> a -> t
ana = hylo embed

hyloM :: (Monad m, Traversable t) => (t b -> m b) -> (a -> m (t a)) -> a -> m b
hyloM alg coalg = h
  where
    h = alg <=< traverse h <=< coalg

iso :: (Iso s t) => s -> t
iso = hylo embed project

trans :: (Recursive s, Corecursive t) => (forall a. Base s a -> Base t a) -> s -> t
trans f = hylo (embed . f) project

type family Dibase (t :: Type -> Type) :: Type -> Type -> Type

class (Bifunctor (Dibase f)) => Direcursive f where
  diproject :: f a -> Dibase f a (f a)

class (Bifunctor (Dibase f)) => Codirecursive f where
  diembed :: Dibase f a (f a) -> f a

type Diiso a b = (Direcursive a, Codirecursive b, Dibase a ~ Dibase b)

dihylo :: (Bifunctor f) => (a -> b) -> (f b d -> d) -> (c -> f a c) -> c -> d
dihylo f alg coalg = h where h = alg . bimap f h . coalg

dicata :: (Direcursive f) => (a -> b) -> (Dibase f b c -> c) -> f a -> c
dicata f alg = dihylo f alg diproject

diana :: (Codirecursive f) => (a -> b) -> (c -> Dibase f a c) -> c -> f b
diana f coalg = dihylo f diembed coalg

-- hylo'' = dihylo id
-- cata'' = dicata id
-- ana'' = diana id

diprojectM :: (Monad m, Direcursive f) => f a -> m (Dibase f a (f a))
diprojectM = pure . diproject

diembedM :: (Monad m, Codirecursive f) => Dibase f a (f a) -> m (f a)
diembedM = pure . diembed

dicataM ::
  (Monad m, Direcursive f, Bitraversable (Dibase f)) =>
  (a -> m b) ->
  (Dibase f b c -> m c) ->
  f a ->
  m c
dicataM f alg = dihyloM f alg diprojectM

dianaM ::
  (Monad m, Codirecursive f, Bitraversable (Dibase f)) =>
  (a -> m b) ->
  (c -> m (Dibase f a c)) ->
  c ->
  m (f b)
dianaM f coalg = dihyloM f diembedM coalg

dihyloM ::
  (Monad m, Bitraversable f) =>
  (a -> m b) ->
  (f b d -> m d) ->
  (c -> m (f a c)) ->
  c ->
  m d
dihyloM f alg coalg = h where h = alg <=< bitraverse f h <=< coalg

dicataA :: (Applicative f, Direcursive t, Bitraversable (Dibase t)) => (a -> f b) -> (Dibase t b c -> c) -> t a -> f c
dicataA f alg = dihyloA f alg diproject

dianaA :: (Applicative f, Codirecursive t, Bitraversable (Dibase t)) => (a -> f b) -> (c -> Dibase t a c) -> c -> f (t b)
dianaA f coalg = dihyloA f diembed coalg

dihyloA :: (Applicative f, Bitraversable t) => (a -> f c) -> (t c d -> d) -> (b -> t a b) -> b -> f d
dihyloA f alg coalg = h where h = fmap alg . bitraverse f h . coalg

diiso :: (Diiso s t) => (a -> b) -> s a -> t b
diiso f = dihylo f diembed diproject

ditrans ::
  (Direcursive s, Codirecursive t) =>
  (a -> b) ->
  (forall c. Dibase s b c -> Dibase t b c) ->
  s a ->
  t b
ditrans f g = dihylo f (diembed . g) diproject

class (Functor f, Direcursive f, Codirecursive f) => Difunctor f where
  dimap :: (a -> b) -> (forall c. Dibase f b c -> Dibase g b c) -> f a -> g b
  -- dimap = ditrans
  lmap :: (a -> b) -> f a -> f b
  -- lmap = diiso
  rmap :: (forall b. Dibase f a b -> Dibase g a b) -> f a -> g a
  -- rmap = ditrans id
