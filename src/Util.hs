module Util (note, guard', (?:), (.:.), (<$$>)) where

note :: a -> Maybe b -> Either a b
note a Nothing = Left a
note _ (Just b) = Right b

(?:) :: Maybe a -> a -> a
Nothing ?: a = a
(Just a) ?: _ = a

guard' :: Bool -> a -> Either a ()
guard' False = Left
guard' True = const $ Right ()

(.:.) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:.) = (.) . (.)

(<$$>) :: (Functor f0, Functor f1) => (a -> b) -> f0 (f1 a) -> f0 (f1 b)
(<$$>) = fmap . fmap
