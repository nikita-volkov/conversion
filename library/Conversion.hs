module Conversion (Conversion(..)) where

import BasePrelude


-- |
-- A type-class, 
-- which provides a non-partial conversion function from a value of type @a@ 
-- to a value of type @b@.
class Conversion a b where
  convert :: a -> b


-- |
-- Equivalent to 'atomically'.
instance Conversion (STM a) (IO a) where
  {-# INLINE convert #-}
  convert = atomically

-- |
-- Converts to any 'Alternative' type ('Maybe', list).
instance Alternative f => Conversion (Either a b) (f b) where
  {-# INLINE convert #-}
  convert = either (const empty) pure

-- |
-- Checks whether the value is 'Right'.
instance Conversion (Either a b) Bool where
  {-# INLINE convert #-}
  convert = either (const False) (const True)

-- |
-- Converts to any 'Alternative' type ('Either', list).
instance Alternative f => Conversion (Maybe a) (f a) where
  {-# INLINE convert #-}
  convert = maybe empty pure

-- |
-- Checks whether the value is 'Just'.
instance Conversion (Maybe a) Bool where
  {-# INLINE convert #-}
  convert = maybe False (const True)

-- |
-- Converts into a function, which extracts the value, 
-- given a default value in the 'Nothing' case.
-- 
-- Equivalent to 'fromMaybe'.
instance Conversion (Maybe a) (a -> a) where
  {-# INLINE convert #-}
  convert = flip fromMaybe

-- |
-- Gets the head of a list.
instance Alternative f => Conversion [a] (f a) where
  {-# INLINABLE convert #-}
  convert = \case [] -> empty; a : _ -> pure a

-- |
-- Checks whether the list is not empty.
instance Conversion [a] Bool where
  {-# INLINE convert #-}
  convert = null

-- |
-- Equivalent to 'catMaybes'.
instance Conversion [Maybe a] [a] where
  {-# INLINE convert #-}
  convert = catMaybes


instance Conversion Int Integer where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Int (f Int8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int (f Int16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int (f Int32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Conversion Int Int64 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Int (f Word) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int (f Word8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int (f Word16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int (f Word32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int (f Word64) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral


instance Conversion Int8 Integer where
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Int8 Int where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Int8 Int16 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Int8 Int32 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Int8 Int64 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Int8 (f Word) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int8 (f Word8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int8 (f Word16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int8 (f Word32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int8 (f Word64) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral


instance Conversion Int16 Integer where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Int16 Int where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Int16 (f Int8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Conversion Int16 Int32 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Int16 Int64 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Int16 (f Word) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int16 (f Word8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int16 (f Word16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int16 (f Word32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int16 (f Word64) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral


instance Conversion Int32 Integer where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Int32 Int where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Int32 (f Int8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int32 (f Int16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Conversion Int32 Int64 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Int32 (f Word) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int32 (f Word8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int32 (f Word16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int32 (f Word32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int32 (f Word64) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral


instance Conversion Int64 Integer where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Int64 (f Int) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int64 (f Int8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int64 (f Int16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int64 (f Int32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int64 (f Word) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int64 (f Word8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int64 (f Word16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int64 (f Word32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Int64 (f Word64) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral


instance Conversion Word Integer where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Word (f Int) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word (f Int8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word (f Int16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word (f Int32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word (f Int64) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word (f Word8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word (f Word16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word (f Word32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Conversion Word Word64 where 
  {-# INLINE convert #-}
  convert = fromIntegral


instance Conversion Word8 Integer where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Word8 Int where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Word8 (f Int8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Conversion Word8 Int16 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Word8 Int32 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Word8 Int64 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Word8 Word where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Word8 Word16 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Word8 Word32 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Word8 Word64 where 
  {-# INLINE convert #-}
  convert = fromIntegral


instance Conversion Word16 Integer where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Word16 Int where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Word16 (f Int8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word16 (f Int16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Conversion Word16 Int32 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Word16 Int64 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Word16 (f Word) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word16 (f Word8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Conversion Word16 Word32 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Word16 Word64 where 
  {-# INLINE convert #-}
  convert = fromIntegral


instance Conversion Word32 Integer where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Word32 (f Int) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word32 (f Int8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word32 (f Int16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word32 (f Int32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Conversion Word32 Int64 where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Conversion Word32 Word where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Word32 (f Word8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word32 (f Word16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Conversion Word32 Word64 where 
  {-# INLINE convert #-}
  convert = fromIntegral


instance Conversion Word64 Integer where 
  {-# INLINE convert #-}
  convert = fromIntegral

instance Alternative f => Conversion Word64 (f Int) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word64 (f Int8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word64 (f Int16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word64 (f Int32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word64 (f Int64) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word64 (f Word) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word64 (f Word8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word64 (f Word16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Word64 (f Word32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral


instance Alternative f => Conversion Integer (f Int) where
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Integer (f Int8) where
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Integer (f Int16) where
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Integer (f Int32) where
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Integer (f Int64) where
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Integer (f Word) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Integer (f Word8) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Integer (f Word16) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Integer (f Word32) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral

instance Alternative f => Conversion Integer (f Word64) where 
  {-# INLINE convert #-}
  convert = checkedFromIntegral


instance Conversion Float Rational where
  {-# INLINE convert #-}
  convert = realToFrac

instance Conversion Float Double where
  {-# INLINE convert #-}
  convert = realToFrac


instance Conversion Double Rational where
  {-# INLINE convert #-}
  convert = realToFrac


{-# INLINABLE isomorphicallyChecked #-}
isomorphicallyChecked :: (Alternative f, Conversion b a, Eq a) => (a -> b) -> a -> f b
isomorphicallyChecked =
  \f a -> f a & \b -> if a == convert b then pure b else empty

{-# INLINABLE checkedFromIntegral #-}
checkedFromIntegral :: (Alternative f, Integral a, Integral b) => a -> f b
checkedFromIntegral =
  \a -> fromIntegral a & \b -> if fromIntegral b == a then pure b else empty
