{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts, FlexibleInstances, ScopedTypeVariables #-}

module GenericAST where 

import GHC.Generics
import Types


class Generalise b where 
    toGeneric :: b -> GenericAST

    default toGeneric :: (Generic b, GGeneralise (Rep b)) => b -> GenericAST
    toGeneric = head . gtoGeneric . from

class GGeneralise f where 
    gtoGeneric :: f a -> [GenericAST]

    
-- | Unit: User for constructors without arguments 
instance GGeneralise U1 where 
    gtoGeneric U1 = []


-- | Constants, additional parameters and recursion of kind *
instance (GGeneralise a, GGeneralise b) => GGeneralise (a :*: b) where
    gtoGeneric (a :*: b) = gtoGeneric a ++ gtoGeneric b 


-- | Meta-information (constructor names, etc.)
instance (GGeneralise a, GGeneralise b) => GGeneralise (a :+: b) where
    gtoGeneric (L1 x) = gtoGeneric x
    gtoGeneric (R1 x) = gtoGeneric x


-- Datatype
instance (GGeneralise f) => GGeneralise (M1 D c f) where
    gtoGeneric (M1 x) = gtoGeneric x


-- Constructor Metadata
instance (GGeneralise f, Constructor c) => GGeneralise (M1 C c f) where
    gtoGeneric (M1 x) = [GenericAST nodeName children]
        where 
            nodeName = conName (undefined :: t c f a)
            children = gtoGeneric x


-- Selector Metadata
instance (GGeneralise f, Selector c) => GGeneralise (M1 S c f) where
    gtoGeneric (M1 x) = gtoGeneric x


-- Constructor Paramater
instance (Generalise f) => GGeneralise (K1 i f) where
    gtoGeneric (K1 x) = [toGeneric x]


-- Instances for primitive types 
instance Generalise Int where
    toGeneric i = GenericAST (show i) []

instance Generalise Bool where 
        toGeneric b = GenericAST (show b) []

instance Generalise String where 
    toGeneric s = GenericAST s [] 

instance Generalise Char where 
    toGeneric c = GenericAST [c] []