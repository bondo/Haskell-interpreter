module AssocList where
    
import Data.List (deleteBy)

remove :: Eq a => [(a,b)] -> a -> [(a,b)]
remove []           _   = []
remove (x@(k,_):xs) key = if k == key then xs else x : remove xs key

aLookup :: Eq a => [(a,b)] -> a -> b -> b
aLookup alist find defval = maybe defval id $ lookup find alist
