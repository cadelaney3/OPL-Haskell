-- Chris Delaney
-- assignment 9

replaceInList :: Eq a => [a] -> a -> a -> [a]
replaceInList lst target replacement = 
 if lst == [] then lst
 else if (head lst) == target then replacement:replaceInList (tail lst) target replacement
 else (head lst):replaceInList (tail lst) target replacement

myReverse :: Eq a => [a] -> [a]
myReverse lst = 
 if lst == [] then lst
 else (last lst):myReverse (init lst)

myMaximum :: Ord a => [a] -> a
myMaximum lst = 
 if lst == [] then error "no maximum in empty list"
 else if (tail lst) == [] then (head lst)
 else if (head lst) > head (tail lst) then myMaximum ((head lst):(tail (tail lst)))
 else myMaximum (tail lst)

everyOther :: Eq a => [a] -> [a]
everyOther lst 
 | lst == [] = error "empty list"
 | tail lst == [] = lst
 | tail (tail lst) == [] = init lst
 | otherwise = head lst : everyOther (tail (tail lst))

rotateHelper :: Eq a => [a] -> [a] -> [a]
rotateHelper lst lst2
 | lst == [] = lst2
 | otherwise = rotateHelper (tail lst) ((head lst) : lst2)

rotate :: Eq a => [a] -> [a]
rotate lst
 | lst == [] = error "empty list"
 | otherwise = rotateHelper (rotateHelper (tail lst) []) myList 
 where myList = head lst : []

