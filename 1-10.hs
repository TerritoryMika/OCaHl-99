-- 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) :
last' :: [x] -> Maybe x
last' [] = Nothing
last' xs = Just $ last xs

-- 2. Find the last but one (last and penultimate) elements of a list. (easy) : 
last_two' :: [x] -> Maybe [x]
last_two' xs = if (length xs) < 2
                then Nothing
                else Just $ drop ((length xs) - 2) xs

-- 3. Find the K'th element of a list. (easy) : 
at' :: Int -> [x] -> Maybe x
at' n xs = if (length xs) < n
                then Nothing
                else Just $ xs !! n

-- 4. Find the number of elements of a list. (easy) :
length' xs = length xs

-- 5. Reverse a list. (easy) :
rev' = reverse

-- 6. Find out whether a list is a palindrome. (easy) :
is_palindrome' xs = xs == reverse xs

-- 7. Flatten a nested list structure. (medium) :
flatten' xs = foldr (++) [] xs

-- 8. Eliminate consecutive duplicates of list elements. (medium) :
compress' xs = foldr (\y ys -> if elem y ys then ys else y : ys) [] xs

-- 9. Pack consecutive duplicates of list elements into sublists. (medium) :
pack' :: Eq a => [a] -> [[a]]
pack' [] = []
pack' xss@(x:xs) = filter (== x) xss : pack' (filter (/= x) xss)

-- 10. Run-length encoding of a list. (easy)
encode' :: Eq a => [a] -> [(Int, a)]
encode' [] = []
encode' xss@(x:xs) = (length matched, head matched) : encode' (filter (/= x) xss)
    where matched = filter (== x) xss