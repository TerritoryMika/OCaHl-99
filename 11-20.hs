-- 11. Modified run-length encoding. (easy) :
encode2' :: Eq a => [a] -> [Either (Int, a) a]
encode2' [] = []
encode2' xss@(x:xs) = if length matched > 1
                       then Left (length matched, head matched) : encode2' (filter (/= x) xss)
                       else Right (head matched) : encode2' (filter (/= x) xss)
                        where matched = filter (== x) xss

-- 12. Decode a run-length encoded list. (medium) :
decode' :: Eq a => [Either (Int, a) a] -> [a]
decode' [] = []
decode' ((Right x):xs) = x : decode' xs
decode' ((Left  x):xs) = replicate (fst x) (snd x) ++ decode' xs

-- 13. Run-length encoding of a list (direct solution). (medium) :
encode3' :: Eq a => [a] -> [Either (Int, a) a]
encode3' [] = []
encode3' xss@(x:xs) = if matched > 1
                       then Left (matched, x) : encode3' (filter (/= x) xss)
                       else Right x : encode3' (filter (/= x) xss)
                        where matched = foldr (\y acc -> if y == x then acc + 1 else acc) 0 xss

-- 14. Duplicate the elements of a list. (easy) :
duplicate' :: [a] -> [a]
duplicate' [] = []
duplicate' (x:xs) = x : x : duplicate' xs

-- 15. Replicate the elements of a list a given number of times. (medium) :
replicate' :: [a] -> Int -> [a]
replicate' [] _ = []
replicate' (x:xs) n = replicate n x ++ replicate' xs n

-- 16. Drop every N'th element from a list. (medium) :
drop' :: [a] -> Int -> [a]
drop' [] _ = []
drop' xs n = take (n-1) xs ++ drop' (drop n xs) n

-- 17. Split a list into two parts; the length of the first part is given. (easy) :
split' xs n = take n xs : drop n xs : []

-- 18. Extract a slice from a list. (medium) :
slice' xs i k = take (k - i + 1) $ drop i xs

-- 19. Rotate a list N places to the left. (medium) :
rotate' xs n = if n > 0
                then drop n xs ++ take n xs
                else drop nx xs ++ take nx xs
                 where nx = (length xs) + n

-- 20. Remove the K'th element from a list. (easy) :
remove_at' n xs = take n xs ++ drop (n + 1) xs