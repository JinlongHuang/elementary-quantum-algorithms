doubleMe x = x + x 

doubleUs x y = doubleMe x  + doubleMe y

doubleSmallNumber x = if x > 100
                         then x
                         else x*2
doubleSmallNumber' x = (if x>100 then x else x*2) +1

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

first :: (a,b,c) -> a  
first (x,_,_) = x  

second :: (a,b,c) -> b  
second (_,y,_) = y  

third :: (a,b,c) -> c  
third (_,_,z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight,you emo,you!"  
    | bmi <= normal = "You're supposedly normal. Pffft,I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight,fatty!"  
    | otherwise     = "You're a whale,congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs)
  let smallSorted = [a | a <- xs, a <= x]
      biggerSorted = [a | a <- xs, a > x]
  in smallSorted ++ [x] ++ biggerSorted
