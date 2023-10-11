
module midterm
import StdEnv

//// Disarium number
/*1.
Given a positive integer number, check if the given number
is a Disarium number or not.
A Disarium number is a number defined by the following:
Sum of its digits powered with their respective position
is equal to the original number.
Example: 135 is a Disarium number, 1^1+3^2+5^3 = 135
*/

toDigits :: Int -> [Int]
toDigits x
| x < 10 = [x]
= toDigits (x / 10) ++ [x rem 10]

isDisariumNum :: Int -> Bool
isDisariumNum a = sum [x^i \ x <- (toDigits a) & i <- [1..]] == a

//Start = isDisariumNum 135 // True
//Start = isDisariumNum 598 // True
//Start = isDisariumNum 518 // True
//Start = isDisariumNum 220 // False
//Start = isDisariumNum 110 // False

//// Harshad numbers
/*2.
Given a list of positive integer numbers, return a list that
contains the Harshad numbers of the list.
A Harshad number is an integer that is divisible by the sum of
its digits when written in that base.
Examples:
200 is a Harshad Nr, the sum of digits 2+0+0=2 and 200 is divisible by 2.
171 is a Harshad Nr, the sum of digits 1+7+1=9 and 171 is divisible by 9.
*/

isHarshadNum :: Int -> Bool
isHarshadNum x = x rem (sum (toDigits x)) == 0

harshadNums :: [Int] -> [Int]
harshadNums list = filter (isHarshadNum) list

//Start = harshadNums ([8, 9, 10, 12, 18, 20, 21, 24, 27, 30] ++ [13..17]) // [8, 9, 10, 12, 18, 20, 21, 24, 27, 30]
//Start = harshadNums ([31..35] ++ [36, 17,40, 42, 45, 13, 48, 50, 54, 11, 60, 63]) // [36, 40, 42, 45, 48, 50, 54, 60, 63]
//Start = harshadNums [] // []

//// Leader numbers of a list
/*3.
Given a list of integer numbers, return all the leaders in the list.
A number is leader if it is strictly greater than all the elements
to its right side in a list.
Example: [10,9,14,23,15,0,9] -> [23,15,9]
23 is greater than all the numbers to its right 15,0,9.
15 is greater than all the numbers to its right 0,9.
9 there are no numbers in its right.
*/

isLeader :: Int [Int] -> Bool
isLeader x list = and [a < x \ a <- list]

leaders :: [Int] -> [Int]
leaders [] = []
leaders [x:xs]
| isLeader x xs = [x] ++ leaders xs
= leaders xs

//Start = leaders [10,9,14,23,15,0,9] // [23,15,9]
//Start = leaders [1..10] // [10]
//Start = leaders [10,9..1] // [10,9,8,7,6,5,4,3,2,1]
//Start = leaders [7,8,10,9,5,3,6,4] // [10,9,6,4]
//Start = leaders [] // []

//// Replacing
/*4.
Given the list and a number K, remove all numbers that are divisible by K
and replace all other with reminder by K. Return resulting list.
Example: [1,3,8,6,2], K=3 -> [1,2,2]
3 and 6 are removed as they are divisible by K.
1,8,2 are replaced with 1, 2, 2
*/

filteredRem :: Int [Int] -> [Int]
filteredRem k list = [x rem k \ x <- list | not (x rem k == 0)]

//Start = filteredRem 3 [1,3,8,6,2] // [1,2,2]
//Start = filteredRem 5 [5,10,30] // []
//Start = filteredRem 2 [2,8,3,4,1] // [1,1]
//Start = filteredRem 100 [20,17] // [20,17]

//// GoodNumbers
/*5.
Write a function that takes a list as an argument and counts how many numbers are:
greater or equal to 10 AND less or equal to 99 AND divisible by 3.
*/

countGoodNums :: [Int] -> Int
countGoodNums list = length [x \ x<-list | x >=10 && x<=99 && x rem 3==0]

//Start = countGoodNums [1,12,10,99] // 2
//Start = countGoodNums [12,15,30,33,39,96,99] // 7
//Start = countGoodNums [9, 10, 100, 102, 105] // 0
//Start = countGoodNums [] // 0

//// Valid Triangles
/*6.
Given a list of tuples, each with 3 numbers.
For each tuple check if these 3 numbers can be used as sides of a triangle,
replace the tuple either with True or False.
3 numbers can be sides of triangles if each pair's
sum is greater than the remaining 3rd number.
A number cannot be a side if it is negative or 0.
*/

isValid :: (Int, Int, Int) -> Bool
isValid (a,b,c)
| a>0 && b>0 && c>0 = a+b>c && b+c>a && a+c>b
= False

validTriangles :: [(Int,Int,Int)] -> [Bool]
validTriangles list = map isValid list

//Start = validTriangles [] // []
//Start = validTriangles [(3,3,3), (2,4,5), (4,2,5), (3,3,10)] // [True, True, True, False]
//Start = validTriangles [(8,2,4), (3,10,3), (1,2,3)] // [False, False, False]
//Start = validTriangles [(10,8,3), (-10,4,2)] // [True, False]

//// Replicate
/*7.
Given a list of tuples, where each tuple contains a string and a number N.
For each tuple generate a list that contains N copies of the given string.
For example, the tuple ("ab", 3) should be replaced with ["ab","ab","ab"]
For negative number N generate empty list.
*/

stringCopy :: [(String,Int)] -> [[String]]
stringCopy x = [[a \ n <-[1..b]] \ (a,b)<-x]

stringCopy2 x = [ repeatn b a \ (a,b)<-x]

stringCopy3 x = [ repeatn (snd y) (fst y) \ y <-x]

f u v = [u \ _ <-[1..v]]

stringCopy4 x = [ f a b \ (a,b)<-x]

l = [11 \ x <- [1..11]]
//Start = l
//Start = repeatn 11 11

//Start = stringCopy4 [("X",3),("AA",2)] // [["X","X","X"],["AA","AA"]]
//Start = stringCopy [("Clean", 1),("?!",0),("Empty",-1)] // [["Clean"],[],[]]
//Start = stringCopy [] // []

//// Integers' insertion
/*8.
Given two integers, insert the second integer to the first one.
After each digit considered in the first integer,
insert a digit from the second integer.
Both given numbers are of equal length.
Example: 123 321 -> 132231
13 13 -> 1133
*/

toInteger :: [Int] -> Int
toInteger ls = foldl (\ a b = a * 10 + b) (hd ls) (tl ls)

//Start = toInteger [1,2,3,4,5]

//foldl 1 [2,3,4] ((1*10 + 2)*10 + 3)*10 + 4 = 1234

toInteger2:: [Int] -> Int
toInteger2 list = toInt (foldr (+++) "" (map toString list)) // [1,2,3,4] ["1", "2", "3", "4"] -> "1234" 1234

// "1" +++ ("2" +++ ("3" +++ ("4" +++ ""))) "1234" 1234

//foldr (+) 0 [1,2,3] foldr (\ a b = a+b) 0 [1,2,3]

toInteger3:: [Int] -> Int
toInteger3 list = sum [x*10^i \ x <- list & i <- [((length list)-1), ((length list)-2) .. 0]]

//Start = toInteger3 [1,2,3,4,5]

//[1,2,3,4] 1234 = 110^3 + 210^2 + 310^1 + 410^0

intInsertion :: Int Int -> Int
intInsertion n1 n2 = toInteger2 (insAux ln1 ln2)
where
ln1 = toDigits n1
ln2 = toDigits n2

insAux :: [Int] [Int] -> [Int]
insAux [] [] = []
insAux ls1 ls2 = [hd ls1, hd ls2] ++ insAux (tl ls1) (tl ls2)

//Start = intInsertion 123 123 // 112233
//Start = intInsertion 123 321 // 132231
//Start = intInsertion 13 13 // 1133
//Start = intInsertion 1 2 // 12
//Start = intInsertion 2 1 // 21
//Start = intInsertion 1234 4321

//// Failed-passed students
/*9.
Given list of tuples and an integer value representing the 'pass_marks',
each tuple represents a student (name,marks), write a function which
groups the students into two categories based on their marks obtained in a test.
The function should return a tuple containing the list of the students who failed,
and the list of the students who passed.
Example:
list: [("Ramesh",23), ("Vivek",40), ("Harsh",88), ("Mohammad",60)], pass_marks: 30
Output: ([("Ramesh",23)],[("Vivek",40), ("Harsh",88), ("Mohammad",60)])
--failed-- -------------passed------------------------
'Ramesh' failed as his marks 23 are less than the given number 30, all others passed.
*/

group_by_marks :: [(String, Int)] Int -> ([(String,Int)], [(String,Int)])
group_by_marks list mark = (failed, passed)
where
failed = filter (\ x = snd x < mark) list
passed = filter (\ x = snd x >= mark) list

//Start = group_by_marks [("Ramesh",23), ("Vivek",40), ("Harsh",88), ("Mohammad",60)] 30
// ([("Ramesh",23)],[("Vivek",40),("Harsh",88),("Mohammad",60)])
//Start = group_by_marks [("Ramesh",50),("Vivek",20),("Harsh",10),("Abdullah",90),("Mohammed",30),("Ahmed",0),("Othman",70)] 50
// ([("Vivek",20),("Harsh",10),("Mohammed",30),("Ahmed",0)],[("Ramesh",50),("Abdullah",90),("Othman",70)])
//Start = group_by_marks [] 1 // ([],[])

//// Ciphering
/*10.
Given a list of characters, extract all the vowels and count them.
After that, cipher the list of characters by that count.
Ciphering here means just shift the character by that count.
English vowels are: a, e, i, o, and u.
Example: let's assume that the vowels' count is 2, then:
'a' + 2 = 'c' ... Here we ciphered 'a' into 'c'
'c' + 2 = 'e' ... We did the same as above
For the input ['m', 'o', 'h', 'i','d','o'] count of vowels is 3 o,i,o
Cipher of the list: ['m', 'o', 'h', 'i','d','o']->['p','r','k','l','g','r']
*/

isVowel :: Char -> Bool
isVowel ch = isMember ch ['a','e','i','o','u']

cipherList :: [Char] -> [Char]
cipherList list = map (\ x = toChar (fromChar x + key)) list // toInt
where
key = length (filter isVowel list)

//Start = cipherList ['m', 'o', 'h', 'i','d','o'] // ['p','r','k','l','g','r']
//Start = cipherList ['t', 'a', 'r', 'i', 'q'] // ['v','c','t','k','s']
//Start = cipherList ['b', 'e', 'k', 'a'] // ['d','g','m','c']
//Start = cipherList ['a','b','d','u','l','l','a','h'] // ['d','e','g','x','o','o','d','k']

//// Reachable points
/*11.
Given coordinates of a source point (x1, y1) determine if it is possible to reach
the destination point (x2, y2). All coordinates are positive.
From any point (x, y) there are only two types of valid movements:(x, x + y)
and (x + y, y). Return a Boolean True if it is possible, else return False.
Example: source point: (2, 10)
destination point: (26,12)
output: True (2, 10)->(2, 12)->(14, 12)->(26, 12) is a valid path.
*/

isReachable :: (Int,Int) (Int,Int) -> Bool
isReachable (sx,sy) (dx,dy)
| sx > dx || sy > dy = False
| sx == dx && sy == dy = True
= isReachable (sx + sy ,sy) (dx, dy) || isReachable (sx, sy + sx) (dx, dy)

isReachable2 :: (Int,Int) (Int,Int) -> Bool
isReachable2 (a, b) (x, y) = ((x - a) rem y == 0) && ((y-b) rem a == 0)

//Start = isReachable2 (2, 10) (26, 12) // True
//Start = isReachable (4, 20) (52, 28) // True
//Start = isReachable2 (8, 40) (104,48) // True
//Start = isReachable2 (6, 12) (20, 10) // False
//Start = isReachable2 (3, 15) (58, 69) // False

//// Evaluate
/*12.
Given a list of integer numbers representing coefficients of a polynomial,
the polynomial coefficients are given in increasing order of power.
Implement a function which evaluates the polynomial
according to a given value (substituting the given value into it).
You can assume that the given list is not empty.
Example: given the list [2,3,-5,1], the polynomial is 2x^0 + 3x^1 - 5x^2 + 1x^3.
if the given value is 1: 2 + (3 * 1) + (-5 * 1^2) + (1 * 1^3) = 1.
*/

evaluate :: [Int] Int -> Int
evaluate list a = sum [x *(a^i) \ x <- list & i <- [0..]]

//Start = evaluate [2,3,-5,1] 1 // 1
//Start = evaluate [1,-5,2,-8] -2 // 83
//Start = evaluate [1,1,1,1,1,1,1,1] 1 // 8

//// Moving digit
/*13.
Complete the function Mover that takes three integers: init, digit and target
and calculates the amount of places the digit, that is equal to digit in the number init,
has to be shifted to the right in order to get the target number.
It is guaranteed that digit exists in init and has to be shifted to the right.
Example: 134442 3 144423 -> 4
The digit 3 exists in the init number, and it has to be moved 4 places
in order to get the target number.
*/

searchDigit :: [Int] Int Int -> Int
searchDigit [] n k = -1
searchDigit [x:xs] n k
| x == n = k
= searchDigit xs n (k+1)

//Start = searchDigit [1,4,4,4,2,3] 3 0 // 5

//Start = searchDigit [1,3,4,4,4,2] 3 0 // 1

Mover :: Int Int Int -> Int
Mover init digit target = (searchDigit (toDigits target) digit 0) - (searchDigit (toDigits init) digit 0)

//Start = Mover 123 2 132 // 1
Start = Mover 134442 3 144423 // 5-1 = 4
//Start = Mover 100020001 2 100002001 // 1

module midterm_multisolved
import StdEnv

///////////////////////////////// TASK 1 /////////////////////////////////
/*1- Unique digits - 10 points
Given an integer n, return the count of all unique digits of n.
Input: n = 1232
Output: 2 (only 1 and 3 are unique - appeared only once in n).
Input: n = 1111
Output: 0 (There is no unique digit in n.)
*/

toDigit :: Int -> [Int]
toDigit x
| x < 10 = [x]
= toDigit (x/10) ++ [x rem 10]

isUnique :: Int [Int] -> Bool
isUnique x ls = length (filter ((==) x) ls) == 1

count_unique_digits :: Int -> Int
count_unique_digits x = length (filter ((==) True ) (map (\d = isUnique d y) y))
where y = toDigit x

/////

occur :: Int [Int] Int -> Int
occur num [] i = i
occur num [x:xs] i
| num == x = occur num xs (i + 1)
= occur num xs i

count_unique_digits2 :: Int -> Int
count_unique_digits2 x = length [num \ num<-(toDigit x) | occur num (toDigit x) 0 == 1 ]

Start = count_unique_digits 1234 // 4
// Start = count_unique_digits 12325332 // 2
// Start = count_unique_digits 111111 // 0
// Start = count_unique_digits 1 // 1

///////////////////////////////// TASK 2 /////////////////////////////////
/*2- Count good lists - 10 points
Given a list of lists of integer numbers, count the good sublists in
the given list. A list is considered to be good if the numbers at
even positions are even and the numbers at odd positions are prime.
Input: [[2,2,4,5], [2,3,3,5]]
Output: 1 (Only the [2,2,4,5] sublist is good as the numbers at
0th, 2nd (even) positions are even and the numbers at 1st, 3rd (odd)
positions are prime.
*/

isGood :: [Int] -> Bool
isGood [] = True
isGood [x] = isEven x
isGood [x,y:xs] = isEven x && isEmpty[s \ s <- [2..(y-1)] | y rem s == 0 ] && isGood xs

count_good_lists :: [[Int]] -> Int
count_good_lists ls = foldr (\d y | d = y + 1 = y ) 0 (map (\subls = isGood subls) ls)

/////

isPrime :: Int -> Bool
isPrime n= length [i\i<-[1..n]|(n rem i)==0]==2

isGood2 :: [Int]->Bool
isGood2 xs = xs==[xs!!i\i<-[0..(length xs)-1]|( (isEven i)&&(isEven (xs!!i)) || ((isOdd i)&&(isPrime (xs!!i))) ) ]

count_good_lists2 :: [[Int]] -> Int
count_good_lists2 xs = length [x\x<-xs|isGood2 x]

// Start = count_good_lists [[2,2,4,5],[2,3,3,5]] // 1
// Start = count_good_lists [[2,23,22],[2,29,22,5],[1,2,3]] // 2
// Start = count_good_lists [[2,2,4,5],[2,2,6,7,8,11,12,17],[12,23,4]] // 3
// Start = count_good_lists [] // 0

///////////////////////////////// TASK 3 /////////////////////////////////
/*3- Increase by position - 10 points
Given a list of real numbers, add the position of every number to the number.
Input: [1.0,2.1,3.5,2.0]
Output: [1.0,3.1,5.5,5.0] (the position of 1.0 is 0 -> 1.0 + 0 = 1.0
the position of 2.1 is 1 -> 2.1 + 1 = 3.1
the position of 3.5 is 2 -> 3.5 + 2 = 5.15
the position of 2.0 is 3 -> 2.0 + 3 = 5.0)
*/

increaseByPosition :: [Real] -> [Real]
increaseByPosition x = [el + toReal i \ el <- x & i <- [0..]]

/////

increaseByPosition2 :: [Real] -> [Real]
increaseByPosition2 list = [el+y \ el <- list & y <- [0.0..]]

// Start = increaseByPos [1.0,2.1,3.5,2.0] // [1,3.1,5.5,5]
// Start = increaseByPos [55.12,22.45,2.10,15.1,20.20] // [55.12,23.45,4.1,18.1,24.2]
// Start = increaseByPos [] // []

///////////////////////////////// TASK 4 /////////////////////////////////
/*4- Reverse integers - 10 points
Given a list of integer numbers, reverse every number in the list.
Reversing a number means to write its digits in the reversed order.
Input: [1,234,5677,43,0]
Output: [1,432,7765,34,0] Reverse of 1 is 1
Reverse of 234: the digits of 234 in reversed order are 4,3 and 2,
and by combining these digits we get the number 432
Note: reverse of e.g. 230 is 32 NOT 032
*/

reverse_num :: Int -> Int
reverse_num x = toInt ( foldr (+++) "" (map (\x= toString x) (reverse (toDigit x))) )

rev_nums :: [Int] -> [Int]
rev_nums [] = []
rev_nums [x:xs] = [(reverse_num x)] ++ rev_nums xs

/////

flat :: [Int] -> Int
flat [] = 0
flat list = hd list * (10^(length (list) - 1)) + flat (tl list)

rev_nums2 :: [Int] -> [Int]
rev_nums2 list = map (\ x = flat (reverse (toDigit x))) list

// Start = rev_nums [1,234,5677,43,0] // [1,432,7765,34,0]
// Start = rev_nums [1..5] // [1,2,3,4,5]
// Start = rev_nums [222..240]
// [222,322,422,522,622,722,822,922,32,132,232,332,432,532,632,732,832,932,42]
// Start = rev_nums [] // []

///////////////////////////////// TASK 5 /////////////////////////////////
/*5- Passed students - 10 points
Given a list of tuples and an integer number (let's call it x), where
the first element of the tuple represents a student's name and
the second element of the tuple represents the points of the student
that he/she got in a particular subject (its type is a list of real numbers).
Return those students whose points have the following property:
if the sum of the INTEGER parts of the points is greater than or equal
to the given number x.
Input: [("Abdullah",[55.55,66.55,77.75,65.07,65.57]),("Mohammed",[27.55,20.55,10.75,30.07,20.57])] 320
Output: ["Abdullah"] ( the sum of the integer parts of [55.55,66.55,77.75,65.07,65.57]
= 55 + 66 + 77 + 65 + 65 = 328 >= 320 (the given x)
- the sum of the integer parts of [27.55,20.55,10.75,30.07,20.57]
= 27 + 20 + 10 + 30 + 20 = 107 < 320 (the given x) )
*/

passedStudents :: [(String,[Real])] Int -> [String]
passedStudents ls x = [z \ z <- [name \ (name,points) <- ls | (foldr (\v s| toReal (toInt v) < v = toInt v + s = ((toInt v) - 1) + s) 0 points ) >= x]]

/////

tointAux:: Real->Int
tointAux n
|toReal(toInt n) > n = (toInt n) - 1
= toInt n

passedStudents2 :: [(String,[Real])] Int -> [String]
passedStudents2 ls n = [s \ (s,x)<-ls | sum (map (tointAux) x) >= n]

// Start = passedStudents [("Abdullah",[55.55,66.55,77.75,65.07,65.57]),("Mohammed",[27.55,20.55,10.75,30.07,20.57])] 320 // ["Abdullah"]
// Start = passedStudents [("Sara" , [5.55,44.55,55.75,30.07,90.57]),("Rayan",[56.55,66.55,7.75,77.07,77.57]),("Ali",[1.55,6.55,66.75,6.07,7.57]),("Maria",[54.55,60.55,66.75,20.07,74.57])] 200
// ["Sara","Rayan","Maria"]
// Start = passedStudents [] 100 // []

///////////////////////////////// TASK 6 /////////////////////////////////
/*6- Eliminate - 10 points
Given a list of numbers eliminate the first number of
every two numbers in the list, until only one number is left.
Input: a = [1, 2, 3, 4, 5, 6, 7, 8, 9]
a = [2, 4, 6, 8]
a = [4, 8]
a = [8]
*/

aux :: [Int] Int -> [Int]
aux [] _ = []
aux list i
|isEven i = aux (tl list) (i+1)
= [hd list] ++ aux (tl list) (i+1)

eliminate :: [Int] -> [Int]
eliminate a
| length a == 1 = a
= eliminate (aux a 0)

/////

eliminate2 :: [Int] -> [Int]
eliminate2 [] = []
eliminate2 [x] = [x]
eliminate2 ls = eliminate2 [x \ x<- ls & i <-[1..] | i rem 2 == 0]

//Start = eliminate [1..9] // [8]
//Start = eliminate [1,2,3,4] // [4]
//Start = eliminate [0] // [0]
//Start = eliminate [] // []

///////////////////////////////// TASK 7 /////////////////////////////////
/*7- Delete third - 10 points
Delete every third element from a list.
*/

del3 :: [Int] -> [Int]
del3 [] = []
del3 [x] = [x]
del3 [x,y] = [x,y]
del3 [x,y,z: t] = [x,y : del3 t]

/////

del32 :: [Int] -> [Int]
del32 ls = [x \ x <-ls & i <-[1..] | i rem 3 <> 0]

//Start = del3 [1..7] // [1,2,4,5,7]
//Start = del3 [1..20] // [1,2,4,5,7,8,10,11,13,14,16,17,19,20]
//Start = del3 [1..5] // [1,2,4,5]
//Start = del3 [] // []

///////////////////////////////// TASK 8 /////////////////////////////////
/*8- Fibonacci lists - 10 points
Write a function that takes a list of integers and for every integer
returns a list of Fibonacci sequence less than or equal to the integer.
A Fibonacci sequence is a sequence of numbers where each number is
the sum of the previous two numbers: 0, 1, 1, 2, 3, 5 ..... and so on
Input: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Output: [[0],[0,1],[0,1,1],[0,1,1,2],[0,1,1,2,3],[0,1,1,2,3,5],[0,1,1,2,3,5]
,[0,1,1,2,3,5,8],[0,1,1,2,3,5,8],[0,1,1,2,3,5,8]]
*/

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

genFib :: Int Int -> [Int]
genFib x n
| n == 0 = [0]
| x >= n = []
| fib x > n = []
= [fib x] ++ genFib (x+1) n

FibList :: [Int] -> [[Int]]
FibList x = map (\ y = genFib 0 y) x

//Start = FibList [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
// [[0],[0,1],[0,1,1],[0,1,1,2],[0,1,1,2,3],[0,1,1,2,3,5],
// [0,1,1,2,3,5],[0,1,1,2,3,5,8],[0,1,1,2,3,5,8],[0,1,1,2,3,5,8]]
//Start = FibList [0,45,88,87,21]
// [[0],[0,1,1,2,3,5,8,13,21,34],[0,1,1,2,3,5,8,13,21,34,55],
// [0,1,1,2,3,5,8,13,21,34,55],[0,1,1,2,3,5,8,13,21]]
//Start = FibList [] // []

///////////////////////////////// TASK 9 /////////////////////////////////
/*9- Carry-less addition - 10 points
Given 2 lists of integers of same length, perform carry-less addition
between 2 integers at the same position and return the list of integers.
Carry-less addition: 9+7 results 6, not 16.
35+48 results 73, not 83
Here, for the sake of simplicity, there will be only 1-digit integer.
*/

carrylessDigitAddition :: [Int] [Int] -> [Int]
carrylessDigitAddition list1 list2 = [(x+y) rem 10 \ x <- list1 & y <- list2]

//Start = carrylessDigitAddition [9,6,3,4,3,4,5] [4,8,9,0,9,6,5] // [3,4,2,4,2,0,0]
//Start = carrylessDigitAddition [7,6,5,9,8,7,6] [9,8,7,6,3,2,9] // [6,4,2,5,1,9,5]
//Start = carrylessDigitAddition [7,8,3,8] [6,2,7,8] // [3,0,0,6]
//Start = carrylessDigitAddition [2,3,4,3,2] [9,8,-7,6,5] // [1,1,-3,9,7]

///////////////////////////////// TASK 10 /////////////////////////////////
/*10- Zip with LCM - 10 points
Write a function that takes two lists of integers and returns a
list of tuples where the first element of the tuple is an element of
the first list and the second element of the tuple is an element
of the second list at the same position and the third element is the
LCM of the first two elements (LCM - least common multiple of two numbers).
LCM(2,3) = 6 LCM(3,4) = 12 LCM(12,15) = 60
If the lists are of different lengths, the function should return a list
of tuples of the same length as the shorter list.
*/

lcm :: Int Int -> Int
lcm x y = (x * y) / gcd x y

ZipWithLCM :: [Int] [Int] -> [(Int, Int, Int)]
ZipWithLCM [] _ = []
ZipWithLCM _ [] = []
ZipWithLCM [x:xs] [y:ys] = [(x,y,lcm x y)] ++ ZipWithLCM xs ys

/////

ZipWithLCM2 :: [Int] [Int] -> [(Int, Int, Int)]
ZipWithLCM2 x y = [(u, v, (u*v)/ gcd u v) \ u <- x & v <- y]

//Start = ZipWithLCM [12,14,22,57,66] [13,15,17,19,21]
// [(12,13,156),(14,15,210),(22,17,374),(57,19,19),(66,21,462)]
//Start = ZipWithLCM [78,43,12,33,65] [32,77,21,11,9,43]
// [(78,32,1248),(43,77,3311),(12,21,84),(33,11,33),(65,9,585)]
//Start = ZipWithLCM [] [32,77,21,11,9,43] // []
//Start = ZipWithLCM [78,43,12,33,65] [] // []
//Start = ZipWithLCM [] [] // []

///////////////////////////////// TASK 11 /////////////////////////////////
/*11- Split number - 10 points
Write a function that takes a number, splits in the middle and interchanges the
two halves. If digits' number is odd, the second half contains the middle digit.
1234 -> 12 | 34 -> 34 | 12 -> 3412
12345 -> 12 | 345 -> 345 | 12 -> 34512
*/

NumtoList :: Int -> [Int]
NumtoList 0 = []
NumtoList n = NumtoList (n / 10) ++ [n rem 10]

ListtoNum :: [Int] -> Int
ListtoNum [] = 0
ListtoNum [x:xs] = x * 10 ^ (length xs) + ListtoNum xs

FunNum :: Int -> Int
FunNum n = ListtoNum ((drop ((length x)/2) x) ++ (take ((length x)/2) x))
where x = NumtoList n

/////

toInteger :: [Int] -> Int
toInteger x = toInt (foldl (+++) "" (map toString x))

FunNum2 :: Int -> Int
FunNum2 n = toInteger ((drop ((length p)/2) p) ++ (take ((length p)/2) p))
where p = toDigit n

// Start = FunNum 0 // 0
// Start = FunNum 1234 //3412
// Start = FunNum 12345 //34512
// Start = FunNum 123456 //456123

///////////////////////////////// TASK 12 /////////////////////////////////
/12- Fold if true - 10 points
Write function foldiftrue which reduces only those elements of a list which
satisfy a given predicate. There are 4 reduce options which are given in String:
"max" return max number, "min" return min number, "" return product, "+" return sum.
*/

foldiftrue :: (Int -> Bool) String [Int] -> Int
foldiftrue pred func list
| func == "+" = sum (filter pred list)
| func == "*" = prod (filter pred list)
| func == "max" = maxList (filter pred list)
| func == "min" = minList (filter pred list)
= abort "not good reduce"

/////

foldiftrue2 :: (Int -> Bool) String [Int] -> Int
foldiftrue2 cond function list
| function == "max" = last (sort (filter cond list))
| function == "min" = hd (sort (filter cond list))
| function == "+" = foldr (+) 0 (filter cond list)
| function == "" = foldr () 1 (filter cond list)
= abort "not good reduce"

// Start = foldiftrue ((>)5) "max" [6,1,2,3] // 3
// Start = foldiftrue ((>)5) "min" [6,1,2,3] // 1
// Start = foldiftrue (isEven) "+" [6,1,2,3, 233, 287] // 8
// Start = foldiftrue (isEven) "*" [6,1,2,3, 233, 287] // 12

///////////////////////////////// TASK 13 /////////////////////////////////
/*13- Salary calculation - 30 points this task has 3 parts, each of 10 points
You are given list of tuples with employees' name, age and salaries, do some analysis.
Find about all the given queries using functions.
1. What is the average salary of the employees?
2. If the employer is to deduct 15% of the salaries of employees younger than 35 years old,
how much money would he save?
3. Give only the list of names where the employee is older than 35 but earns more than 300.
[("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384),
("Marie", 55, 573), ("Lucy", 37, 400)]
1. 424.71...
2. 87.6
3. ["Bob", "Anna", "Marie", "Lucy"]
*/

averageSalary :: [(String, Int, Int)] -> Real
averageSalary x = toReal (sum (map (\t = thd3 t) x)) / toReal (length x)

//Start = averageSalary [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]

savedMoney :: [(String, Int, Int)] -> Real
savedMoney x = sum (map (\y = toReal (thd3 y) * 0.15) (filter (\t = snd3 t < 35) x))

//Start = savedMoney [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]

namesOlder35 :: [(String, Int, Int)] -> [String]
namesOlder35 x = map (\y = fst3 y) (filter (\t = snd3 t > 35 && thd3 t > 300) x)

//Start = namesOlder35 [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]

/////

getSalaries :: [(String, Int, Int)] -> [Real]
getSalaries list = map toReal (map thd3 list)

averageSalary2 :: [(String, Int, Int)] -> Real
averageSalary2 list = avg (getSalaries list)
//Start = averageSalary2 [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)] // 424.714285714286

isOlderThan35 :: (String, Int, Int) -> Bool
isOlderThan35 t = snd3 t > 35

savedMoney2 :: [(String, Int, Int)] -> Real
savedMoney2 list = total - total*0.85
where total = foldr (+) 0.0 (map toReal (map thd3 [x \ x <- list | not (isOlderThan35 x)]))
//Start = savedMoney2 [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]//87.6

earnsMoreThan300 :: (String, Int, Int) -> Bool
earnsMoreThan300 t = thd3 t > 300

namesOlder352 :: [(String, Int, Int)] -> [String]
namesOlder352 list = map fst3 [x \ x <- list | earnsMoreThan300 x && isOlderThan35 x]
//Start = namesOlder352 [("John", 23, 200), ("Bob", 60, 700), ("Anna", 38, 427), ("Joe", 36, 289), ("Doe", 22, 384), ("Marie", 55, 573), ("Lucy", 37, 400)]//["Bob", "Anna", "Marie", "Lucy"]

//////////////////////////////////////////////////////////////////

module midretake_solved

import StdEnv

/* 1. Dice - 10 points
Write a function that takes an integer n and returns a
list of all possible outcomes of rolling two
dice with n sides each. The outcomes should be sorted
in ascending order. E.g. outcomes for 3:
[(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)] */

dice :: Int -> [(Int,Int)]
dice x = [(a,b) \ a <- [1..x], b <- [1..x]]

Start = dice 3 // [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3)]
//Start = dice 6 // [(1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,1),(2,2),(2,3),(2,4),(2,5),(2,6),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(4,1),(4,2),(4,3),(4,4),(4,5),(4,6),(5,1),(5,2),(5,3),(5,4),(5,5),(5,6),(6,1),(6,2),(6,3),(6,4),(6,5),(6,6)]
//Start = dice 0 // []

/* 2. Halfs - 10 points
Write a function that takes a list of lists and returns
the sum of the sums of first halves of all the sublists.
If there are odd number of elements in the list,
sum all elements before the middle element.
E.g. if the sublist is [1,2,3,4,5] the sum is 1+2=3 so
[[1,2,3,4,5],[6,7,1,8,9,10],[11,12,13,14,15]] -> [3,14,23] -> 40 */

sumFirstHalf :: [[Int]] -> Int
sumFirstHalf list = foldr (+) 0 (map (\x = sum (take ((length x)/2) x)) list)

//Start = sumFirstHalf [[1,2,3,4,5],[6,7,1,8,9,10],[11,12,13,14,15]] // 40
//Start = sumFirstHalf [[1,2,3,4],[1,2,3,4],[1,2,3,4,5],[],[1]] // 9

/* 3. Triangles - 10 points
You are given a list of lists, each sublist containing angles of a triangle.
Based on angles, for each triangle determine if is valid and what kind it is.
A triangle is valid if exactly 3 angles are given and their sum is 180.
If invalid skip the triangle. Replace the list with 0, 1 or 2 like:
0 - if is right-angled triangle (has one 90 angle)
1 - obtuse triangle (has one angle > 90)
2 - acute triangle (all angles are < 90) */

typeTriangle :: [Int] -> Int
typeTriangle t
| isMember 90 t = 0
| length [a \ a <- t | a > 90] == 1 = 1
= 2

triangle :: [[Int]] -> [Int]
triangle ts = [ typeTriangle t \ t <- ts | sum t == 180 && length t == 3]

//Start = triangle [[],[10,10,20,21],[90,45,45],[118,42,20]] // [0,1]
//Start = triangle [[28,120,32],[80,70,30],[10,20,14],[90,60,30],[180]] // [1,2,0]

/* 4. Amicable numbers - 10 points
Let d(n) be the sum of proper divisors of n. Proper divisor is a number
that is less than n and divides n. Amicable pairs are two number where
d(a)=b and d(b)=a but a<>b. Given two numbers check if they are amicable.
E.g.: d(220) = 1+2+4+5+10+11+20+22+44+55+110 = 284
d(284) = 1+2+3+71+142 = 220, so 220 and 284 are amicable numbers */

sumDivisor :: Int -> Int
sumDivisor n = sum [x \ x <- [1..n-1] | n rem x == 0]

isAmicable :: Int Int -> Bool
isAmicable a b = sumDivisor(a) == b && sumDivisor(b) == a && a <> b

//Start = isAmicable 284 220 // True
//Start = isAmicable 6 86 // False
//Start = isAmicable 49 73 // False
//Start = isAmicable 1184 1210// True
//Start = isAmicable 123 123 // False

/* 5. Second strings - 10 points
Take a list of Strings and return a String formed by joining
every second string of the list.
E.g. : ["Good ","Morning ","to ","you "] => "Morning you " */

joinEverySecond :: [String] -> String
joinEverySecond list = foldr (+++) "" [(list!!x) \ x <- [1,3..(length list)-1]]

//Start = joinEverySecond ["Good ","Morning ","to ","you "] // "Morning you "
//Start = joinEverySecond ["I ","am ","here ","a ","new ","student ",", my ","friend"] // "am a student friend"

/* 6. Magic dates - 10 points
Given a list of tuples of (day, month, year) type, determine
the magic dates. Magic date rules: if month*day is

one digit then it matches the last digit of year
2 digit number then matches the last 2 digits of the year
3 digit number then matches the last 3 digits of the year
E.g. (1,1,2011) true, 11=1 and is last digit of 2011
(9,2,2018) true, 92=18 are last 2 digits of 2018
(2,12,2021) false, 212=24 not equal to 21 of 2021
(20,11,2220) true 2011=220 equal to last 3 digits of 2220 */
magicDates :: [(Int, Int, Int)] -> [Bool]
magicDates [] = []
magicDates [x:xs]
| (fst3 x * snd3 x) < 10 = [(fst3 x * snd3 x) == (thd3 x) rem 10] ++ magicDates xs
| (fst3 x * snd3 x) < 100 = [(fst3 x * snd3 x) == (thd3 x) rem 100] ++ magicDates xs
= [(fst3 x * snd3 x) == (thd3 x) rem 1000] ++ magicDates xs

//Start = magicDates [(1,1,2011),(4,1,2011),(5,2,2010),(9,2,2011)] // [True,False,True,False]
//Start = magicDates [(12,12,2144),(9,11,1999),(9,9,2001),(19,7,1990),(10,8,1980)] // [True,True,False,False,True]

/* 7. Insert - 10 points
Write a function that takes a list of lists and an element.
Insert the element into the middle of each sublist
If there are odd number of elements in the sublist,
insert before the middle element.
Remove the empty or singleton sublists.
E.g. the sublist [1,2,3] and 4 becomes [1,4,2,3] 4 inserted before 2
[[1],[4,5,6,8],[7,8,9]] 9 => [[4,5,9,6,8],[7,9,8,9]] */

funIns :: [[a]] a -> [[a]]
funIns list n = map (\x = (take ((length x)/2) x ++ [n] ++ drop ((length x)/2) x)) (filter (\x = length x > 1) list)

//Start = funIns [[1],[4,5,6,8],[7,8,9]] 9 // [[4,5,9,6,8],[7,9,8,9]]
//Start = funIns [[], [10], [], [0]] 8 // []
//Start = funIns [['a','b','c'],['d','e','f'],['g','i'],[],['y']] 'x' // [['a','x','b','c'],['d','x','e','f'],['g','x','i']]

/* 8. Generator - 10 points
Given a list of any type elements, generate the list of different pairs that
can be generated from it. The result can be any order; it should not have duplicate pairs.
E.g.: [a,b,c,d,e,f] then the result is:
[(a,b),(a,c),(a,d),(a,e),(a,f),(b,c),(b,d),(b,e),(b,f),(c,d),(c,e),(c,f),(e,f)] */

pair :: [a] -> [(a,a)]
pair [] = []
pair [x:xs] = [ (x,y) \ y <- xs] ++ pair xs

//Start = pair ['a','b','c'] // [('a','b'),('a','c'),('b','c')]
//Start = pair [4,5,6,7,8] // [(4,5),(4,6),(4,7),(4,8),(5,6),(5,7),(5,8),(6,7),(6,8),(7,8)]
//Start = pair ["huh","okie","dokie"] // [("huh","okie"),("huh","dokie"),("okie","dokie")]
//Start = pair [1.2,3.4,5.6,7.8] // [(1.2,3.4),(1.2,5.6),(1.2,7.8),(3.4,5.6),(3.4,7.8),(5.6,7.8)]

/* 9. Tuples - 10 points
Given a list of tuples (number,function) and a condition.
Transform the list as follows: if the condition is true
for the number, then apply its pair function to the number.
Example: [(4,inc),(7,sq),(8,double),(9,dec)] isEven, then
4 and 8 are even, so computing inc 4 and double 8 result [5,16] */

double x = x2
zero x = 0
same x = x
incStr x = toString (inc x)
doubleStr x = toString (x2)
zeroStr x = toString 0
nullStr x = toString x
negate x = ~x
true x = True
multiTen x = x*10
dec x = x-1

funcTuple :: [(Int, (Int->a))] (Int->Bool) -> [a]
funcTuple list cond = [(fun x) \ (x,fun) <- list | cond x]

//Start = funcTuple [(4,inc),(3,double),(8,double),(5,same)] isEven // [5,16]
//Start = funcTuple [(7,incStr),(9,doubleStr),(8,zeroStr),(5,nullStr)] isOdd // ["8","18","5"]
//Start = funcTuple [(4,inc),(6,double),(8,zero),(5,same),(3,negate)] true // [5,12,0,5,-3]

/* 10. Compose - 10 points
Given a list of functions (Int-> Int) and an Integer, calculate
the composition of the functions.
E.g.: [double,same,negate,inc] 20 then
double(same(negate(inc 20)))) = -42 */

composit :: [(Int -> Int)] Int -> Int
composit list num = foldr (\func y = (func y)) num list

//Start = composit [double,same,negate,inc] 20 // -42
//Start = composit [multiTen,same,double,dec] 35 // 680
//Start = composit [multiTen,multiTen,multiTen] 1 // 1000
//Start = composit [zero,inc,double,double] 2 // 0

/* 11. Replace - 10 points
Given a list of integers and a list of lists. The sublists of the later
have exactly 2 elements, first element represents the current values
and the second element represents new values. The integer list has
to be transformed according to the given list of lists.
E.g.: [5, 7, 6, 8, 9, 5] and [[5,2],[6,3],[3,6]] Then:
[5,2] the values 5 should be replaced by 2 => [2,7,6,8,9,2]
[6,3] the values 6 should be replaced by 3 => [2,7,3,8,9,2]
[3,6] the values 3 should be replaced by 6 => [2,7,6,8,9,2]
After replacements, the result is [2,7,6,8,9,2]. */

transformListAux :: [Int] [Int] -> [Int]
transformListAux _ [] = []
transformListAux [elem, new_elem] [head:rest]
| elem == head = [new_elem] ++ transformListAux [elem, new_elem] rest
= [head : transformListAux [elem, new_elem] rest]

transformList :: [Int] [[Int]] -> [Int]
transformList ls [] = ls
transformList ls [x:xs] = transformList (transformListAux x ls ) xs

//Start = transformList [5,7,6,8,9,5] [[5,2],[6,3],[3,6]] // [2,7,6,8,9,2]
//Start = transformList ((take 5 (repeat 5)) ++ (take 3 (repeat 7))) [[5,1],[6,1],[1,6]] // [6,6,6,6,6,7,7,7]
//Start = transformList [] [[5,2],[6,2]] // []

/* 12. Remainder - 10 points
Write a function that takes two integers and returns
the remainder of the first integer divided by the second.
You CANNOT use the rem, / and ~ operators. */

findRem :: Int Int -> Int
findRem x y
| x < y = x
= findRem (x-y) y

//Start = findRem 5 2 // 1
//Start = findRem 19 5 // 4

/* 13. Binary number - 10 points
Convert decimal to binary number.
E.g. 142/2 = 71/2 = 35/2 = 17/2 = 8/2 = 4/2 = 2/2 = 1 => 10001110
rem 0 1 1 1 0 0 0 1
<<--------------------------------------------- */

aux :: Int -> [Int]
aux 0 = [0]
aux 1 = [1]
aux n = [n rem 2] ++ aux (n/2)

listToInt :: [Int] -> Int
listToInt [x] = x
listToInt list = (last list) * (10^(length list - 1)) + listToInt (init list)

toBinary :: Int -> Int
toBinary n = listToInt (aux n)

//Start = toBinary 142 // 10001110
//Start = toBinary 11 // 1011
//Start = toBinary 45 // 101101
//Start = toBinary 339 // 101010011

/* 14. Rewrite - 10 points
Rewrite the function takeWhile using higher order functions. */

takeWhileX :: (a -> Bool) [a] -> [a]
takeWhileX _ [] = []
takeWhileX cond list = foldr (\x y | cond x = ([x] ++ y) = []) [] list

//Start = takeWhileX isEven [2,4,6,7,8,10,12,14,16] // [2,4,6]
//Start = takeWhileX isEven [1,3,5,7,9] // []
//Start = takeWhileX isOdd [1,3,5,7,9] // [1,3,5,7,9]
//Start = takeWhileX (\num = [x \ x <- [2..(num - 1)] | num rem x == 0 ] == [] ) [17,5,23,29,2,4,6,11,6] // [17,5,23,29,2]

module slides_codes

import StdEnv

//// CODE EXAMPLES of SLIDES
////////////// SLIDES1

//Start = 4+5 // 9
//Start = 42 // 42
//Start = 3+10*2 // 23
//Start = sqrt 3.0 // 1.73...

double :: Int -> Int
double x = x + x

quadruple :: Int -> Int
quadruple x = double (double x)

//Start = double 2
//Start = quadruple 2

factorial :: Int -> Int
factorial n = prod [1 .. n]

//Start = factorial 5

// two cases
abs1 :: Int -> Int
abs1 x
| x<0 = ~x
| otherwise = x

//Start = abs1 -4 // 4

// otherwise can be omitted
abs2 :: Int -> Int
abs2 x
| x<0 = ~x
= x

//Start = abs2 4 // 4

// more then two guards or cases
signof :: Int -> Int
signof x
| x>0 = 1
| x==0 = 0
| x<0 = -1

//Start = signof -8 // -1

factor :: Int -> Int
factor n
| n==0 = 1
| n>0 = n * factor (n-1)

//Start = factor 5

power :: Int Int -> Int
power x n
| n == 0 = 1
= x * power x (n-1)

//Start = power 2 5

l1 :: [Int]
l1 = [1, 2, 3, 4, 5]
l2 :: [Bool]
l2 = [True, False, True]
l3 :: [Real->Real]
l3 = [sin, cos, sin]
l4 :: [[Int]]
l4 = [[1, 2, 3], [8, 9]]
l5 :: [a]
l5 = []
l6 :: [Int]
l6 = [1..10]
l7 :: [Int]
l7 = [1..]

//Start = l4

//Start = [1..10]
//Start = [1,2..10]
//Start = [1,0.. -10]
//Start = [1.. -10]
//Start = [1..0]
//Start = [1..1]
//Start = [1,3..4]
//Start = [1..]
//Start = [1,3..]
//Start = [100,80..]

//Start = [1,2..10]

//Start = hd [1, 2, 3, 4, 5]
//Start = tl [1, 2, 3, 4, 5]
//Start = drop 2 [1, 2, 3, 4, 5]
//Start = take 2 [1, 2, 3, 4, 5]
//Start = [1, 2, 3] ++ [6, 7]
//Start = reverse [1, 2, 3]
//Start = length [1, 2, 3, 4]
//Start = last [1, 2, 3]
//Start = init [1, 2, 3]
//Start = isMember 2 [1, 2, 3]
//Start = isMember 5 [1, 2, 3]
//Start = flatten [[1,2], [3, 4, 5], [6, 7]]

//Start = take 2 []
//Start = drop 5 [1,2,3]
//Start = take 2 [1 .. 10]
//Start = drop ([1..5]!!2) [1..5]

//Start = reverse [1,3..10]
//Start = reverse [5,4 .. -5]
//Start = isMember 0 []
//Start = isMember -1 [1..10]
//Start = isMember ([1..5]!!1) [1..5]

// some list patterns
triplesum :: [Int] -> Int
triplesum [x, y, z] = x + y + z

//Start = triplesum [1,2,4] // 7 [1,2,3,4] error

head :: [Int] -> Int
head [x : y] = x

//Start = head [1..5] // 1

tail :: [Int] -> [Int]
tail [x : y] = y

//Start = tail [1..5] // [2,3,4,5]

// omitting values
f :: Int Int -> Int
f _ x = x

//Start = f 4 5 // 5

// patterns with list constructor
g :: [Int] -> Int
g [x, y : z] = x + y

//Start = g [1, 2, 3, 4, 5] // 3

// patterns + recursively applied functions
lastof :: [Int] -> Int
lastof [x] = x
lastof [x : y] = lastof y

//Start = lastof [1..10] // 10

// recursive functions on lists
sum1 :: [Int] -> Int
sum1 x
| x == [] = 0
| otherwise = hd x + sum1 (tl x)

//Start = sum1 [1..5] // 15

sum2 :: [Int] -> Int
sum2 [] = 0
sum2 [first : rest] = first + sum2 rest

//Start = sum2 [1..5] // 15

// recursive function with any element pattern
length1 :: [Int] -> Int
length1 [] = 0
length1 [_ : rest]= 1 + length1 rest

//Start = length1 [1..10] // 10 l1 :: [Int]
//Start = isMember ([1..5]!!1) [1..5]

//Start = filter isEven [1..10] // [2,4,6,8,10]

odd x = not (isEven x)
//Start = odd 23 // True

//Start = filter (not o isEven) [1..100] // [1,3,5,..,99]

//Start = takeWhile isEven [2,4,6,7,8,9] // [2, 4, 6]

//Start = dropWhile isEven [2,4,6,7,8,9] // [7, 8, 9]

//Start = map inc [1, 2, 3] // [2, 3, 4]

//Start = map double [1, 2, 3] // [2, 4, 6]

// lambda expressions
//Start = map (\x = xx+2x+1) [1..10] // [4,9,16,25,36,49,64,81,100,121]

//Start = foldr (+) 10 [1, 2, 3] // 16

product1 = foldr (*) 1

//Start = product1 [1, 2, 3] // 6

and1 :: [Bool] -> Bool
and1 [] = True
and1 [x:xs] = x && and1 xs

and2 = foldr (&&) True

//Start = and2 [True, True, False] // False

sumF = foldr (+) 0

//Start = sumF [1, 2, 3] // 6

qsort :: [a] -> [a] | Ord a
qsort [] = []
qsort [c : xs] = qsort [x \ x <- xs | x < c] ++ [c] ++
qsort [x \ x <- xs | x >= c]

// Start = qsort [2,1,5,3,6,9,0,1] // [0,1,1,2,3,5,6,9]

// sort is the built in operation for sorting

// Start = sort [3,1,4,2,0] // [0,1,2,3,4]

// inserting in already sorted list
Insert :: a [a] -> [a] | Ord a
Insert e [] = [e]
Insert e [x : xs]
| e <= x = [e , x : xs]
| otherwise = [x : Insert e xs]

// Start = Insert 5 [2, 4 .. 10] // [2,4,5,6,8,10]

mysort :: [a] -> [a] | Ord a
mysort [] = []
mysort [a:x] = Insert a (mysort x)

// Start = mysort [3,1,4,2,0] // [0,1,2,3,4]

// Start = Insert 3 (Insert 1 (Insert 4 (Insert 2 (Insert 0 [] ))))

merge1 :: [a] [a] -> [a] | Ord a
merge1 [] ys = ys
merge1 xs [] = xs
merge1 [x : xs] [y : ys]
| x <= y = [x : merge1 xs [y : ys]]
| otherwise = [y : merge1 [x : xs] ys]

// Start = merge1 [2,5,7] [1,5,6,8] // [1,2,5,5,6,7,8]
// Start = merge1 [] [1,2,3] // [1,2,3]
// Start = merge1 [1,2,10] [] // [1,2,10]
// Start = merge1 [2,1] [4,1] // [2,1,4,1]
// Start = merge1 [1,2] [1,4] // [1,1,2,4]

msort :: [a] -> [a] | Ord a
msort xs
| len <= 1 = xs
| otherwise = merge (msort ys) (msort zs)
where
ys = take half xs
zs = drop half xs
half = len / 2
len = length xs

// Start = msort [2,9,5,1,3,8] // [1,2,3,5,8,9]

fromn :: Int -> [Int]
fromn n = [n : fromn (n+1)]

//Start = fromn 8

//Start = map ((^)3) [1..]

//Start = takeWhile ((>) 1000) (map ((^)3) [1..])

repeat1 :: a -> [a]
repeat1 x = list
where list = [x:list]

//Start = repeat1 5

repeatn1 :: Int a -> [a]
repeatn1 n x = take n (repeat x)

//Start = repeatn1 5 8

iterate1 :: (a->a) a -> [a]
iterate1 f x = [x: iterate1 f (f x)]

//Start = iterate1 inc 5 // [5,6,7,8,9,...]

//Start = iterate1 ((+)1) 5 // [5,6,7,8,9,...]

//Start = iterate1 ((*)2) 1 // [1,2,4,8,16,...]

//Start = iterate1 (\ x= x/10) 54321 // [54321,5432,543,54,5,0,0...]

// Prime numbers

divisible :: Int Int -> Bool
divisible x n = x rem n == 0

denominators :: Int -> [Int]
denominators x = filter (divisible x) [1..x]

prime :: Int -> Bool
prime x = denominators x == [1,x]

primes :: Int -> [Int]
primes x = filter prime [1..x]

//Start = primes 100 // [2,3,5,7,...,97]

sieve :: [Int] -> [Int]
sieve [p:xs] = [p: sieve [ i \ i <- xs | i rem p <> 0]]

//Start = take 100 (sieve [2..])

// Define a function CountOccurrences that counts the number of times a given element is
// occurring in a given list.

CountOccurrences :: a [a] -> Int | == a
CountOccurrences a [x : xs] = f a [x : xs] 0
where
f a [] i = i
f a [x : xs] i
| a == x = f a xs i+1
= f a xs i

//Start = CountOccurrences 2 [2, 3, 4, 2, 2, 4, 2, 1] // 4

////////////// SLIDES2

//// Records - Person

:: Person = { name :: String
, birthdate :: (Int,Int,Int)
, fpprogramer :: Bool
}

IsfpUser :: Person -> String
IsfpUser {fpprogramer = True} = "Yes"
IsfpUser _ = "No"

//Start = IsfpUser { name = "Me"
// , birthdate = (1,1,1999)
// , fpprogramer = True} // "Yes"

GetName :: Person -> String
GetName p = p.name

GetName2 :: Person -> String
GetName2 {name} = name

ChangeN :: Person String -> Person
ChangeN p s = {p & name = s}

//Start = ChangeN {name = "XY", birthdate = (1,1,2000), fpprogramer = True} "Alex"

//// Records - Point

:: Point = { x :: Real
, y :: Real
, visible :: Bool
}

:: Vector = { dx :: Real
, dy :: Real
}

Origo :: Point
Origo = { x = 0.0
, y = 0.0
, visible = True
}
Dist :: Vector
Dist = { dx = 1.0
, dy = 2.0
}

IsVisible :: Point -> Bool
IsVisible {visible = True} = True
IsVisible _ = False

xcoordinate :: Point -> Real
xcoordinate p = p.x

hide :: Point -> Point
hide p = { p & visible = False }

Move :: Point Vector -> Point
Move p v = { p & x = p.x + v.dx, y = p.y + v.dy }

//Start = Move (hide Origo) Dist

//// Records - Q type

:: Q = { nom :: Int
, den :: Int
}

QZero :: Q
QZero = { nom = 0, den = 1 }
QOne :: Q
QOne = { nom = 1, den = 1 }

simplify :: Q -> Q
simplify {nom=n,den=d}
| d == 0 = abort " denominator is 0"
| d < 0 = { nom = ~n/g, den = ~d/g}
| otherwise = { nom = n/g, den = d/g}
where g = gcdm n d

gcdm :: Int Int -> Int
gcdm x y = gcdnat (abs x) (abs y)
where gcdnat x 0 = x
gcdnat x y = gcdnat y (x rem y)

mkQ :: Int Int -> Q
mkQ n d = simplify { nom = n, den = d }

//Start = mkQ 81 90

//// Arrays

MyArray :: {Int}
MyArray = {1,3,5,7,9}

//Start = MyArray
//Start = MyArray.[2] // 5

MapArray1 f a = {f e \ e <-: a}

//Start :: {Int}
//Start = MapArray1 inc MyArray

//// Algebraic types, trees

:: Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

:: Tree a = Node a (Tree a) (Tree a)
| Leaf

sizeT :: (Tree a) -> Int
sizeT Leaf = 0
sizeT (Node x l r) = 1 + sizeT l + sizeT r

//Start = aTree

//Start = sizeT aTree // 4

depth :: (Tree a) -> Int
depth Leaf = 0
depth (Node _ l r) = (max (depth l) (depth r)) + 1

//Start = depth aTree // 2

treesort :: ([a]-> [a]) | Eq, Ord a
treesort = collect o listtoTree

listtoTree :: [a] -> Tree a | Ord, Eq a
listtoTree [] = Leaf
listtoTree [x:xs] = insertTree x (listtoTree xs)

insertTree :: a (Tree a) -> Tree a | Ord a
insertTree e Leaf = Node e Leaf Leaf
insertTree e (Node x le ri)
| e<=x = Node x (insertTree e le) ri
| e>x = Node x le (insertTree e ri)

collect :: (Tree a) -> [a]
collect Leaf = []
collect (Node x le ri) = collect le ++ [x] ++ collect ri

//Start = treesort [3, 1, 5, 9, 2, 7, 0]

atree = Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)

//Start = atree

:: Tree2 a = Node2 a (Tree2 a) (Tree2 a)
| Leaf2 a

nrNodes :: (Tree2 a) -> Int
nrNodes (Leaf2 y) = 1
nrNodes (Node2 x l r) = 1 + nrNodes l + nrNodes r

aTree2 :: Tree2 Int

aTree2 = Node2 4 (Node2 2 (Node2 1 (Leaf2 1) (Leaf2 1)) (Node2 3 (Leaf2 3) (Leaf2 3))) (Leaf2 5)

//Start = aTree2

//Start = nrNodes aTree2 // 9

:: Tree3 a b = Node3 a (Tree3 a b) (Tree3 a b)
| Leaf3 b

aTree3 :: Tree3 Int Real

aTree3 = Node3 2 (Node3 1 (Leaf3 1.1) (Leaf3 2.5)) (Node3 3 (Leaf3 3.0) (Leaf3 6.9))

//Start = aTree3

sumLeaves :: (Tree3 Int Real) -> Real
sumLeaves (Leaf3 y) = y
sumLeaves (Node3 x le ri) = sumLeaves le + sumLeaves ri

//Start = sumLeaves aTree3 //13.5

// Triple branches
:: Tree4 a = Node4 a (Tree4 a) (Tree4 a) (Tree4 a)
| Leaf4

// Rose-tree - tree with variable multiple branches
// No leaf constructor, node with no branches
:: Tree5 a = Node5 a [Tree5 a]

// Every node has one branch = list
:: Tree6 a = Node6 a (Tree6 a)
| Leaf6

// Tree with different types
:: Tree7 a b = Node7a Int (Tree7 a b) (Tree7 a b)
| Node7b b (Tree7 a b)
| Leaf7a b
| Leaf7b Int

:: BTree a = Bin (BTree a) (BTree a)
| Tip a

mapbtree :: (a -> b) (BTree a) -> BTree b
mapbtree f (Tip x) = Tip (f x)
mapbtree f (Bin t1 t2) = Bin (mapbtree f t1) (mapbtree f t2)

foldbtree :: (a a -> a) (BTree a) -> a
foldbtree f (Tip x) = x
foldbtree f (Bin t1 t2) = f (foldbtree f t1) (foldbtree f t2)

aBTree = Bin (Bin (Bin (Tip 1) (Tip 1)) (Bin (Tip 3) (Tip 3))) (Tip 5)

//Start = aBTree
//Start = mapbtree inc aBTree
//Start = foldbtree (+) aBTree // 13

//// Instances

instance + String
where
(+) s1 s2 = s1 +++ s2

//Start = "Hello" + " world!" // "Hello world!"

instance + (a,b) | + a & + b
where
(+) (x1,y1) (x2,y2) = (x1+x2,y1+y2)

//Start = (1,2) + (3,4) // (4,6)

/* in StdTuple.dcl
instance == (a,b) | Eq a & Eq b
instance == (a,b,c) | Eq a & Eq b & Eq c

in StdTuple.icl
instance == (a,b) | Eq a & Eq b
where
(==) ::!(a,b) !(a,b) -> Bool | Eq a & Eq b
(==) (x1,y1) (x2,y2) = x1==x2 && y1==y2

instance == (a,b,c) | Eq a & Eq b & Eq c
where
(==) ::!(a,b,c) !(a,b,c) -> Bool | Eq a & Eq b & Eq c
(==) (x1,y1,z1) (x2,y2,z2) = x1==x2 && y1==y2 && z1==z2
*/

//Start = (1,2) == (3,4) // False == overloading

increment n = n+1

//Start = increment 4

doubleA :: a -> a | +a
doubleA x = x + x

//Start = doubleA 3

//Start = doubleA 3.3

delta :: a a a -> a | ,-,fromInt a
delta a b c = bb - (fromInt 4)ac

//Start = delta 1.0 2.0 1.0

class Delta a | *,-,fromInt a

delta1 :: a a a -> a | Delta a
delta1 a b c = b*b - (fromInt 4)ac

//Start = delta1 1.0 2.0 1.0

//// Classes

class PlusMinx a
where
(+) infixl 6 :: !a !a -> a
(-) infixl 6 :: !a !a -> a
zerox :: a

instance PlusMinx Char
where
(+) :: !Char !Char -> Char
(+) x y = toChar (toInt(x) + toInt(y))
(-~) x y = toChar (toInt(x) - toInt(y))
zerox = toChar 0

//Start = 'a' +~ 'e'

//Start :: Char
//Start = zerox

double1 :: a -> a | PlusMin a
double1 x = x + x

//Start = double1 2 // 4

//// Instances Q type

instance + Q
where
(+) x y = mkQ (x.nomy.den+y.nomx.den) (x.den*y.den)

// Start = mkQ 2 4 + mkQ 5 6 // (Q 4 3)

instance - Q
where
(-) x y = mkQ (x.nomy.den-y.nomx.den) (x.den*y.den)

// Start = mkQ 2 4 - mkQ 5 6 // (Q -1 3)

instance fromInt Q
where
fromInt i = mkQ i 1

//Start :: Q
//Start = fromInt 3 // (Q 4 3)

instance zero Q
where
zero = fromInt 0

//Start :: Q
//Start = zero // (Q 0 1)

instance one Q
where
one = fromInt 1 //

//Start :: Q
//Start = one // (Q 1 1)

instance toString Q
where
toString q
| xq.den == 1 = toString xq.nom
| otherwise = toString xq.nom +++"/"+++ toString xq.den
where xq = simplify q

//Start = toString (mkQ 3 4) // "3/4"

instance < Q
where
(<) x y = x.nomy.den < y.nomx.den

//Start = mkQ 1 2 < mkQ 3 4 // True

ls = [toString q \ q <- [zero, mkQ 1 3 .. mkQ 3 2]]

//Start :: [String]
//Start = ls // ["0","1/3","2/3","1","4/3"]

//overloading can not be solved
//Start = toString zero+zero

/*
Start :: String
Start = toString sum // "0"
where sum :: Q
sum = zero + zero */

//// Instances C type

:: C = { re :: Real
, im :: Real
}

mkC r i = { re = r, im = i }

//Start = mkC 1.0 10.0 // (C 1 10)

instance + C
where
(+) x y = mkC (x.re+y.re) (x.im+y.im)

//Start = mkC 2.2 4.1 + mkC 1.5 6.4 // (C 3.7 10.5)

instance - C
where
(-) x y = mkC (x.re-y.re) (x.im-y.im)

//Start = mkC 2.2 4.1 - mkC 1.5 6.4 // (C 0.7 -2.3)

instance * C
where
() x y = mkC (x.rey.re - x.imy.im) (x.rey.im + x.im*y.re)

//Start = mkC 2.0 4.0 * mkC 3.0 2.0 // (C -2 16)

// for simplicity only division by a real nr. is defined
instance / C
where
(/) x y
| y.im == 0.0 = mkC (x.re/y.re) (x.im/y.re)
= abort "division not defined"

//Start = (mkC 2.0 4.0) / (mkC 2.0 0.0) // (C 1 2)

instance fromReal C
where
fromReal r = mkC r 0.0

//Start :: C
//Start = fromReal 3.0 // (C 3 0)

instance toReal C
where
toReal x
| x.im == 0.0 = x.re
= abort "x has imaginary part"

//Start = toReal (mkC 3.0 0.0) // 3

instance zero C
where
zero = fromReal 0.0

//Start :: C
//Start = zero // (C 0 0)

instance one C
where
one = fromReal 1.0

//Start :: C
//Start = one // (C 1 0)

instance abs C
where
abs x = fromReal (sqrt (x.rex.re + x.imx.im))

//Start = abs (mkC 3.0 4.0) // (C 5 0)

//conjugate of a complex x+yi is x-yi
instance ~ C
where
~x = mkC x.re (~x.im)

//Start = ~ (mkC 2.0 3.0) // (C 2 -3)

instance toString C
where
toString x
| x.im == 0.0 = toString x.re
| otherwise = toString x.re +++ "+" +++ toString x.im +++ "i"

//Start = toString (mkC 3.0 4.0) // "3+4i"

instance == C
where
(==) x y = x.re == y.re && x.im == y.im

//Start = mkC 1.0 2.0 == mkC 1.0 2.0 // True

// tests whether the complex number represents a real nr.
isRealC :: C -> Bool
isRealC x
| x.im == 0.0 = True
= False

//Start = isRealC (mkC 2.0 0.0) // True

// returns real part
re :: C -> Real
re x = x.re

//Start = re (mkC 1.0 2.0) // 1

// returns imaginary part
im :: C -> Real
im x = x.im

//Start = im (mkC 1.0 2.0) // 2

// for Map see separate file
///////////

