-- Chris Delaney
-- HW8

medianOfThree a b c = 
 if (a < b && a > c) || (a < c && a > b)  then a
 else if (b < a && b > c) || (b < c && b > a) then b
 else c

medianOfNine a b c d e f g h i = 
 if (medianOfThree a b c < medianOfThree d e f && 
     medianOfThree a b c > medianOfThree g h i) ||
     (medianOfThree a b c < medianOfThree g h i &&
     medianOfThree a b c > medianOfThree d e f) then medianOfThree a b c
 else if (medianOfThree d e f < medianOfThree a b c && 
          medianOfThree d e f > medianOfThree g h i) ||
          (medianOfThree d e f < medianOfThree g h i &&
          medianOfThree d e f > medianOfThree a b c) then medianOfThree d e f
 else medianOfThree g h i

isUnordered a b c = 
 if a < b && b < c then False
 else True

quadrant (x, y) = 
 if x > 0 && y > 0 then 1
 else if x < 0 && y > 0 then 2
 else if x < 0 && y < 0 then 3
 else if x > 0 && y < 0 then 4
 else 0

earlierDate (m1, d1, y1) (m2, d2, y2) =
 if y1 < y2 then True
 else if y1 <= y2 && m1 < m2 then True
 else if y1 <= y2 && m1 <= m2 && d1 < d1 then True
 else False

distance (x1, y1) (x2, y2) = 
 sqrt((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

manhattanDistance (x1, y1) (x2, y2) =
 abs(x1 - x2) + abs(y1 - y2)
