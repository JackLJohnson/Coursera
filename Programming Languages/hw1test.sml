
val p11 = is_older((2012,12,30),(2013,1,1)) = true
val p12 = is_older((1956,1,15),(1980,12,31)) = true
val p13 = is_older((2000, 10, 15), (10, 10, 15)) = false

val p21 = number_in_month([(1990, 10, 1), (1912, 5, 10), (10, 10, 10)], 10) = 2
val p22 = number_in_month([(1990, 10, 1), (1912, 5, 10), (10, 10, 10)], 2) = 0
val p23 = number_in_month([(1990, 4, 1), (1912, 4, 4), (4, 4, 4)], 10) = 0

val p31 = number_in_months([(1990, 10, 1), (1912, 5, 10), (10, 10, 10)], [5,10]) = 3
val p32 = number_in_months([(1990, 10, 1), (1912, 5, 10), (10, 10, 10)], [1,6]) = 0
val p33 = number_in_months([(1990, 10, 1), (1912, 4, 4), (10, 3, 10)], [3,4,5]) = 2

val p41 = dates_in_month([(1990, 10, 1), (1912, 5, 10), (10, 10, 10)], 10) = [(1990, 10, 1),(10, 10, 10)]
val p42 = dates_in_month([(4212, 12, 1)], 12) = [(4212, 12, 1)]
val p43 = dates_in_month([(4212, 12, 1), (21, 12, 12), (2121, 12, 12)], 12) = [(4212, 12, 1), (21, 12, 12), (2121, 12, 12)]

val p51 = dates_in_months([(1990, 10, 1), (1912, 5, 10), (10, 10, 10)], [5,10]) = [(1912, 5, 10),(1990, 10, 1),(10, 10, 10)]
val p52 = dates_in_months([(4212, 12, 1)], [12,3,5,6]) = [(4212, 12, 1)]
val p53 = dates_in_months([(4212, 12, 1), (21, 12, 12), (2121, 12, 12)], [1,2,3,4,12]) = [(4212, 12, 1), (21, 12, 12), (2121, 12, 12)]

val p61 = get_nth(["a","b","c","d","e"], 3) = "c"
val p62 = get_nth(["a","b","c","d","e"], 5) = "e"
val p63 = get_nth(["a"], 1) = "a"

val p71 = date_to_string(2012,4,25) = "April 25, 2012"
val p72 = date_to_string(1,1,1) = "January 1, 1"
val p73 = date_to_string(1452,2,28) = "February 28, 1452"

val p81 = number_before_reaching_sum(20, [3,5,7,1,2,8,10,20]) = 5
val p82 = number_before_reaching_sum(3, [3,5,7,1,2,8,10,20]) = 0
val p83 = number_before_reaching_sum(4, [3,5]) = 1

val p91 = what_month(1) = 1
val p92 = what_month(42) = 2
val p93 = what_month(365) = 12

val p101 = month_range(30,37) = [1,1,2,2,2,2,2,2]
val p102 = month_range(41,40) = []
val p103 = month_range(1,5) = [1,1,1,1,1]

val p111 = oldest([(2012,1,12)]) = SOME(2012,1,12)
val p112 = oldest([(2012,1,12), (1921,4,30), (2054,12,31)]) = SOME(1921,4,30)
val p113 = oldest([(2012,1,12), (1921,4,30), (456,12,31)]) = SOME(456,12,31)
