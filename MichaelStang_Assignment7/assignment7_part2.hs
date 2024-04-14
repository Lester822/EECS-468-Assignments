-- ASSIGNMENT_NAME: EECS 468 Assignment 7 (Part 2 of 2)
-- FUNCTION: A haskell program that solves the various permutation and combination problems from EECS 210
-- INPUTS: None
-- OUTPUTS: Demos of each of the functions created
-- AUTHOR_NAME: Michael Stang
-- COLLABORATORS: in-class slides and ChatGPT for part c
-- CREATION_DATE: 04/14/24

factorial :: Integer -> Integer -- Helper functio to calculate a factorials
factorial num = product [a | a <- [1..num]] -- Uses a list comprehension and "product" to calculate the factorial

dist_ob_dist_box :: Integer -> Integer -> Integer -- Sorting distinguishable objects into distinguishable boxes
dist_ob_dist_box object_c box_c = (factorial object_c) `div` ((factorial (object_c - box_c)) * (factorial box_c))-- Uses the formula to calculate

indist_ob_dist_box :: Integer -> Integer -> Integer -- Sortoing indist objects into dist boxes
indist_ob_dist_box object_c box_c = dist_ob_dist_box (object_c + box_c -1) object_c -- Uses the formula given in the slides

-- ChatGPT was used to help with some of the logic of implementing the sterling function from the given formula given in the 210 slides
sterling :: Integer -> Integer -> Integer -- Function to calculate sterling values
sterling n k -- Takes in n and k, with n being objects and k being boxes
    | k > n = 0 -- Base cases where we have more boxes than objects
    | k == n = 1  -- Base cases, where they're equal
    | k == 1 = 1 -- Base cases, where we have one box
    | otherwise = k * sterling (n - 1) k + sterling (n - 1) (k - 1) -- Recursively does sterling to handle summation aspect


dist_ob_indist_box :: Integer -> Integer -> Integer -- Counting the number of ways to distribute distinguishable objects into indistinguishable boxes
dist_ob_indist_box obj boxes = sum [sterling obj j | j <- [1..boxes]] -- Does the summation of sterlings as stated by the slides


indist_ob_indist_box :: Integer -> Integer -> Integer -- Indistinguishable objects in indestinguishable boxes
indist_ob_indist_box objects boxes -- Boxes = k objects = n
    | boxes == 1 = 1 -- Base cases as defined in the slides
    | objects == 0 = 1 -- If objects is 0, then 1
    | boxes < 1 = 0 -- if boxes less than 1, then 0
    | objects < 0 = 0 -- if objects less than 0
    | otherwise = (indist_ob_indist_box (objects - boxes) boxes) + (indist_ob_indist_box objects (boxes - 1)) -- Recurse through the function but goimg down in the object and box count getting each combo



main :: IO () -- Main function
main = do -- Tells main what to do

     -- This part of the code is essentially just for demoing purposes. Most of it is printing to the console to showcase how the functions work.

     -- DIST OBJECTS DIST BOXES

     putStrLn "Part 2a: Distinguishable Objects and Distinguishable Boxes" -- Header print
     putStrLn "\nDebug Example: How many ways are there to deal 5-card poker hands from a 52-card deck to each of four players?" -- Prints what example we're solving
     let ways_to_deal = (dist_ob_dist_box 52 5) * (dist_ob_dist_box 47 5) * (dist_ob_dist_box 42 5) * (dist_ob_dist_box 37 5) -- Calculates the output
     print ways_to_deal -- Prints the output

     putStrLn "\nFunction Test Example: A professor packs her collection of 40 issues of a mathematics journal in four boxes with 10 issues per box. How many ways can she distribute the journals if each box is numbered, so that they are distinguishable?"  -- Prints the example
     let ways_to_box = (dist_ob_dist_box 40 10) * (dist_ob_dist_box 30 10) * (dist_ob_dist_box 20 10) -- Calculates the output
     print ways_to_box -- Displays the output

     -- INDIST OBJECTS DIST BOXES

     putStrLn "Part 2b: Indistinguishable Objects and Distinguishable Boxes" -- Header print
     putStrLn "\nDebug Example: How many ways are there to place 10 indistinguishable balls into 8 distinguishable bins?" -- Prints what example we're solving
     let iodb_example = indist_ob_dist_box 10 8 -- Calculates the output
     print iodb_example -- Prints the output

     putStrLn "\nFunction Test Example: How many ways are there to distribute 12 indistinguishable balls into six distinguishable bins?" -- Prints the example
     let iodb_example2 = indist_ob_dist_box 12 6 -- Calculates the output
     print iodb_example2 -- Displays the output

     -- DIST OBJECTS INDIST BOXES

     putStrLn "Part 2c: Distinguishable Objects and Indistinguishable Boxes" -- Header print
     putStrLn "\nDebug Example: How many ways can Anna, billy, Caitlin, and Danny be placed into three indistinguishable homerooms?" -- Prints what example we're solving
     let doib_example1 = dist_ob_indist_box 4 3 -- Calculates the output
     print doib_example1 -- Prints the output

     putStrLn "\nFunction Test Example: How many ways are there to put five temporary employees into four identical offices?" -- Prints the example
     let doib_example2 = dist_ob_indist_box 5 4 -- Calculates the output
     print doib_example2 -- Displays the output

     -- INDIST OBJECTS INDIST BOXES

     putStrLn "Part 2d: Indistinguishable Objects and Indistinguishable Boxes" -- Header print
     putStrLn "\nDebug Example: How many ways can you pack six copies of the same book into four identical boxes?" -- Prints what example we're solving
     let ioib_example1 = indist_ob_indist_box 6 4 -- Calculates the output
     print ioib_example1 -- Prints the output

     putStrLn "\nFunction Test Example: How many ways are there to distribute five indistinguishable objects into three indistinguishable boxes?"  -- Prints the example
     let ioib_example2 = indist_ob_indist_box 5 3 -- Calculates the output
     print ioib_example2 -- Displays the output