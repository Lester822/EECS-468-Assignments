-- ASSIGNMENT_NAME: EECS 468 Assignment 7 (Part 1 of 2)
-- FUNCTION: A haskell program that showcases the creation of various functions created using list comprehensions
-- INPUTS: None
-- OUTPUTS: Demos of each of the functions created
-- AUTHOR_NAME: Michael Stang
-- COLLABORATORS: in-class slides
-- CREATION_DATE: 04/13/24

replicate' :: Int -> a -> [a] -- Function to create a list of length i containing element a in each spot
replicate' count element = [ element | _ <- [1..count]]  -- List comprehension to make the list

perfects :: Int -> [Int]  -- Function to find all of the "perfect" numbers from 1 to the passed in int
perfects top = [val | val <- [1..top], sum (factors val) == val] -- Uses a list comprehension to go through each number and see if its perfect

factors :: Int -> [Int]  -- Helper function to find all of the factors of a given number
factors num = [cur | cur <- [1..num-1], num `mod` cur == 0]  -- Gets all of the factors using a list comprehension

find :: Eq a => a -> [(a,b)] -> [b] -- Finds each secondary element a key element is paired with from a list
find key passed_l = [snd val | val <- passed_l, (fst val) == key] -- A list comprehensions 

positions :: Eq a => a -> [a] -> [Int] -- Finds all of the indicies that the passed val is in
positions val passed_l = find val (zip passed_l [n | n <- [0..(length passed_l-1)]])  -- 

scalarproduct :: [Int] -> [Int] -> Int -- Performs the scalar sum of two lists
scalarproduct xs ys = sum[x * y | (x, y) <- zip xs ys] -- Calculates it using sum, a list comprehension, and zip

main :: IO () -- Main function that handles the demo of the various functions
main = do -- The main starts here:

     -- This code is for demo purposes, the bulk of the "useful" code is the functions above this main function.

     putStrLn "\nPart 1a: Replicate'" -- Header for section
     let part_1a_example_1 = replicate' 3 True  -- Calls the given function
     putStrLn "> replicate' 3 True" -- Prints what command was run to the console
     print part_1a_example_1 -- Prints the result of the command

     let part_1a_test_case = replicate' 5 "test code" -- Runs the given test-case function
     putStrLn "> replicate' 5 \"test code\"" -- Prints what function was run
     print part_1a_test_case -- Prints the results of the call
     putStrLn "\n\n" -- New lines for formatting

     putStrLn "Part 1b: Perfects" -- Prints the header for the second function of part 1
     let part_1b_example_1 = perfects 500 -- Calls the given function
     putStrLn "> perfects 500" -- Prints the function that we ran
     print part_1b_example_1 -- Prints the results

     let part_1b_test_case = perfects 9000 -- Calculates the test case
     putStrLn "> perfects 9000" -- Prints what was called
     print part_1b_test_case -- Prints results
     putStrLn "\n\n" -- New lines for formatting

     putStrLn "Part 1c: Find" -- Prints the header for the third function of part 1
     let part_1c_example_1 = find 'b' [('a',1),('b',2),('c',3),('b',4)] -- Calls the given function
     putStrLn "> find 'b' [('a',1),('b',2),('c',3),('b',4)]" -- Prints what we ran
     print part_1c_example_1 -- Prints the results

     let part_1c_test_case = find 'c' [('a',1),('b',2),('c',3),('b',4),('c',25)] -- Run the test case
     putStrLn "> find 'c' [('a',1),('b',2),('c',3),('b',4),('c',25)]" -- Print what we ran
     print part_1c_test_case -- Prints the result
     putStrLn "\n\n" -- Prints new lines for nice formatting

     putStrLn "Part 1d: Positions" -- Prints the header for the fourth function of part 1
     let part_1d_example_1 = positions 0 [1,0,0,1,0,1,1,0] -- Runs the example
     putStrLn "> positions 0 [1,0,0,1,0,1,1,0]" -- Prints what example we're doing
     print part_1d_example_1 -- Prints the result

     let part_1d_test_case = positions 1 [1,0,0,1,0,1,1,0] -- Call the test case
     putStrLn "> positions 1 [1,0,0,1,0,1,1,0]" -- Print what we ran
     print part_1d_test_case -- Print the result
     putStrLn "\n\n" -- Print newlines for nice formatting

     putStrLn "Part 1e: Scalar Product" -- Prints the header for the fifth function of part 1
     let part_1e_example_1 = scalarproduct [1,2,3] [4,5,6] -- Call the function in the example
     putStrLn "> scalarproduct [1,2,3] [4,5,6]" -- Print what we called
     print part_1e_example_1 -- Print the results

     let part_1e_test_case = scalarproduct [-1,2,3] [-4,-5,6] -- Call the function from the test case
     putStrLn "> scalarproduct [-1,2,3] [-4,-5,6]" -- Print what we called
     print part_1e_test_case -- Print the result
     putStrLn "\n\n" -- Print new lines for formatting


