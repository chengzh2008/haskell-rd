module LogicProgram where


{-
https://wiki.haskell.org/Logic_programming_example


proglem:

There is a tribe where all the Male members speak true statements and Female members never speak two true statements in a row, nor two untrue statements in a row. (I apologize for the obvious misogyny).

A researcher comes across a mother, a father, and their child. The mother and father speak English but the child does not. However, the researcher asks the child "Are you a boy?". The child responds but the researcher doesn't understand the response and turns to the parents for a translation.

Parent 1: "The child said 'I am a boy.'"
Parent 2: "The child is a girl. The child lied."
What is the sex of parent 1, parent 2, the child, and what sex did the child say they were?


We have two axioms:

A Male does not lie.
A Female will never tell two lies or two truths in a row.
And we have three statements (i.e. logical expressions) in the puzzle:

The child said a single statement, in which they declared their sex.
Parent 1 said a single statement: "The child said 'I am a a boy'"
Parent 2 said two statements: "The child is a girl. The child lied."

-}

import Control.Monad (guard)

data Gender = Male | Female deriving (Eq, Show)

data PuzzleAnswer = PuzzleAnswer { parent1 :: Gender
                                 , parent2 :: Gender
                                 , child :: Gender
                                 , child_desc :: Gender
                                 }

instance Show PuzzleAnswer where
  show pa = concat [ "Parent1 is  ", (show $ parent1 pa), "\n"
                   , "Parent2 is ", (show $ parent2 pa), "\n"
                   , "The child is ", (show $ child pa), "\n"
                   , "The child said they were ", (show $ child_desc pa), "\n"
                   ]

isChildStatementValid :: Gender -> Gender -> Bool
isChildStatementValid Male Female = False
isChildStatementValid _ _ = True


-- parent 1 test: taking parent1 gender and child_desc gender
isParent1StatementValid :: Gender -> Gender -> Bool
isParent1StatementValid Male Female = False
isParent1StatementValid _ _ = True

-- parent 2 test: taking parent2_gender, child_gender, child_desc_gender
isParent2Valid :: Gender -> Gender -> Gender -> Bool
isParent2Valid Female _ Female = True
isParent2Valid Male Female Male = True
isParent2Valid _ _ _ = False

allGender = [Male, Female]

solvePuzzle :: (Gender -> Gender -> Bool) -> [PuzzleAnswer]
solvePuzzle predicate = do
  parent1 <- allGender
  parent2 <- allGender
  child <- allGender
  child_desc <- allGender
  guard $ predicate parent1 parent2
  guard $ isChildStatementValid child child_desc
  guard $ isParent1StatementValid parent1 child_desc
  guard $ isParent2Valid parent2 child child_desc
  return $ PuzzleAnswer parent1 parent2 child child_desc

main = do
  mapM_ print $ solvePuzzle (/=)
