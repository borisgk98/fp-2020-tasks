module Trees where

import Part2.Types

-- (1 2 (3 4 (5 6 nil)))
--   2
--  / \
-- 1   4
--    / \
--   3   6
--      /
--     5
tree1 :: Tree Int
tree1 = Tree (Just $ Tree Nothing 1 Nothing)
             2
             (Just $ Tree (Just $ Tree Nothing 3 Nothing)
                          4
                          (Just $ Tree (Just $ Tree Nothing 5 Nothing)
                                        6
                                        Nothing))

-- ((1 2 3) 4 (5 6 nil))
--      4
--     / \
--    /   \
--   2     6
--  / \   /
-- 1   3 5
tree2 :: Tree Int
tree2 = Tree (Just $ Tree (Just $ Tree Nothing 1 Nothing)
                          2
                          (Just $ Tree Nothing 3 Nothing))
             4
             (Just $ Tree (Just $ Tree Nothing 5 Nothing)
                          6
                          Nothing)

-- (3 1 2)
--   1
--  / \
-- 3   2
tree3 :: Tree Int
tree3 = Tree (Just $ Tree Nothing 3 Nothing) 1 (Just $ Tree Nothing 2 Nothing)

tree4 :: Tree ()
tree4 = Tree (Just $ Tree Nothing () Nothing)
             ()
             (Just $ Tree Nothing () Nothing)

-- ((x x nil) x nil)
--     X
--    /
--   X
--  /
-- X
tree5 :: Tree ()
tree5 = Tree (Just $ Tree (Just $ Tree Nothing () Nothing)
                          ()
                          Nothing)
             ()
             Nothing