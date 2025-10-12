{-|  * CSC324H5 Fall 2025: Week 5 Lab *
Module:        W05lab
Description:   Week 5 Lab: Type Systems
Copyright: (c) University of Toronto Mississsauga
               CSC324 Principles of Programming Languages, Fall 2025
-}
-- This lists what this module exports. Don't change this!
module W05lab
  (
    postsWithMinimumComments,
    removePhoneNumbers,
    retrieveAllCommentedPosts,
    followingBack,
    Account(..),
    Post(..),
  )
where
-- Remember that you may not add any additional imports
import Test.QuickCheck (Property, quickCheck, (==>))
data Account = Account String String (Maybe String) [Account] [Post] deriving (Show, Eq)   -- username, email, phone number (if provided), accounts that this user is following, posts
data Post = Post Int (Either String Int) deriving (Show, Eq)                               -- postidentifier, number of comments on the post (either Left "Disabled" or Right (number of comments))
-------------------------------------------------------------------------------
-- * Note on Either Type
-- In class, you may have learned about the Maybe type constructor! In this lab, we will be working with
-- the Either type constructor as well (as seen in the Post type). You can read more about how the Either
-- type constructor works in this link.
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Either.html
-- | Return all posts with comments enabled and has more comments than the argument provided
postsWithMinimumComments :: [Post] -> Int -> [Post]
postsWithMinimumComments posts minComments = filter hasEnoughComments posts
  where
    hasEnoughComments (Post _ (Right numComments)) = numComments > minComments
    hasEnoughComments (Post _ (Left _)) = False

-- | Remove all phone numbers for accounts that have one provided
removePhoneNumbers :: [Account] -> [Account]
removePhoneNumbers accounts = map removePhone accounts
  where
    removePhone (Account username email _ following posts) = 
      Account username email Nothing following posts

-- | Given an account and a list of accounts, return the list of accounts that are following this account back
followingBack :: Account -> [Account] -> [Account]
followingBack account accounts = filter isFollowingBack accounts
  where
    isFollowingBack otherAccount = account `elem` getFollowing otherAccount
    getFollowing (Account _ _ _ following _) = following

-- | Go through all accounts and return all posts (with commenting enabled) across all accounts, in one list
retrieveAllCommentedPosts :: [Account] -> [Post]
retrieveAllCommentedPosts accounts = concatMap getCommentedPosts accounts
  where
    getCommentedPosts (Account _ _ _ _ posts) = filter hasCommentsEnabled posts
    hasCommentsEnabled (Post _ (Right _)) = True
    hasCommentsEnabled (Post _ (Left _)) = False

-------------------------------------------------------------------------------
-- * Main function (for testing purposes only)
-------------------------------------------------------------------------------
prop_postsWithMinimumComments :: Bool
prop_postsWithMinimumComments = [Post 1 (Right 124)] == postsWithMinimumComments [Post 1 (Right 124), Post 2 (Left "Disabled")] 123
-- This main function is executed when you compile and run this Haskell file.
-- It runs the QuickCheck tests; we'll talk about "do" notation much later in
-- the course, but for now if you want to add your own tests, just define them
-- above, and add a new `quickCheck` line below.
main :: IO ()
main = do
  quickCheck prop_postsWithMinimumComments
    -- TODO: add other tests