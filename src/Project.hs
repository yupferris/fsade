module Project
       ( Project
       , serializeProject
       ) where

data Project = Project { projectName :: String
                       } deriving (Show)

serializeProject _ = do return ()
