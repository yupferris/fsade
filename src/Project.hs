module Project
       ( Project
       , createDefaultProject
       , serializeProjectFile
       ) where

import Text.XML.Light

data Project = Project { projectName :: String
                       } deriving (Show)

createDefaultProject name =
  Project { projectName = name }

serializeProject project =
  ppTopElement $ Element { elName = unqual "Project"
                         , elAttribs = []
                         , elContent = []
                         , elLine = Nothing
                         }

serializeProjectFile filePath = writeFile filePath . serializeProject
