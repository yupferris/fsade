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

serializeProject = ppTopElement . serializeProjectXml

serializeProjectXml project =
  unode "Project"
    ([ attr "ToolsVersion" "12.0"
     , attr "DefaultTargets" "Build"
     , attr "xmlns" "http://schemas.microsoft.com/developer/msbuild/2003"
     ],
     [ unode "Import"
       [ attr "Project" "$(MSBuildExtensionsPath)\\$(MSBuildToolsVersion)\\Microsoft.Common.props"
       , attr "Condition" "Exists('$(MSBuildExtensionsPath)\\$(MSBuildToolsVersion)\\Microsoft.Common.props')"
       ]
     , unode "PropertyGroup"
       [ unode "Configuration"
         (attr "Condition" " '$(Configuration)' == '' ",
          "Debug")
       , unode "Platform"
         (attr "Condition" " '$(Platform)' == '' ",
          "AnyCPU")
       , unode "SchemaVersion" "2.0"
       ]
     ])

attr name value = Attr (unqual name) value

serializeProjectFile filePath = writeFile filePath . serializeProject
