module Project
       ( Project
       , projectName
       , guid
       , createDefaultProject
       , serializeProjectFile
       ) where

import Data.UUID
import Data.UUID.V4
import Text.XML.Light

data Project = Project { projectName :: String
                       , guid :: UUID
                       } deriving (Show)

createDefaultProject name = do
  guid <- nextRandom
  return Project { projectName = name, guid = guid }

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
       [ unode "Configuration" (attr "Condition" " '$(Configuration)' == '' ", "Debug")
       , unode "Platform" (attr "Condition" " '$(Platform)' == '' ", "AnyCPU")
       , unode "SchemaVersion" "2.0"
       , unode "ProjectGuid" $ toString $ guid project
       , unode "OutputType" "Exe"
       , unode "RootNamespace" $ projectName project
       , unode "AssemblyName" $ projectName project
       , unode "TargetFrameworkVersion" "v4.5"
       , unode "AutoGenerateBindingRedirects" "true"
       , unode "TargetFSharpCoreVersion" "4.3.1.0"
       , unode "Name" $ projectName project
       ]
     , unode "PropertyGroup"
       ([ attr "Condition" " '$(Configuration)|$(Platform)' == 'Debug|AnyCPU' " ],
        [ unode "DebugSymbols" "true"
        , unode "DebugType" "full"
        , unode "Optimize" "false"
        , unode "Tailcalls" "false"
        , unode "OutputPath" "bin\\Debug\\"
        , unode "DefineConstants" "DEBUG;TRACE"
        , unode "WarningLevel" "3"
        , unode "PlatformTarget" "AnyCPU"
        , unode "DocumentationFile" $ "bin\\Debug\\" ++ (projectName project) ++ ".XML"
        , unode "Prefer32Bit" "true"
        ])
     , unode "PropertyGroup"
       ([ attr "Condition" " '$(Configuration)|$(Platform)' == 'Release|AnyCPU' " ],
        [ unode "DebugType" "pdbonly"
        , unode "Optimize" "true"
        , unode "Tailcalls" "true"
        , unode "OutputPath" "bin\\Release\\"
        , unode "DefineConstants" "TRACE"
        , unode "WarningLevel" "3"
        , unode "PlatformTarget" "AnyCPU"
        , unode "DocumentationFile" $ "bin\\Release\\" ++ (projectName project) ++ ".XML"
        , unode "Prefer32Bit" "true"
        ])
     , unode "ItemGroup"
       [ reference "mscorlib" True
       , reference "FSharp.Core, Version=$(TargetFSharpCoreVersion), Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a" False
       , reference "System" True
       , reference "System.Core" True
       , reference "System.Numerics" True
       ]
     , unode "ItemGroup"
       [ unode "Compile" $ include "Program.fs"
       , unode "None" $ include "App.config"
       ]
     , unode "PropertyGroup"
       (unode "MinimumVisualStudioVersion" (attr "Condition" "'$(MinimumVisualStudioVersion)' == ''", "11"))
     , unode "Choose"
       [ unode "When"
         (attr "Condition" "'$(VisualStudioVersion)' == '11.0'",
          unode "PropertyGroup"
          (attr "Condition" "Exists('$(MSBuildExtensionsPath32)\\..\\Microsoft SDKs\\F#\\3.0\\Framework\\v4.0\\Microsoft.FSharp.Targets')",
           unode "FSharpTargetsPath" "$(MSBuildExtensionsPath32)\\..\\Microsoft SDKs\\F#\\3.0\\Framework\\v4.0\\Microsoft.FSharp.Targets"))
       , unode "Otherwise"
         $ unode "PropertyGroup"
         (attr "Condition" "Exists('$(MSBuildExtensionsPath32)\\Microsoft\\VisualStudio\\v$(VisualStudioVersion)\\FSharp\\Microsoft.FSharp.Targets')",
          unode "FSharpTargetsPath" "$(MSBuildExtensionsPath32)\\Microsoft\\VisualStudio\\v$(VisualStudioVersion)\\FSharp\\Microsoft.FSharp.Targets")
       ]
     , unode "Import" $ attr "Project" "$(FSharpTargetsPath)"
     ])

attr name value = Attr (unqual name) value
reference name isPublic = case isPublic of
                           True -> unode "Reference" $ include name
                           False -> unode "Reference" (include name, unode "Private" "true")
include = attr "Include"

serializeProjectFile filePath = writeFile filePath . serializeProject
