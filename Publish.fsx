#load "Common.fsx"
#r @"System.IO.Compression.dll"
#r @"System.IO.Compression.FileSystem.dll"

open System
open System.IO
open System.IO.Compression

open Common

let inAppveyor, buildVersion =
    let s = Environment.GetEnvironmentVariable("APPVEYOR_BUILD_VERSION")
    if isNull s then
        false, "0.0.0"
    else
        true, s

let pushArtifact path =
    if inAppveyor then
        cmd "appveyor" ("PushArtifact " + path)

if Directory.Exists("Publish") then
    Directory.Delete("Publish", true)
Directory.CreateDirectory(@"Publish") |> ignore
cmd "msbuild" @"/p:Configuration=Release Malmacs.sln"


copyAllFiles @"Malmacs\bin\Release" "Publish"
copyAllFiles @"Malmacs\Mal" @"Publish\Mal"
copyFile @"Malmacs\Malmacs.config.Release.json" @"Publish\Malmacs.config.json"
let net45ZipFileName = sprintf "Malmacs-%s.zip" buildVersion
if File.Exists(net45ZipFileName) then
    File.Delete(net45ZipFileName)
ZipFile.CreateFromDirectory("Publish", net45ZipFileName)
pushArtifact net45ZipFileName
