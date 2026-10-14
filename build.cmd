@echo off
setlocal

REM Build and test the solution using .NET CLI
dotnet restore FsGettextUtils.sln
dotnet build FsGettextUtils.sln --configuration Release --no-restore
dotnet test FsGettextUtils.sln --configuration Release --no-build

echo Build and test completed successfully.
