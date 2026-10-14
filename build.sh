#!/usr/bin/env bash

set -eu
set -o pipefail

echo "Building and testing with .NET CLI..."
dotnet restore FsGettextUtils.sln
dotnet build FsGettextUtils.sln --configuration Release --no-restore
dotnet test FsGettextUtils.sln --configuration Release --no-build

echo "Build and test completed successfully."
