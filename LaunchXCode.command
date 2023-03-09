#!/bin/bash

# Create the intermediate and bin directories if they don't exist
mkdir -p intermediate bin

# Navigate to the intermediate directory
cd intermediate

# Call cmake with the Xcode generator
cmake -G Xcode ..

# Navigate back to the original directory
cd ..

# Open XCode
open intermediate/GameEngine.xcodeproj
