# Generate Voodoo projects for XCode
cd -- "$(dirname "$0")"
python3 projectBuilder/main.py
open intermediate/projects/FurballCatGameEngine.xcworkspace