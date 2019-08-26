# Generate Voodoo projects for XCode
cd -- "$(dirname "$0")"
python3.6 projectBuilder/main.py
open intermediate/projects/voodooEngine.xcworkspace