# Compile shaders
cd -- "$(dirname "$0")"

echo "";
echo "## Start shader compilation ##";
echo "";

for f in *.vert;
do
    ../thirdparty/vulkansdk/macOS/bin/glslangValidator -V $f
done;

for f in *.frag;
do
    ../thirdparty/vulkansdk/macOS/bin/glslangValidator -V $f
done;

echo "";
echo "## compilation finished ##";
echo "";
read -p "Press enter to continue"
