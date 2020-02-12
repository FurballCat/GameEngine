# Compile shaders
cd -- "$(dirname "$0")"

echo "";
echo "## Start shader compilation ##";
echo "";

mkdir -p compiled

for f in *.vert;
do
    ../thirdparty/vulkansdk/macOS/bin/glslangValidator -V $f -o compiled/$(basename -s .vert $f).spv
done;

for f in *.frag;
do
    ../thirdparty/vulkansdk/macOS/bin/glslangValidator -V $f -o compiled/$(basename -s .frag $f).spv
done;

echo "";
echo "## compilation finished ##";
echo "";
read -p "Press enter to continue"
