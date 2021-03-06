#!/bin/bash

set -e

artifacts=(perfect-flow)

arch=${1:-win64}

distDir="dist/$arch"

# Move to the project's root directory
dir="."
while [ ! -f "$dir/package.yaml" ]; do
    dir="$dir/.."
done

pushd $dir

EXE_SUFFIX=''

case arch in
    win64)
        libs=(SDL2.dll SDL2_ttf.dll zlib1.dll)
        EXE_SUFFIX=.exe
        ;;
esac

mkdir -p "$distDir"

echo "Checking required libraries..."

pushd "$distDir"
missingLibs=()
for lib in "${libs[@]}"; do
    if [ ! -f "$lib" ]; then
        missingLibs+=("$lib")
    fi
done
popd

if [ "${#missingLibs[@]}" -eq 0 ]; then
    echo "All required libraries present"
else
    echo "Missing libraries: ${missingLibs[@]}" >&2
    echo "Please add them to $distDir and try again." >&2
    exit 1
fi

echo "Copying license files..."
cp -f licenses/* "$distDir"

echo "Performing build"
stack build

echo "Checking build artifacts..."
for artifact in "${artifacts[@]}"; do
    art="${artifact}${EXE_SUFFIX}"
    if ! stack exec which "$art"; then
        echo "Missing build artifact: $art" >&2
        missing=true
    fi
done
if [ "$missing" = "true" ]; then
    echo "Aborting" >&2
    exit 1
fi
echo "Copying build artifacts to $distDir"
for artifact in "${artifacts[@]}"; do
    art="${artifact}${EXE_SUFFIX}"
    apath=$(stack exec which "$art")
    cp -f "$apath" "$distDir"
done

echo "Copying data files to $distDir"
cp -rf data "$distDir"

popd