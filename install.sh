rm -rf build
fpm build --compiler=gfortran --flag="-Wall -Wextra -Wfatal-errors -pedantic -fcheck=all -O0 -g" --verbose
